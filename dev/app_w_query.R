library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(stringr)
library(readr)

source("get_all_incidents.R")

theme_set(theme_minimal())

# Load violation classifications
violations <- read_csv("data/violations_major_minor.csv")

# Load incidents
df_feb_2020 <- read_csv("data/incidents_to_feb_2020.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  theme = "bpd_covid19_app.css",
  
  # App title ----
  titlePanel("Boston Police Incidents : Coronavirus Tracker"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h4("Explore Boston PD Incidents"),
      p(paste("View plots in the different tabs to track the behavior of the",
              "Boston Police Department after Governor Baker's",
              "declaration of a State of Emergency on March 10, 2020.")),
      
      h4("Data Source"),
      HTML(paste("We sourced these police incident reports from",
                 "<a href='https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system'>Analyze Boston</a>.",
                 "The data posted there goes back to June 2015, and is updated daily."))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Year-to-Year Comparison", plotOutput("year_to_year_plot")),
        tabPanel("Major & Minor Incidents Comparison", plotOutput("major_minor_plot")),
        tabPanel("Incidents Over Time", plotOutput("incidents_v_time_plot"))
        )
    )
  ),
  
  hr(),
  img(src="OneLineLogo_RGB_Massachusetts.png", width="300px", 
      style="opacity: 0.5; display: block; margin-left: auto; margin-right: auto;"),
  p("Please contact lchambers@aclum.org with questions.", align="center", style="opacity: 0.6;")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  loaded_df_all <- reactive({
    showModal(modalDialog("Downloading latest incident records from data.boston.gov...\n(This might take a few seconds)", footer=NULL))
    
    # df <- get_all_incident_data()
    
    df_new <- get_incident_data_since_feb()
    
    df <- rbind(df_feb_2020, df_new) %>%
      mutate(OFFENSE_CODE = as.numeric(OFFENSE_CODE)) %>%
      merge(violations %>% select(CODE, Lauren_says_minor), 
            by.x="OFFENSE_CODE", by.y="CODE")
          
    removeModal()
    
    df
  })
  
  calc_last_date_to_plot <- reactive({
    date(max(loaded_df_all()$OCCURRED_ON_DATE) - days(1))
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2019 v. 2020
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$year_to_year_plot <- renderPlot({
    # if (is.null(addressesDF()))
    #   return(ward_plot)
    
    df_all <- loaded_df_all()
    
    last_date_to_plot <- calc_last_date_to_plot()
    first_date_to_plot <- last_date_to_plot - months(2)
    
    df_last_year <- df_all %>%
      mutate(date_to_plot = OCCURRED_ON_DATE %>% as.character %>%
               str_replace("^\\d{4}","2020") %>% as_date) %>%
      filter(YEAR == '2019', 
             date_to_plot <= last_date_to_plot, 
             date_to_plot >= first_date_to_plot) %>%
      group_by(date_to_plot) %>%
      summarize(n = n())
    
    last_date_value_2020 <- df_all %>%
      filter(date(OCCURRED_ON_DATE) == last_date_to_plot) %>%
      count() %>%
      pull()
    
    last_date_value_2019 <- df_last_year %>%
      filter(date(date_to_plot) == last_date_to_plot) %>%
      pull(n)
    
    df_all %>%
      filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
      mutate(date_to_plot = date(OCCURRED_ON_DATE)) %>%
      group_by(date_to_plot) %>%
      summarize(n = n()) %>%
    ggplot(aes(x = date_to_plot, y = n)) +
      geom_vline(aes(xintercept = ymd(20200310)),
                 linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
      geom_line(data=df_last_year, size=1.3, alpha=.4) +
      geom_path(aes(group = 1, color = date_to_plot >= ymd(20200310)), 
                size=1.3, show.legend = FALSE) +
      geom_point(aes(color = date_to_plot >= ymd(20200310)), size=.6, show.legend = FALSE) +
      ylim(0, 350) +
      labs(x = "", y = "Daily Number of Incidents", color="") +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size=18),
            plot.margin = unit(c(1,3,1,1), "lines")) +
      scale_color_manual(values=c("black", "#fbb416")) +
      annotate("text", x = ymd(20200310) - 3, y = 100, angle=90, hjust=0.5,
               family="GT America", lineheight=0.9, size = 5, color="#fbb416", 
               label = "State of Emergency\ndeclared in MA") +
      annotate("text", x = last_date_to_plot, y = last_date_value_2020, hjust=-.2,
               family="GT America", size = 5, color="#fbb416", fontface="bold",
               label = "2020") +
      annotate("text", x = last_date_to_plot, y = last_date_value_2019, hjust=-.2,
               family="GT America", size = 5, alpha=0.5, fontface="bold",
               label = "2019") +
      scale_x_date(date_labels = "%b %e ",
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Major & Minor Incidents v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$major_minor_plot <- renderPlot({
    # if (is.null(addressesDF()))
    #   return(ward_plot)
    
    df_all <- loaded_df_all()
    
    last_date_to_plot <- calc_last_date_to_plot()
    
    last_date_value_major <- df_all %>%
      filter(date(OCCURRED_ON_DATE) == last_date_to_plot,
             Lauren_says_minor == FALSE) %>%
      count() %>%
      pull()
    
    last_date_value_minor <- df_all %>%
      filter(date(OCCURRED_ON_DATE) == last_date_to_plot,
             Lauren_says_minor == TRUE) %>%
      count() %>%
      pull()
    
    df_all %>%
      filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
      group_by(date = date(OCCURRED_ON_DATE), Lauren_says_minor) %>%
      summarize(n = n()) %>%
      ggplot(aes(x=date, y = n, alpha = date >= ymd(20200310))) +
      geom_vline(aes(xintercept=ymd(20200310)), 
                 linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
      geom_path(aes(color = Lauren_says_minor, group=Lauren_says_minor), 
                size=1.3, show.legend = FALSE) +
      ylim(0, 200) +
      labs(x = "", y = "Number of Incidents", color="") +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size=18),
            plot.margin = unit(c(1,5,1,1), "lines")) +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      scale_color_manual(values=c("#ef404d", "#0055aa")) +
      scale_alpha_manual(values=c(0.3, 1)) +
      annotate("text", x=ymd(20200310)-2.5, y = 60, angle=90, hjust=0.5,
               color="#fbb416", family="GT America", lineheight=0.9, size=4,
               label = "State of Emergency\ndeclared in MA") +
      annotate("text", x = last_date_to_plot, y = last_date_value_major, hjust=-.1, vjust = 0.5,
               family="GT America", size = 5, color="#ef404d", fontface="bold",
               label = "Major\nincidents") +
      annotate("text", x = last_date_to_plot, y = last_date_value_minor, hjust=-.1, vjust = -0.5,
               family="GT America", size = 5, fontface="bold", color="#0055aa",
               label = "Minor\nincidents") +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Incidents v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$incidents_v_time_plot <- renderPlot({
    # if (is.null(addressesDF()))
    #   return(ward_plot)
    
    df_all <- loaded_df_all()
    
    last_date_to_plot <- calc_last_date_to_plot()
    
    df_all %>%
      filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
      group_by(date = date(OCCURRED_ON_DATE)) %>%
      summarize(n = n()) %>%
    ggplot(aes(x=date, y = n, color = date >= ymd(20200310))) +
      geom_vline(aes(xintercept=ymd(20200310)), 
                 linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
      geom_path(aes(group = 1), size=1.3, show.legend = FALSE) +
      geom_point(size=.6, show.legend = FALSE) +
      ylim(0, 350) +
      labs(x = "", y = "Daily Number of Incidents", color="") +
      theme(plot.title= element_text(family="GT America", face='bold'),
            text = element_text(family="GT America", size=18)) +
      scale_color_manual(values=c("black", "#fbb416")) +
      annotate("text", x=ymd(20200310)-3, y = 100, angle=90, hjust=0.5,
               color="#fbb416", family="GT America", lineheight=0.9, size=5,
               label = "State of Emergency\ndeclared in MA") +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot))
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)

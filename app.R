library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(stringr)
library(readr)
library(shinycssloaders)
library(showtext)

theme_set(theme_minimal())

# Load violation classifications
violations <- read_csv("data/violations_major_minor.csv")
group_choices <- c("--", "All", violations %>% pull(incident_group) %>% unique() %>% sort())
group_choices <- c(group_choices[group_choices != "Other"], "Other")

# Define modal UI explaining incident types
modal_text <- list()
i <- 1
for (grp in group_choices[3:28]) {
  modal_text[[i]] <- h3(grp)
  i <- i + 1
  
  incidents_in_grp <- violations %>%
    filter(incident_group == grp) %>%
    mutate(code_desc = paste(OFFENSE_CODE, desc, sep=" - ")) %>%
    arrange(desc) %>%
    pull(code_desc)
  
  for (incident in incidents_in_grp) {
    modal_text[[i]] <- p(incident)
    i <- i + 1
  }
}

# Load all incidents
df_all <- read_rds("data/all_bpd_incidents.rds") %>%
  mutate(OFFENSE_CODE = as.numeric(OFFENSE_CODE)) %>%
  merge(violations, 
        by="OFFENSE_CODE")

# Load time of last query
last_query_time <- read_rds("data/query_log.rds") %>%
  tail(1) %>%
  pull(query_time) %>%
  with_tz(tzone="America/New_York") %>%
  format(format="%A %B %e, %Y at %I:%M %p %Z")

last_date_to_plot <- date(max(df_all$OCCURRED_ON_DATE) - days(1))

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = "bpd_covid19_app.css",
  
  # Add development warning and link to shinyapps.io page
  div(id="dev-warning",
    wellPanel(
      icon('exclamation-triangle'),
      p(paste("This application is under active development.",
            "For a working prototype, please visit:")),
      a("https://laurenmarietta.shinyapps.io/bpd_covid19/",
        href="https://laurenmarietta.shinyapps.io/bpd_covid19/")
      )
  ),
  
  # App title ----
  titlePanel("Boston Police Incidents : Coronavirus Tracker"),
  
  div(
    navlistPanel(widths = c(3, 9),
      tabPanel("About", 
               h4("Explore Boston PD Incidents"),
               p(paste("View plots in the different tabs to track the behavior of the",
                       "Boston Police Department after Governor Baker's",
                       "declaration of a State of Emergency on March 10, 2020.")),
               
               h4("Data Source"),
               HTML(paste("We sourced these police incident reports from",
                          "<a href='https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system'>Analyze Boston</a>.",
                          "The data posted there goes back to June 2015, and is updated daily."))
               ),
      
      # tabPanel("Year-to-Year Comparison", 
      #          withSpinner(
      #            plotOutput("year_to_year_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Year-to-Year Comparison", 
                 div(
                   style = "position:relative",
                   withSpinner(plotOutput("year_to_year_plot", 
                              hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                              type=4, color="#b5b5b5", size=0.5),
                   uiOutput("hover_info")
                 )),
      
      tabPanel("Incidents by Type",
               wellPanel(
                 p("Select up to three kinds of incidents to plot versus time.", 
                   actionLink("modal_incidents", label = NULL, icon=icon("info-circle"))),
                 splitLayout(
                   selectInput("select_incidentgroup1", label = NULL, choices = group_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_incidentgroup2", label = NULL, choices = group_choices,
                               selected = "Motor Vehicle", multiple=FALSE),
                   selectInput("select_incidentgroup3", label = NULL, choices = group_choices,
                               selected = "Investigations", multiple=FALSE)
                 )),
               withSpinner(plotOutput("incidents_group_plot"), type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("Major & Minor Incidents Comparison", 
               withSpinner(plotOutput("major_minor_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Incidents Over Time", 
               withSpinner(plotOutput("incidents_v_time_plot"), type=4, color="#b5b5b5", size=0.5))
      
      ),
    
    em(paste("Latest query:", last_query_time), align="right", style="opacity: 0.6;")
    ),
  
  br(),
  hr(),
  img(src="OneLineLogo_RGB_Massachusetts.png", width="300px", 
      style="opacity: 0.5; display: block; margin-left: auto; margin-right: auto;"),
  p("Please contact lchambers@aclum.org with questions.", align="center", style="opacity: 0.6;")
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # Load ggplot-friendly font using show_text
  font_add("gtam", "GT-America-Standard-Regular.ttf",
           bold = "GT-America-Standard-Bold.ttf")
  showtext_auto()
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 2019 v. 2020
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
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
  
  df_2019_v_2020 <- df_all %>%
    filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
    mutate(date_to_plot = date(OCCURRED_ON_DATE)) %>%
    group_by(date_to_plot) %>%
    summarize(n = n())
  
  output$year_to_year_plot <- renderPlot({
    
    # first_date_to_plot <- last_date_to_plot - months(2)
    # 
    # df_last_year <- df_all %>%
    #   mutate(date_to_plot = OCCURRED_ON_DATE %>% as.character %>%
    #            str_replace("^\\d{4}","2020") %>% as_date) %>%
    #   filter(YEAR == '2019', 
    #          date_to_plot <= last_date_to_plot, 
    #          date_to_plot >= first_date_to_plot) %>%
    #   group_by(date_to_plot) %>%
    #   summarize(n = n())
    # 
    # last_date_value_2020 <- df_all %>%
    #   filter(date(OCCURRED_ON_DATE) == last_date_to_plot) %>%
    #   count() %>%
    #   pull()
    # 
    # last_date_value_2019 <- df_last_year %>%
    #   filter(date(date_to_plot) == last_date_to_plot) %>%
    #   pull(n)
    # 
    # df_2019_v_2020 <- df_all %>%
    #   filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
    #   mutate(date_to_plot = date(OCCURRED_ON_DATE)) %>%
    #   group_by(date_to_plot) %>%
    #   summarize(n = n())
    
    df_2019_v_2020 %>%
    ggplot(aes(x = date_to_plot, y = n)) +
      geom_vline(aes(xintercept = ymd(20200310)),
                 linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
      geom_line(data=df_last_year, size=1.3, alpha=.4) +
      geom_path(aes(group = 1, color = date_to_plot >= ymd(20200310)), 
                size=1.3, show.legend = FALSE) +
      geom_point(aes(color = date_to_plot >= ymd(20200310)), size=.6, show.legend = FALSE) +
      ylim(0, 350) +
      labs(x = "", y = "Daily Number of Incidents", color="") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=18),
            plot.margin = unit(c(1,3,1,1), "lines")) +
      scale_color_manual(values=c("black", "#fbb416")) +
      annotate("text", x = ymd(20200310) - 2.5, y = 100, angle=90, hjust=0.5,
               family="gtam", size = 4, color="#fbb416", 
               label = "State of Emergency\ndeclared in MA") +
      annotate("text", x = last_date_to_plot, y = last_date_value_2020, hjust=-.2,
               family="gtam", size = 5, color="#fbb416", fontface="bold",
               label = "2020") +
      annotate("text", x = last_date_to_plot, y = last_date_value_2019, hjust=-.2,
               family="gtam", size = 5, alpha=0.5, fontface="bold",
               label = "2019") +
      scale_x_date(date_labels = "%b %e ",
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      coord_cartesian(clip = 'off')
    
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point_2020 <- nearPoints(df_2019_v_2020, 
                             hover, threshold = 10, maxpoints = 1, addDist = T)
    point_2019 <- nearPoints(df_last_year, 
                             hover, threshold = 10, maxpoints = 1, addDist = T)

    point <- point_2020
    is_2019 <- F
    if (nrow(point_2020) == 0) {
      point <- point_2019
      is_2019 <- T
      if (nrow(point_2019) == 0) {
        point <- NULL
      return(NULL)
      }
    }
      
    print(point)
    print(point$date_to_plot)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    date <- if_else(is_2019, 
                    paste0("2019", str_sub(point$date_to_plot, 5, 10)), 
                    as.character(point$date_to_plot))
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Date: </b>", date, "<br/>",
                    "<b> Number of Incidents: </b>", point$n, "<br/>")))
    )
  })
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Incidents by Group v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which incidents to plot
  inc_grps_to_plot <- reactive({
    c(input$select_incidentgroup1, 
      input$select_incidentgroup2,
      input$select_incidentgroup3)
  })
  
  observeEvent(input$modal_incidents, {
    showModal(modalDialog(renderUI(modal_text), easyClose = TRUE, footer = NULL))
  })
  
  # Plot
  output$incidents_group_plot <- renderPlot({
    
    all_df_all <- df_all %>%
      filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
      group_by(date = date(OCCURRED_ON_DATE)) %>%
      summarize(n = n()) %>%
      mutate(incident_group = "All")
    
    df_by_incidentgroup <- df_all %>%
      filter(OCCURRED_ON_DATE >= last_date_to_plot - months(2)) %>%
      group_by(date = date(OCCURRED_ON_DATE), incident_group) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      rbind(all_df_all)
    
    df_by_incidentgroup %>%
      filter(incident_group %in% inc_grps_to_plot()) %>%
    ggplot(aes(x=date, y = n, alpha = date >= ymd(20200310), 
               color=incident_group)) +
      geom_vline(aes(xintercept=ymd(20200310)), 
                 linetype="dashed", color = "#fbb416", size=1.2, alpha=0.5) +
      geom_path(aes(group=incident_group), 
                size=1.3, show.legend = T) +
      labs(x = "", y = "Number of Incidents", color="") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=18),
            plot.margin = unit(c(3,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"), legend.text = element_text(size=12)) +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      scale_color_manual(values=c("black", "#ef404d", "#0055aa")) +
      scale_alpha_manual(values=c(0.3, 1), guide="none") +
      annotate("text", x=ymd(20200310), y = Inf, hjust=0.5,
               color="#fbb416", family="gtam", size=4,
               label = "State of Emergency\ndeclared in MA\n\n") +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Major & Minor Incidents v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$major_minor_plot <- renderPlot({
    
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
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=18),
            plot.margin = unit(c(1,5,1,1), "lines")) +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      scale_color_manual(values=c("#ef404d", "#0055aa")) +
      scale_alpha_manual(values=c(0.3, 1)) +
      annotate("text", x=ymd(20200310)-2.5, y = 60, angle=90, hjust=0.5,
               color="#fbb416", family="gtam", size=4,
               label = "State of Emergency\ndeclared in MA") +
      annotate("text", x = last_date_to_plot, y = last_date_value_major, hjust=-.1, vjust = 0.5,
               family="gtam", size = 5, color="#ef404d", fontface="bold",
               label = "Major\nincidents") +
      annotate("text", x = last_date_to_plot, y = last_date_value_minor, hjust=-.1, vjust = -0.5,
               family="gtam", size = 5, fontface="bold", color="#0055aa",
               label = "Minor\nincidents") +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Incidents v. Time
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  output$incidents_v_time_plot <- renderPlot({
    
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
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size=18)) +
      scale_color_manual(values=c("black", "#fbb416")) +
      annotate("text", x=ymd(20200310)-2.5, y = 100, angle=90, hjust=0.5,
               color="#fbb416", family="gtam", size=4,
               label = "State of Emergency\ndeclared in MA") +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot))
    
  })
  
}

shinyApp(ui = ui, server = server)

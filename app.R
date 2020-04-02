library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(stringr)
library(readr)
library(shinycssloaders)
library(showtext)
library(leaflet)
library(leafsync)

# Set ggplot settings
theme_set(theme_minimal())
MA_label_fontsize = 8 #4
axis_label_fontsize = 30 #18
legend_label_fontsize = 20 #12
year_label_fontsize = 9 #5

# Define filenames and column data types
db_filename <- "data/all_bpd_incidents_cumulative.rds"
query_log_filename <- "data/query_log.rds"
violations_filename <- "data/violations_major_minor.csv"

# Load violation classifications
violations <- read_csv(violations_filename)
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
df_all <- read_rds(db_filename) %>%
  mutate(OFFENSE_CODE = as.numeric(OFFENSE_CODE)) %>%
  merge(violations, 
        by="OFFENSE_CODE", all.x=T)

# Load time of last query
last_query_time <- read_rds(query_log_filename) %>%
  tail(1) %>%
  pull(query_time) %>%
  with_tz(tzone="America/New_York") %>%
  format(format="%A %B %e, %Y at %I:%M %p %Z")

last_date_to_plot <- date(max(df_all$OCCURRED_ON_DATE) - days(1))

# Filter for mapping
df_to_map <- df_all %>%
  filter((OCCURRED_ON_DATE <= last_date_to_plot & OCCURRED_ON_DATE >= ymd(20200301)) | 
           (OCCURRED_ON_DATE <= last_date_to_plot - years(1) & OCCURRED_ON_DATE >= ymd(20190301)),
         !is.na(Long)) %>%
  mutate(Long=as.numeric(Long), Lat = as.numeric(Lat),
         labs = paste(format(with_tz(OCCURRED_ON_DATE, tzone="America/New_York"), 
                             format="%A %B %e, %Y at %I:%M %p"), 
                      desc, sep="<br/>")) %>%
  filter(Long < -69 & Long > -74, Lat < 43 & Lat > 41)

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
      
      tabPanel("Year-to-Year Comparison", 
               withSpinner(plotOutput("year_to_year_plot"), type=4, color="#b5b5b5", size=0.5)),
      
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
      
      tabPanel("Incident Locations", 
               splitLayout(align="center", h2("2019"), h2("2020")),
               p(align="center", paste0("Showing incident locations between March 10 and ", 
                                       format(last_date_to_plot, format="%B %e.\n")), br(),
                 em("Please note that only approximately 90% of all police incident reports include location coordinates.\n",
                    style="font-size:11px;")),
               withSpinner(uiOutput("synced_maps"), 
                           type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("Major & Minor Incidents Comparison", 
               withSpinner(plotOutput("major_minor_plot"), 
                           type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Incidents Over Time", 
               withSpinner(plotOutput("incidents_v_time_plot"), 
                           type=4, color="#b5b5b5", size=0.5))
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
  # 🗓 2019 v. 2020 🗓 
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  output$year_to_year_plot <- renderPlot({
    
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
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = axis_label_fontsize),
            plot.margin = unit(c(1,3,1,1), "lines")) +
      scale_color_manual(values=c("black", "#fbb416")) +
      annotate("text", x = ymd(20200310) - 2.5, y = 100, angle=90, hjust=0.5,
               family="gtam", size = MA_label_fontsize, color="#fbb416", 
               label = "State of Emergency\ndeclared in MA") +
      annotate("text", x = last_date_to_plot, y = last_date_value_2020, hjust=-.2,
               family="gtam", size = year_label_fontsize, color="#fbb416", fontface="bold",
               label = "2020") +
      annotate("text", x = last_date_to_plot, y = last_date_value_2019, hjust=-.2,
               family="gtam", size = year_label_fontsize, alpha=0.5, fontface="bold",
               label = "2019") +
      scale_x_date(date_labels = "%b %e ",
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🕰 Incidents by Group v. Time 🕰
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Determine which incidents to plot
  inc_grps_to_plot <- reactive({
    c(input$select_incidentgroup1, 
      input$select_incidentgroup2,
      input$select_incidentgroup3)
  })
  
  # Connect modal to incident info link
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
            text = element_text(family="gtam", size = axis_label_fontsize),
            plot.margin = unit(c(3,1,4,1), "lines"),
            legend.position = c(.5, -.22), legend.direction="horizontal",
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm"), 
            legend.text = element_text(size=legend_label_fontsize)) +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      scale_color_manual(values=c("black", "#ef404d", "#0055aa")) +
      scale_alpha_manual(values=c(0.3, 1), guide="none") +
      annotate("text", x=ymd(20200310), y = Inf, hjust=0.5,
               color="#fbb416", family="gtam", size = MA_label_fontsize,
               label = "State of Emergency\ndeclared in MA\n\n") +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🌍 Incidents by Location 🌍
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Plot map
  output$synced_maps <- renderUI({
    
    map_2019 <- leaflet(df_to_map %>%filter(YEAR==2019), 
                  options = leafletOptions(attributionControl = T)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(lng = ~Long, lat = ~Lat,
                       label = lapply(df_to_map %>%filter(YEAR==2019) %>%pull(labs), HTML),
                       stroke=F, fillOpacity=.2, radius=2.5,
                       color="#0055aa", group="circle_marks") %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }")))
    
    map_2020 <- leaflet(df_to_map %>%filter(YEAR==2020), 
                  options = leafletOptions(attributionControl = T)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(lng = ~Long, lat = ~Lat, 
                       label = lapply(df_to_map %>%filter(YEAR==2020) %>%pull(labs), HTML),
                       stroke=F, fillOpacity=.2, radius=2.5,
                       color="#0055aa", group="circle_marks") %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }")))
    
    sync(map_2019, map_2020)
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🙅🏽 Major & Minor Incidents v. Time 🤷🏽
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
            text = element_text(family="gtam", size = axis_label_fontsize),
            plot.margin = unit(c(1,5,1,1), "lines")) +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot)) +
      scale_color_manual(values=c("#ef404d", "#0055aa")) +
      scale_alpha_manual(values=c(0.3, 1)) +
      annotate("text", x=ymd(20200310)-2.5, y = 60, angle=90, hjust=0.5,
               color="#fbb416", family="gtam", size = MA_label_fontsize,
               label = "State of Emergency\ndeclared in MA") +
      annotate("text", x = last_date_to_plot, y = last_date_value_major, hjust=-.1, vjust = 0.5,
               family="gtam", size = year_label_fontsize, color="#ef404d", fontface="bold",
               label = "Major\nincidents") +
      annotate("text", x = last_date_to_plot, y = last_date_value_minor, hjust=-.1, vjust = -0.5,
               family="gtam", size = year_label_fontsize, fontface="bold", color="#0055aa",
               label = "Minor\nincidents") +
      coord_cartesian(clip = 'off')
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 📉 Incidents v. Time 📉
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
            text = element_text(family="gtam", size = axis_label_fontsize)) +
      scale_color_manual(values=c("black", "#fbb416")) +
      annotate("text", x=ymd(20200310)-2.5, y = 100, angle=90, hjust=0.5,
               color="#fbb416", family="gtam", size = MA_label_fontsize,
               label = "State of Emergency\ndeclared in MA") +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(last_date_to_plot - months(2), last_date_to_plot))
    
  })
  
}

shinyApp(ui = ui, server = server)

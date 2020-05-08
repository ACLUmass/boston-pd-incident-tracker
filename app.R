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
library(plotly)
library(aws.s3)

source("plotly_builders.R")

# Read environment vars
readRenviron(".Renviron")

# Set up connection to S3 bucket
aws_s3_bucket <- get_bucket("app-bpd-incidents")

# Set ggplot settings
theme_set(theme_minimal())
MA_label_fontsize = 5 #4 / 8
MA_label_lineheight = 1 #1 / 0.8
axis_label_fontsize = 18 #18 / 30
legend_label_fontsize = 12 #12 / 20
year_label_fontsize = 5 #5 / 9

# Define filenames and column data types
violations_filename <- "data/violations_major_minor.csv"

# Load violation classifications
violations <- read_csv(violations_filename, 
                       col_types = cols(
                         OFFENSE_CODE = col_double(),
                         Lauren_says_minor = col_logical(),
                         incident_group = col_character(),
                         desc = col_character()
                       ))
group_choices <- c("--", "All", violations %>% 
                     pull(incident_group) %>% 
                     unique() %>% 
                     sort())
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

today <- date(now())

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# UI
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ui <- fluidPage(theme = "bpd_covid19_app.css",
  
  # Use shinyjs package for dynamic enabling/disabling          
  shinyjs::useShinyjs(),
                
  # Add favicon          
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
  ),
  
  # App title ----
  titlePanel("Tracking Boston Police Incidents"),
  
  div(
    navlistPanel(widths = c(3, 9), id="panels",
                 
      tabPanel("About", 
               h4("Explore Boston PD Incidents"),
               p(paste("View plots in the different tabs to track the behavior of the",
                       "Boston Police Department over time, including the frequency and location",
                       "of specific incident types.")),
               
               h4("About the Data"),
               HTML(paste("We source these police incident reports from publicly available data hosted on",
                          "<a href='https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system'>Analyze Boston</a>.",
                          "The reports posted there go back to June 2015, and are (usually) updated daily by the Boston Police.")),
               p("The data sourced for all visualizations on this site are available for download",
                 actionLink("link_to_download", "here."), style="margin-top: 1rem;"),
               
               h4("Source Code"),
               p("Interested programmers can view the source code for this app, written in R, on", 
                 a("GitHub.", href="https://github.com/ACLUmass/boston-pd-incident-tracker", target="_blank"))
               ),
      
      tabPanel("Year-to-Year Comparison", 
               wellPanel(id="internal_well",
                         p("Select date range in 2020 to compare to 2019:"),
                         dateRangeInput("y2y_date", label="")),
               withSpinner(plotlyOutput("year_to_year_plot"), type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Incidents by Type Over Time",
               wellPanel(id="internal_well",
                 p("Select up to three kinds of incidents to plot versus time.", 
                   actionLink("modal_incidents", label = NULL, icon=icon("info-circle"))),
                 splitLayout(
                   selectInput("select_incidentgroup1", label = NULL, choices = group_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_incidentgroup2", label = NULL, choices = group_choices,
                               selected = "Motor Vehicle", multiple=FALSE),
                   selectInput("select_incidentgroup3", label = NULL, choices = group_choices,
                               selected = "Investigations", multiple=FALSE)
                 ),
                 splitLayout(
                   selectInput("select_incident1", label = NULL, choices = group_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_incident2", label = NULL, choices = group_choices,
                               selected = "All", multiple=FALSE),
                   selectInput("select_incident3", label = NULL, choices = group_choices,
                               selected = "All", multiple=FALSE)
                 ),
                 p("Select date range to plot:"),
                 dateRangeInput("inc_type_date", label="")),
               withSpinner(plotlyOutput("incidents_group_plot"), type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("Incident Locations", 
               wellPanel(id="internal_well",
                         p("Select date range in 2020 to compare to 2019:"),
                         dateRangeInput("loc_date", label="",
                                        start = today - months(1) - days(1), end = today - days(1),
                                        min = "2015-06-15", max = today - days(1))
               ),
               uiOutput("range_warning"),
               splitLayout(align="center", h2("2019"), h2("2020")),
               p(align="center", "Showing incident locations between", 
                 textOutput("first_date_str", inline=T), 
                 "and", 
                 textOutput("last_date_str", inline=T), 
                 br(),
                 em("Please note that only ", 
                    textOutput("percent_w_loc", inline=T), 
                    "% of all police incident reports include location coordinates.\n",
                    style="font-size:11px;")),
               withSpinner(uiOutput("synced_maps"), 
                           type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("Major & Minor Incidents Comparison", 
               wellPanel(id="internal_well",
                         p("Select date range to plot:"),
                         dateRangeInput("maj_min_date", label="")),
               withSpinner(plotlyOutput("major_minor_plot"), 
                           type=4, color="#b5b5b5", size=0.5)),
      
      tabPanel("Download Data", 
               p(paste("In order to insulate our data visualizations from",
                       "inconsistencies in the AnalyzeBoston database, we",
                       "maintain our own database of all Boston Police",
                       "incidents since 2015. This database is updated daily,",
                       "adding new incidents and at times updating old incidents if",
                       "BPD provided updated location data for that incident", 
                       "since the previous query. The ACLUM database differs",
                       "from the BPD database in context, not content - that is",
                       "to say, we do not alter incident details or metadata except to",
                       "compile entries cumulatively. This accumulated database",
                       "is the source of all visualizations on this site.")),
               p(strong("Current database size: "),
                 textOutput("n_inc_str", inline=T), 
                 "incidents"),
               p("Our curated database is available for download here:"),
               downloadButton("downloadData", "Download CSV"),
               br(), br(),
               em("The download might take a few seconds."))
      )
  ),
  
  div(id="footer",
      em("\n\nLatest query:", 
         textOutput("last_query_time_str", inline=T), 
         align="right", style="opacity: 0.6;"),
      br(),
      hr(),
      div(align="center",
          a(href="https://www.aclum.org/", target="_blank",
            img(src="Logo_CMYK_Massachusetts_Massachusetts.png", height="50px", 
                style="display: inline; margin: 10px;")),
          a(href="https://www.data.aclum.org/",  target="_blank",
            img(src="D4J-logo.png", height="50px", 
                style="display: inline; margin: 10px;"))),
      p("Please contact lchambers@aclum.org with questions.", align="center", style="opacity: 0.6;")
  )
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Server
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
server <- function(input, output, session) {

  # Load ggplot-friendly font using show_text
  font_add("gtam", "GT-America-Standard-Regular.ttf",
           bold = "GT-America-Standard-Bold.ttf")
  showtext_auto()
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Load Data
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Load all incidents FROM AWS, omg
  df_all <- s3readRDS(object = "all_bpd_incidents_cumulative.rds", 
                      bucket = aws_s3_bucket) %>%
    mutate(OFFENSE_CODE = as.numeric(OFFENSE_CODE),
           date = date(OCCURRED_ON_DATE)) %>%
    merge(violations,
          by="OFFENSE_CODE", all.x=T)
  
  # Get total number of incidents
  n_incidents <- df_all %>% nrow()
  output$n_inc_str <- renderText({n_incidents})
  
  # Load time of last query ALSO FROM AWS wow
  last_query_time <- s3readRDS(object = "query_log.rds", 
                               bucket = aws_s3_bucket) %>%
    tail(1) %>%
    pull(query_time) %>%
    with_tz(tzone="America/New_York") %>%
    format(format="%A %B %e, %Y at %I:%M %p %Z")
  output$last_query_time_str <- renderText({last_query_time})
  
  last_date_to_plot <- date(max(df_all$OCCURRED_ON_DATE, na.rm=T) - days(1))
  first_date_to_plot <- last_date_to_plot - months(3)
  
  earliest_date <- min(df_all$date, na.rm=T)
  
  # Filter for mapping
  df_to_map <- df_all %>%
    filter(!is.na(Long)) %>%
    mutate(Long=as.numeric(Long), Lat = as.numeric(Lat),
           labs = paste(format(with_tz(OCCURRED_ON_DATE, tzone="America/New_York"), 
                               format="%A %B %e, %Y at %I:%M %p"), 
                        desc, sep="<br/>")) %>%
    filter(Long < -69 & Long > -74, Lat < 43 & Lat > 41)
  
  # Calculate percentage of incidents with location
  output$percent_w_loc <- renderText({
    round((n_incidents - df_all %>% filter(is.na(Long)) %>% nrow()) / n_incidents * 100, 1) %>%
      as.character()
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🗓 2019 v. 2020 🗓 
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  isolate({
    updateDateRangeInput(session, "y2y_date", 
                         start = first_date_to_plot, end = last_date_to_plot, 
                         min = ymd(20200101), max = last_date_to_plot)
  })
  
  output$year_to_year_plot <- renderPlotly({
    
    first_date_to_plot <- input$y2y_date[1]
    last_date_to_plot <- input$y2y_date[2]
    
    label_x <- last_date_to_plot + 
      as.difftime(last_date_to_plot - first_date_to_plot, units="days") * .02
    
    df_last_year <- df_all %>%
      mutate(date_to_plot = OCCURRED_ON_DATE %>% as.character %>%
               str_replace("^\\d{4}","2020") %>% as_date) %>%
      filter(YEAR == '2019', 
             date_to_plot <= last_date_to_plot, 
             date_to_plot >= first_date_to_plot) %>%
      group_by(date_to_plot) %>%
      summarize(n = n())  %>%
      ungroup() %>%
      mutate(year = 2019)
    
    data_2019_2020 <- df_all %>%
      filter(date >= first_date_to_plot,
             date <= last_date_to_plot) %>%
      mutate(date_to_plot = date) %>%
      group_by(date_to_plot) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      mutate(year = 2020) %>%
      bind_rows(df_last_year)
    
    g <- data_2019_2020 %>%
    ggplot(aes(x = date_to_plot, y = n, color=as.character(year))) +
      geom_line(size=1, alpha=0.8) +
      ylim(0, 350) +
      labs(x = "", y = "Daily Number of Incidents", color="") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = axis_label_fontsize),
            legend.position='none') +
      scale_color_manual(values=c("black", "#ef404d")) +
      geom_text(data = subset(data_2019_2020, date_to_plot == last_date_to_plot), 
                aes(label = year, colour = as.character(year),
                    x = label_x,
                    y = n), 
                family="gtam", fontface="bold", size=5) +
      scale_x_date(date_labels = "%b %e ",
                   limits = c(first_date_to_plot, label_x),
                   expand = expand_scale(mult = c(.05, .15))) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Incidents", "year_to_year")
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🕰 Incidents by Group v. Time 🕰
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Connect modal to incident info link
  observeEvent(input$modal_incidents, {
    showModal(modalDialog(renderUI(modal_text), easyClose = TRUE, footer = NULL))
  })
  
  # Define helper function to update incident dropdown options when
  # the incident group is updated
  update_incs_by_group <- function(select_input) {
    selector_name <- deparse(substitute(select_input))
    selector_name <- strsplit(selector_name, "$", fixed=T)[[1]] %>% tail(1)
    n_selector <- substr(selector_name, nchar(selector_name), nchar(selector_name))
  
    inc_selector_name <- paste0("select_incident", n_selector)
    
    group_name <- select_input
    
    if (group_name == "--"| group_name == "All") {
      choices <- "--"
      shinyjs::disable(inc_selector_name)
    } else {
      shinyjs::enable(inc_selector_name)
      choices <- df_all %>%
        filter(incident_group == group_name) %>%
        pull(desc) %>%
        unique() %>%
        sort()
      
      choices <- c("All", choices)
    } 
    updateSelectInput(session, inc_selector_name, choices = choices)
  }
  
  # Update individual incident dropdowns when group is changed
  observe({update_incs_by_group(input$select_incidentgroup1)})
  observe({update_incs_by_group(input$select_incidentgroup2)})
  observe({update_incs_by_group(input$select_incidentgroup3)})
  
  # Helper function to create list of values to plot
  get_groups_to_plot <- function(grp_list, inc_list) {
    grp_to_plot <- c()
    for (i in 1:3) {
      grp <- grp_list[i]
      inc <- inc_list[i]
      
      if (grp %in% c("--", "All")) {
        grp <- grp
      } else if (inc == "All") {
        grp <- grp
      } else {
        grp <- inc
      }
      
      grp_to_plot[i] <- grp
    }
    return(grp_to_plot)
  }
  
  # Determine which incidents/groups to plot
  inc_grps_to_plot <- reactive({
    c(input$select_incidentgroup1, 
      input$select_incidentgroup2,
      input$select_incidentgroup3)
  })
  incs_to_plot <- reactive({
    c(input$select_incident1, 
      input$select_incident2,
      input$select_incident3)
  })
  
  # Update date range
  isolate({
    updateDateRangeInput(session, "inc_type_date", 
      start = first_date_to_plot, end = last_date_to_plot, 
      min = earliest_date, max = last_date_to_plot)
  })
  
  # Plot
  output$incidents_group_plot <- renderPlotly({
    
    first_date_to_plot <- input$inc_type_date[1]
    last_date_to_plot <- input$inc_type_date[2]
    
    grps_to_plot <- get_groups_to_plot(inc_grps_to_plot(), incs_to_plot())
    
    all_df_all <- df_all %>%
      group_by(date) %>%
      summarize(n = n()) %>%
      mutate(incident_group = "All")
    
    df_by_incidentgroup <- df_all %>%
      group_by(date, incident_group) %>%
      summarize(n = n())
    
    df_by_incident <- df_all %>%
      group_by(date = date, desc) %>%
      summarize(n = n()) %>%
      rename(incident_group = desc) %>%
      ungroup()%>%
      bind_rows(all_df_all) %>%
      bind_rows(df_by_incidentgroup)
  
    # Fill in dates where there are none of a given incident 
    df_by_incident <- tidyr::complete(df_by_incident, incident_group, date,
                                      fill=list(n=0))
    
    g <- df_by_incident %>%
      filter(incident_group %in% grps_to_plot) %>%
    ggplot(aes(x=date, y = n, color=incident_group)) +
      geom_line(size=1, show.legend = T, alpha=0.8) +
      labs(x = "", y = "Number of Incidents", color="") +
      theme(text = element_text(family="gtam", size = axis_label_fontsize),
            legend.background = element_rect(fill=alpha('lightgray', 0.4), color=NA),
            legend.key.width = unit(1, "cm")) +
      scale_x_date(date_labels = "%b %e ", 
                   limits = c(first_date_to_plot, last_date_to_plot)) +
      scale_color_manual(values=c("black", "#ef404d", "#fbb416")) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Incidents", "inc_by_type")
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # 🌍 Incidents by Location 🌍
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  
  # Plot map
  output$synced_maps <- renderUI({
    
    first_date_to_plot <- input$loc_date[1]
    last_date_to_plot <- input$loc_date[2]
    
    if (last_date_to_plot - first_date_to_plot > days(31)) {
      first_date_to_plot <- last_date_to_plot - months(1)
      
      output$range_warning <- renderUI({
        div(id="dev-warning",
            wellPanel(
              fluidRow(
                column(1, icon('exclamation-triangle')),
                column(11, h4("Max Date Range Exceeded"),
                       em("Due to computation limitations, please limit",
                          "your date range to one month or shorter", 
                          style="margin-top:0px"))
              )
            )
        )
      })
    } else {
      output$range_warning <- renderUI({
        div(style="display:none;")
      })
    }
    
    output$first_date_str <- renderText({format(first_date_to_plot, format="%B %e")})
    output$last_date_str <- renderText({format(last_date_to_plot, format="%B %e.\n")})
    
    map_2019 <- df_to_map %>%
      filter(date >= ymd(gsub("\\d{4}", "2019", first_date_to_plot)),
             date <= ymd(gsub("\\d{4}", "2019", last_date_to_plot))) %>%
    leaflet(options = leafletOptions(attributionControl = T)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(lng = ~Long, lat = ~Lat,
                       label = ~lapply(labs, HTML),
                       stroke=F, fillOpacity=.2, radius=2.5,
                       color="#0055aa", group="circle_marks") %>%
      addEasyButton(easyButton(
        icon="fa-home", title="Reset",
        onClick=JS("function(btn, map){ 
                   var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                   map.fitBounds(groupLayer.getBounds());
               }")))
    
    map_2020 <- df_to_map %>%
      filter(date <= last_date_to_plot,
             date >= first_date_to_plot) %>%
    leaflet(options = leafletOptions(attributionControl = T)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addCircleMarkers(lng = ~Long, lat = ~Lat, 
                       label = ~lapply(labs, HTML),
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
  
  isolate({
    updateDateRangeInput(session, "maj_min_date", 
                         start = first_date_to_plot, end = last_date_to_plot, 
                         min = earliest_date, max = last_date_to_plot)
  })
  
  output$major_minor_plot <- renderPlotly({
    
    first_date_to_plot <- input$maj_min_date[1]
    last_date_to_plot <- input$maj_min_date[2]
    
    label_x <- last_date_to_plot +
      as.difftime(last_date_to_plot - first_date_to_plot, units="days") * .01

    data_major_minor <- df_all %>%
      filter(date <= last_date_to_plot) %>%
      group_by(date, Lauren_says_minor) %>%
      summarize(n = n())
    
    labels_too_close <- data_major_minor %>%
      filter(date == last_date_to_plot) %>%
      pull(n) %>%
      diff() < 25
    
    maj_min_labs <- data_major_minor %>%
      filter(date == last_date_to_plot) %>%
      pull(n)
    
    if (labels_too_close) {
      if (maj_min_labs %>% unique() %>% length() == 1) {
        maj_min_labs[2] <- max(maj_min_labs) + 25
      } else {
        maj_min_labs[maj_min_labs == max(maj_min_labs)] <- max(maj_min_labs) + 15
      }
    }
    
    g <- data_major_minor %>%
    ggplot(aes(x=date, y = n, color = Lauren_says_minor)) +
      geom_line(size=1, show.legend = FALSE, alpha=.8) +
      ylim(0, 200) +
      labs(x = "", y = "Number of Incidents", color="") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = axis_label_fontsize),
            legend.position="none") +
      scale_x_date(date_labels = "%b %e ",
                   limits = c(first_date_to_plot, label_x),
                   expand = expand_scale(mult = c(0, .2))) +
      scale_color_manual(values=c("#ef404d", "#fbb416")) +
      geom_text(data = subset(data_major_minor, date == last_date_to_plot), 
                aes(label = ifelse(Lauren_says_minor == "TRUE", 
                                   "Minor\nIncidents", "Major\nIncidents"), 
                    color = Lauren_says_minor,
                    x = label_x,
                    y = maj_min_labs), 
                family="gtam", fontface="bold", hjust=0) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Incidents", "major_minor")
    
  })
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # ⬇️ Download CSV ⬇️
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  observeEvent(input$link_to_download, {
    updateTabsetPanel(session, "panels", "Download Data")
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      timestamp <- parse_date_time(str_replace(last_query_time, "\\s[^ ]+$", ""), 
                                   "%A %B %e, %Y at %I:%M %p", exact=T, tz="America/New_York") %>%
        format(format = "%Y%m%d_%H%M")
      paste0(timestamp, "_all_bpd_incidents.csv")
    },
    content = function(file) {
      withProgress(message = 'Downloading...', value = 1, {
        write.csv(df_all, file)
      })
    }
  )
  
}

shinyApp(ui = ui, server = server)

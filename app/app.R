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
source("send_error_email.R")

# Initialization --------------------------------------------------------------

# Read environment vars
readRenviron(".Renviron")

# Set up function to email me if there's an error
options(shiny.error = send_email)

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
violations_filename <- "offense_codes_descs.csv"

# Load violation classifications
violations <- read_csv(violations_filename, 
                       col_types = cols(
                         OFFENSE_CODE = col_double(),
                         minor = col_logical(),
                         incident_group = col_character(),
                         OFFENSE_DESCRIPTION = col_character(),
                         FBI_UCR = col_character()
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
    mutate(code_desc = paste(OFFENSE_CODE, OFFENSE_DESCRIPTION, sep=" - ")) %>%
    arrange(OFFENSE_DESCRIPTION) %>%
    pull(code_desc)
  
  for (incident in incidents_in_grp) {
    modal_text[[i]] <- p(incident)
    i <- i + 1
  }
}

# Define today's date
today_date <- lubridate::date(now())

# Load ggplot-friendly font using show_text
font_add("gtam", "www/fonts/gtamerica/GT-America-Standard-Regular.ttf",
         bold = "www/fonts/gtamerica/GT-America-Standard-Bold.ttf")
showtext_auto()

# Write warning about incident types changing
inc_warning <- 'The Boston Police have changed their categorization system 
for incidents since they first began reporting this data in 2015. One major change 
seems to have occurred on September 29, 2019. Thus, please be aware that to 
accurately compare one kind of incident across time, multiple different incident 
labels might need to be analyzed together. For example, fires are now all reported under 
"FIRE REPORT", but previously were divided into “FIRE REPORT - HOUSE, BUILDING, 
ETC.” and “FIRE REPORT - CAR, BRUSH, ETC.”.'

# UI --------------------------------------------------------------
ui <- fluidPage(theme = "bpd_covid19_app.css",
  
  # Use shinyjs package for dynamic enabling/disabling          
  shinyjs::useShinyjs(),
                
  # Add favicon          
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "512x512", href = "favicon.png")
  ),
  
  # App title
  titlePanel("Tracking Boston Police Incidents"),
  
  div(
    navlistPanel(widths = c(3, 9), id="panels",
                 
      tabPanel("About", 
               h4("Explore Boston PD Incidents"),
               p(paste("View plots in the different tabs to track the activity of the",
                       "Boston Police Department over time, including the frequency and location",
                       "of specific incident types.")),
               
               h4("About the Data"),
               HTML(paste("We source these police incident reports from publicly available data hosted on",
                          "<a href='https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system'>Analyze Boston</a>.",
                          "The reports posted there go back to June 2015, and are (usually) updated daily by the Boston Police.")),
               p("The data sourced for all visualizations on this site are available for download",
                 actionLink("link_to_download", "here."), style="margin-top: 1rem;"),
               
               h4("Note on Time Analysis"),
               p(inc_warning),
               
               h4("Source Code"),
               p("Interested programmers can view the source code for this app, written in R, on", 
                 a("GitHub.", href="https://github.com/ACLUmass/boston-pd-incident-tracker", target="_blank"))
               ),
      
      tabPanel("Year-to-Year Comparison", 
               wellPanel(id="internal_well",
                         p("Select one kind of incident* to plot:", 
                           actionLink("modal_incidents3", label = NULL, icon=icon("info-circle"))),
                         splitLayout(
                            selectInput("select_incidentgroup_yr2yr", "CATEGORY", 
                                        choices = tail(group_choices, length(group_choices) - 1),
                                            selected = "All", multiple=FALSE),
                            selectInput("select_incident_yr2yr","SUB-CATEGORY", choices = group_choices,
                                            selected = "--", multiple=FALSE)
                            ),
                         p("Select date range to compare to the prior year:"),
                         dateRangeInput("y2y_date", label=""),
                         em("*Be aware of how",
                            actionLink("modal_warning1", "changes in incident categories over time"),
                            "might affect analysis.", style="padding-top: 1rem; display: block;")
                         ),
               splitLayout(
                 div(h2(textOutput("n_incs_2019"), align="center"),
                     p(textOutput("yr2yr_type", inline=T),
                       "incidents during selected date range in prior year", align="center")),
                 div(h2(textOutput("n_incs_2020"), align="center"),
                     p(textOutput("yr2yr_type1", inline=T),
                       "incidents during selected date range", align="center")
                     )),
               withSpinner(plotlyOutput("year_to_year_plot"), type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("Incidents by Type Over Time",
               wellPanel(id="internal_well",
                 p("Select up to three kinds of incidents* to plot versus time.", 
                   actionLink("modal_incidents", label = NULL, icon=icon("info-circle"))),
                 em("Category:", style="display: inline-block; width: 100px;"),
                 span(style="display: inline-block; width: calc(100% - 110px);",
                   splitLayout(
                     selectInput("select_incidentgroup1", label = NULL, choices = group_choices,
                                 selected = "All", multiple=FALSE),
                     selectInput("select_incidentgroup2", label = NULL, choices = group_choices,
                                 selected = "Motor Vehicle", multiple=FALSE),
                     selectInput("select_incidentgroup3", label = NULL, choices = group_choices,
                                 selected = "Investigations", multiple=FALSE)
                   )
                 ),
                 em("Sub-Category:", style="display: inline-block; width: 100px;"),
                 span(style="display: inline-block; width: calc(100% - 110px);",
                   splitLayout(
                     selectInput("select_incident1", label = NULL, choices = group_choices,
                                 selected = "All", multiple=FALSE),
                     selectInput("select_incident2", label = NULL, choices = group_choices,
                                 selected = "All", multiple=FALSE),
                     selectInput("select_incident3", label = NULL, choices = group_choices,
                                 selected = "All", multiple=FALSE)
                   )
                 ),
                 p("Select date range to plot:"),
                 dateRangeInput("inc_type_date", label=""),
                 em("*Be aware of how",
                    actionLink("modal_warning2", "changes in incident categories over time"),
                    "might affect analysis.", style="padding-top: 1rem; display: block;")
               ),
               splitLayout(
                 div(h2(textOutput("n_incs_inc1"), align="center"),
                     p(textOutput("inc1_type", inline=T),
                       "incidents during selected date range", align="center")),
                 div(h2(textOutput("n_incs_inc2"), align="center"),
                     p(textOutput("inc2_type", inline=T),
                       "incidents during selected date range", align="center")),
                 div(h2(textOutput("n_incs_inc3"), align="center"),
                     p(textOutput("inc3_type", inline=T),
                       "incidents during selected date range", align="center"))
               ),
               withSpinner(plotlyOutput("incidents_group_plot"), type=4, color="#b5b5b5", size=0.5)
               ),
      
      tabPanel("Incident Locations",
               wellPanel(id="internal_well",
                 p("Select up to three kinds of incidents* to plot versus time.", 
                   actionLink("modal_incidents2", label = NULL, icon=icon("info-circle"))),
                 em("Category:", style="display: inline-block; width: 100px;"),
                 span(style="display: inline-block; width: calc(100% - 110px);",
                      splitLayout(
                        selectInput("select_incidentgroup_map1", label = NULL, choices = group_choices,
                                    selected = "Verbal Dispute", multiple=FALSE),
                        selectInput("select_incidentgroup_map2", label = NULL, choices = group_choices,
                                    selected = "Motor Vehicle", multiple=FALSE),
                        selectInput("select_incidentgroup_map3", label = NULL, choices = group_choices,
                                    selected = "Investigations", multiple=FALSE)
                      )
                 ),
                 em("Sub-Category:", style="display: inline-block; width: 100px;"),
                 span(style="display: inline-block; width: calc(100% - 110px);",
                      splitLayout(
                        selectInput("select_incident_map1", label = NULL, choices = group_choices,
                                    selected = "All", multiple=FALSE),
                        selectInput("select_incident_map2", label = NULL, choices = group_choices,
                                    selected = "All", multiple=FALSE),
                        selectInput("select_incident_map3", label = NULL, choices = group_choices,
                                    selected = "All", multiple=FALSE)
                      )
                 ),
                 p("Select date range to compare to prior year:"),
                 dateRangeInput("loc_date", label="",
                                start = today_date - days(31), end = today_date - days(1),
                                min = "2015-06-15", max = today_date - days(1)),
                 em("*Be aware of how",
                    actionLink("modal_warning3", "changes in incident categories over time"),
                    "might affect analysis.", style="padding-top: 1rem; display: block;")
               ),
               uiOutput("range_warning"),
               splitLayout(align="center", 
                           h2(textOutput("prior_year_str", inline=T)), 
                           h2(textOutput("current_year_str", inline=T))),
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
                         # em("“Major” incidents include charges like homicide,",
                         #    "sexual assault, most robbery/burglary, assault and",
                         #    "battery, arson, or weapons possession. “Minor” incidents",
                         #    "include incidents like larceny, vandalism, sex work,",
                         #    "drug possession, disorderly conduct, fare evasion,",
                         #    "noise complaints, and motor vehicle infractions."),
                         em("We use the FBI", 
                          a("Uniform Crime Reporting", href="https://ucr.fbi.gov/additional-ucr-publications/ucr_handbook.pdf"),
                            "(UCR) categories to",
                            "delineate between 'major' and 'minor' incidents.",
                            "UCR Part I incidents, here categorized as major, include homicide, rape,",
                            "robbery, aggravated assault, burglary, larceny, and human",
                            "trafficking. All other incidents are ",
                            "UCR Part II, here categorized as minor."),
                         br(), br(),
                         p("Select date range to plot:"),
                         dateRangeInput("maj_min_date", label="")),
               splitLayout(
                 div(h2(textOutput("n_incs_maj"), align="center"),
                     p("UCR Part I incidents during selected date range", align="center")),
                 div(h2(textOutput("n_incs_min"), align="center"),
                     p("UCR Part II incidents during selected date range", align="center"))
               ),
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

# Server --------------------------------------------------------------
server <- function(input, output, session) {
  
  # Load Data --------------------------------------------------------------
  
  # Load all incidents FROM AWS, omg
  df_all <- s3readRDS(object = "all_bpd_incidents_cumulative.rds", 
                      bucket = aws_s3_bucket) %>%
    mutate(OFFENSE_CODE = as.numeric(OFFENSE_CODE),
           date = lubridate::date(OCCURRED_ON_DATE)) %>%
    merge(violations %>% select(-OFFENSE_CODE),
          by="OFFENSE_DESCRIPTION", all.x=T)
  
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
  
  last_date_to_plot <- lubridate::date(max(df_all$OCCURRED_ON_DATE, na.rm=T) - days(1))
  first_date_to_plot <- last_date_to_plot - days(90)
  
  earliest_date <- min(df_all$date, na.rm=T)
  
  # Calculate percentage of incidents with location
  output$percent_w_loc <- renderText({
    round((n_incidents - df_all %>% filter(is.na(Long)) %>% nrow()) / n_incidents * 100, 1) %>%
      as.character()
  })
  
  # Organize by incident/group
  all_df_all <- df_all %>%
    group_by(date) %>%
    summarize(n = n()) %>%
    mutate(incident_group = "All")
  
  df_by_incidentgroup <- df_all %>%
    group_by(date, incident_group) %>%
    summarize(n = n())
  
  df_by_incident <- df_all %>%
    group_by(date = date, OFFENSE_DESCRIPTION) %>%
    summarize(n = n()) %>%
    rename(incident_group = OFFENSE_DESCRIPTION) %>%
    ungroup()%>%
    bind_rows(all_df_all) %>%
    bind_rows(df_by_incidentgroup)
  
  # Fill in dates where there are none of a given incident 
  df_by_incident <- tidyr::complete(df_by_incident, incident_group, date,
                                    fill=list(n=0))
  
  # Filter for mapping
  df_to_map <- df_all %>%
    filter(!is.na(Long)) %>%
    mutate(Long=as.numeric(Long), Lat = as.numeric(Lat),
           labs = paste(format(with_tz(OCCURRED_ON_DATE, tzone="America/New_York"), 
                               format="%A %B %e, %Y at %I:%M %p"), 
                        incident_group,
                        OFFENSE_DESCRIPTION, sep="<br/>")) %>%
    filter(Long < -69 & Long > -74, Lat < 43 & Lat > 41)
  
  # 🗓 Current year v. prior year 🗓 --------------------------------------------------------

  isolate({
    updateDateRangeInput(session, "y2y_date", 
                         start = first_date_to_plot, end = last_date_to_plot, 
                         min = ymd(20200101), max = last_date_to_plot)
  })
  
  # Connect modal2 so incident info link
  observeEvent(input$modal_incidents3, {
    showModal(modalDialog(renderUI(modal_text), easyClose = TRUE, footer = NULL))
  })
  observeEvent(input$modal_warning1, {
    showModal(modalDialog(renderUI(list(h4("Note on Time Analysis"), inc_warning)), 
                          easyClose = TRUE, footer = NULL))
  })
  
  # Update individual incident dropdowns when group is changed
  observe({update_incs_by_group(input$select_incidentgroup_yr2yr)})
  
  output$year_to_year_plot <- renderPlotly({
    
    # Determine incident groups/types to plot
    grps_to_plot_yr2yr <- get_groups_to_plot(input$select_incidentgroup_yr2yr, 
                                             input$select_incident_yr2yr)
    output$yr2yr_type <- renderText({grps_to_plot_yr2yr$value})
    output$yr2yr_type1 <- renderText({grps_to_plot_yr2yr$value})
    
    # Apply date selections
    first_date_to_plot <- input$y2y_date[1]
    last_date_to_plot <- input$y2y_date[2]
    date_range_to_plot <- seq(first_date_to_plot, last_date_to_plot, by="days")
    current_year <- year(last_date_to_plot)
    prior_year <- current_year - 1   

    # Calculate x location of year labels
    label_x <- last_date_to_plot + 
      as.difftime(last_date_to_plot - first_date_to_plot, units="days") * .02
    
    # Filter based on incident selections
    if (grps_to_plot_yr2yr$type == "group") {
      if (grps_to_plot_yr2yr$value != "All") {
        df_filtered <- df_all %>%
          filter(incident_group == grps_to_plot_yr2yr$value)
      } else {
        df_filtered <- df_all
      }
    } else if (grps_to_plot_yr2yr$type == "incident") {
      df_filtered <- df_all %>%
        filter(OFFENSE_DESCRIPTION == grps_to_plot_yr2yr$value)
    }
    
    # Assemble DF with 2 years of data
    
    df_last_year <- df_filtered %>%
      mutate(date_to_plot = OCCURRED_ON_DATE %>% as.character %>%
               as_date() + years(1)) %>%
      filter(date_to_plot <= last_date_to_plot, 
             date_to_plot >= first_date_to_plot) %>%
      group_by(date_to_plot) %>%
      summarize(n = n())  %>%
      ungroup() %>%
      mutate(year = prior_year)
    
    data_2019_2020 <- df_filtered %>%
      filter(date >= first_date_to_plot,
             date <= last_date_to_plot) %>%
      mutate(date_to_plot = date) %>%
      group_by(date_to_plot) %>%
      summarize(n = n()) %>%
      ungroup() %>%
      mutate(year = current_year) %>%
      bind_rows(df_last_year)
    
    # Fill in dates where there are none of a given incident 
    data_2019_2020 <- tidyr::complete(data_2019_2020, 
                                      date_to_plot = date_range_to_plot,
                                      year=c(prior_year, current_year), 
                                      fill=list(n=0)) %>%
      # Remove Feb 29 2019
      filter(year > 2019 | (year == 2019 & date_to_plot != ymd(20200229)))
    
    # Update the incident counters
    output$n_incs_2019 <- renderText({
      data_2019_2020 %>% 
        filter(year == prior_year) %>%
        pull(n) %>%
        sum() %>%
        format(big.mark = ",")
    })
    
    output$n_incs_2020 <- renderText({
      data_2019_2020 %>% 
        filter(year == current_year) %>%
        pull(n) %>%
        sum() %>%
        format(big.mark = ",")
    })
    
    # Plot!
    g <- data_2019_2020 %>%
    ggplot(aes(x = date_to_plot, y = n, color=as.character(year))) +
      geom_line(size=1, alpha=0.8) +
      ylim(0, NA) +
      labs(x = "", y = "Daily Number of Incidents", color="") +
      theme(plot.title= element_text(family="gtam", face='bold'),
            text = element_text(family="gtam", size = axis_label_fontsize),
            legend.position='none') +
      scale_color_manual(values=c("black", "#ef404d")) +
      geom_text(data = year_label_data, 
                aes(label = year, colour = as.character(year),
                    x = label_x,
                    y = n), 
                family="gtam", fontface="bold", size=5) +
      scale_x_date(date_labels = "%b %e ",
                   limits = c(first_date_to_plot, label_x),
                   expand = expansion(mult = c(.05, .15))) +
      coord_cartesian(clip = 'off')
    
    lines_plotly_style(g, "Incidents", "year_to_year", current_year=current_year)
    
  })
  
  # 🕰 Incidents by Group v. Time 🕰 ------------------------------------------

  # Connect modal to incident info link
  observeEvent(input$modal_incidents, {
    showModal(modalDialog(renderUI(modal_text), easyClose = TRUE, footer = NULL))
  })
  observeEvent(input$modal_warning2, {
    showModal(modalDialog(renderUI(list(h4("Note on Time Analysis"), inc_warning)), 
                          easyClose = TRUE, footer = NULL))
  })
  
  # Define helper function to update incident dropdown options when
  # the incident group is updated
  update_incs_by_group <- function(select_input) {
    grp_selector_name <- deparse(substitute(select_input))
    grp_selector_name <- strsplit(grp_selector_name, "$", fixed=T)[[1]] %>% tail(1)
    n_selector <- substr(grp_selector_name, 
                         nchar(grp_selector_name), nchar(grp_selector_name))
    
    inc_selector_name <- sub(".$", n_selector, grp_selector_name)
    inc_selector_name <- sub("group", "", inc_selector_name)
    
    group_name <- select_input
    
    if (group_name == "--"| group_name == "All") {
      choices <- "--"
      shinyjs::disable(inc_selector_name)
    } else {
      shinyjs::enable(inc_selector_name)
      choices <- df_all %>%
        filter(incident_group == group_name) %>%
        pull(OFFENSE_DESCRIPTION) %>%
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
    n_select <- length(grp_list)
    
    grp_to_plot <- data.frame(value=character(), type=character(), 
                              stringsAsFactors=FALSE)
    for (i in 1:n_select) {
      grp <- grp_list[i]
      inc <- inc_list[i]
      
      if (grp %in% c("--", "All")) {
        grp_to_plot[i,] <- list(value=grp,type= "group")
      } else if (inc == "All") {
        grp_to_plot[i,] <- list(value=grp,type= "group")
      } else {
        grp_to_plot[i,] <- list(value=inc,type= "incident")
      }
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
    
    grps_to_plot <- get_groups_to_plot(inc_grps_to_plot(), incs_to_plot())$value
    
    output$n_incs_inc1 <- renderText({
      df_by_incident %>%
        filter(date <= last_date_to_plot,
               date >= first_date_to_plot,
               incident_group == grps_to_plot[1]) %>%
        pull(n) %>%
        sum() %>%
        format(big.mark = ",")
    })
    output$inc1_type <- renderText({grps_to_plot[1]})
    
    output$n_incs_inc2 <- renderText({
      df_by_incident %>% 
        filter(date <= last_date_to_plot,
               date >= first_date_to_plot,
               incident_group == grps_to_plot[2]) %>%
        pull(n) %>%
        sum() %>%
        format(big.mark = ",")
    })
    output$inc2_type <- renderText({grps_to_plot[2]})
    
    output$n_incs_inc3 <- renderText({
      df_by_incident %>%
        filter(date <= last_date_to_plot,
               date >= first_date_to_plot,
               incident_group == grps_to_plot[3]) %>%
        pull(n) %>%
        sum() %>%
        format(big.mark = ",")
    })
    output$inc3_type <- renderText({grps_to_plot[3]})
    
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
  
  # 🌍 Incidents by Location 🌍 -----------------------------------------------
  
  # Connect modal to incident info link
  observeEvent(input$modal_incidents2, {
    showModal(modalDialog(renderUI(modal_text), easyClose = TRUE, footer = NULL))
  })
  observeEvent(input$modal_warning3, {
    showModal(modalDialog(renderUI(list(h4("Note on Time Analysis"), inc_warning)), 
                          easyClose = TRUE, footer = NULL))
  })
  
  # Update individual incident dropdowns when group is changed
  observe({update_incs_by_group(input$select_incidentgroup_map1)})
  observe({update_incs_by_group(input$select_incidentgroup_map2)})
  observe({update_incs_by_group(input$select_incidentgroup_map3)})
  
  # Determine which incidents/groups to map
  inc_grps_to_map <- reactive({
    c(input$select_incidentgroup_map1, 
      input$select_incidentgroup_map2,
      input$select_incidentgroup_map3)
  })
  incs_to_map <- reactive({
    c(input$select_incident_map1, 
      input$select_incident_map2,
      input$select_incident_map3)
  })

  # Plot map
  output$synced_maps <- renderUI({
    
    # Determine incident groups/types to map
    grps_to_map <- get_groups_to_plot(inc_grps_to_map(), incs_to_map())
    
    if ("All" %in% grps_to_map$value) {
      grps_filter <- df_all$incidentgroup %>% unique()
      incs_filter <- df_all$OFFENSE_DESCRIPTION %>% unique()
      
      df_to_map_clr <- df_to_map %>%
        mutate(color = "#0055aa")
      
      colors <- "#0055aa"
      labels <- "All"
    } else {
      grps_filter <- grps_to_map[grps_to_map$type == "group",]$value
      incs_filter <- grps_to_map[grps_to_map$type == "incident",]$value
      
      df_to_map_clr <- df_to_map %>%
        mutate(color = ifelse(incident_group == grps_to_map$value[1] | 
                                OFFENSE_DESCRIPTION == grps_to_map$value[1], "#0055aa",
                              ifelse(incident_group == grps_to_map$value[2] | 
                                       OFFENSE_DESCRIPTION == grps_to_map$value[2], "#ef404d",
                                     ifelse(incident_group == grps_to_map$value[3] | 
                                              OFFENSE_DESCRIPTION == grps_to_map$value[3], "#fbb416", NA))))
                                            
      
      colors <- c("#0055aa", "#ef404d", "#fbb416")
      labels <- grps_to_map$value
      
      # Check for empty labels
      i_empty <- which(labels %in% "--")
      if (length(i_empty) != 0) {
        colors <- colors[i_empty * -1]
        labels <- labels[i_empty * -1]
      }
    }
    
    # Determine date range to map
    first_date_to_plot <- input$loc_date[1]
    last_date_to_plot <- input$loc_date[2]
    
    if (last_date_to_plot - first_date_to_plot > days(31)) {
      first_date_to_plot <- last_date_to_plot - days(30)
      
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

    current_year <- year(last_date_to_plot)
    prior_year <- current_year - 1
    
    output$current_year_str <- renderText({current_year})
    output$prior_year_str <- renderText({prior_year})

    # Create 2019 map
    df_to_map_2019 <- df_to_map_clr %>%
      filter(!is.na(color),
             date >= as_date(first_date_to_plot) - years(1),
             date <= as_date(last_date_to_plot) - years(1))
    
    map_2019 <- df_to_map_2019 %>%
      leaflet(options = leafletOptions(attributionControl = T)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas)
    
    if (nrow(df_to_map_2019) != 0) {
      map_2019 <- map_2019 %>%
          addCircleMarkers(data = df_to_map_2019,
                           lng = ~Long, lat = ~Lat,
                           label = ~lapply(labs, HTML),
                           stroke=F, fillOpacity=.4, radius=2.5,
                           color = ~color, group="circle_marks") %>%
          addEasyButton(easyButton(
            icon="fa-home", title="Reset",
            onClick=JS("function(btn, map){ 
                       var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                       map.fitBounds(groupLayer.getBounds());
                   }"))) %>%
          addLegend(position = "bottomright",
                    colors = colors,
                    labels = labels)
    }
    
    # Create 2020 map
    df_to_map_2020 <- df_to_map_clr %>%
      filter(!is.na(color),
             date <= last_date_to_plot,
             date >= first_date_to_plot)
    
    map_2020 <- df_to_map_2020 %>%
      leaflet(options = leafletOptions(attributionControl = T)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas)
    
    if (nrow(df_to_map_2020) != 0) {
      map_2020 <- map_2020 %>%
        addCircleMarkers(data = df_to_map_2020,
                         lng = ~Long, lat = ~Lat, 
                         label = ~lapply(labs, HTML),
                         stroke=F, fillOpacity=.4, radius=2.5,
                         color = ~color, group="circle_marks") %>%
        addEasyButton(easyButton(
          icon="fa-home", title="Reset",
          onClick=JS("function(btn, map){ 
                     var groupLayer = map.layerManager.getLayerGroup('circle_marks');
                     map.fitBounds(groupLayer.getBounds());
                 }"))) %>%
        addLegend(position = "bottomright",
                  colors = colors,
                  labels = labels)
    }
      
    sync(map_2019, map_2020)
    
  })
  
  # 🙅🏽 Major & Minor Incidents v. Time 🤷🏽 -------------------------------------

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
      filter(date <= last_date_to_plot,
             date >= first_date_to_plot) %>%
      group_by(date, FBI_UCR) %>%
      summarize(n = n())

    if (nrow(data_major_minor) > 0) {
    
      # Counts of major & minor in given date range
      output$n_incs_maj <- renderText({
        data_major_minor %>%
          filter(FBI_UCR==1) %>%
          pull(n) %>%
          sum() %>%
          format(big.mark = ",")
      })
      
      output$n_incs_min <- renderText({
        data_major_minor %>%
          filter(FBI_UCR==2) %>%
          pull(n) %>%
          sum() %>%
          format(big.mark = ",")
      })

      # Deal with UCR labels being on top of each other
      ucr_label_data <- data_major_minor %>%
        ungroup() %>%
        filter(date == last_date_to_plot) %>%
        mutate(date = label_x)

      labels_too_close <- ucr_label_data %>%
        pull(n) %>%
        diff() < 25
      
      if (labels_too_close) {
        if (ucr_label_data$n %>% unique() %>% length() == 1) {
          ucr_label_data$n[2] <- max(ucr_label_data$n) + 25
        } else {
          ucr_label_data$n[ucr_label_data$n == max(ucr_label_data$n)] <- max(ucr_label_data$n) + 15
        }
      }
      
      g <- data_major_minor %>%
      ggplot(aes(x=date, y = n, color = FBI_UCR)) +
        geom_line(size=1, show.legend = FALSE, alpha=.8) +
        labs(x = "", y = "Number of Incidents", color="") +
        theme(plot.title= element_text(family="gtam", face='bold'),
              text = element_text(family="gtam", size = axis_label_fontsize),
              legend.position="none") +
        scale_x_date(date_labels = "%b %e ",
                     limits = c(first_date_to_plot, label_x),
                     expand = expansion(mult = c(0, .2))) +
        scale_color_manual(values=c("#ef404d", "#fbb416")) +
        geom_text(data = ucr_label_data, 
                  aes(label = ifelse(FBI_UCR == 2, 
                                     "UCR Part II\nIncidents", "UCR Part I\nIncidents")),
                  family="gtam", fontface="bold", hjust=0) +
        coord_cartesian(clip = 'off')
      
      lines_plotly_style(g, "Incidents", "major_minor")
    }
    
  })
  
  # ⬇️ Download CSV ⬇️ --------------------------------------------------------
  
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

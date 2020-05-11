#!/usr/local/bin/Rscript
# R module to build interactive plots with ggplotly for BPD Tracker
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: April 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Load ggplot-friendly font using show_text
font_add("gtam", "www/fonts/gtamerica/GT-America-Standard-Regular.ttf",
         bold = "www/fonts/gtamerica/GT-America-Standard-Bold.ttf")
showtext_auto()

# Define ggplotly settings
label_lightback <- list(
  bordercolor = "white",
  font = list(
    family = "gtam",
    size = 15,
    color="black"
  )
)
label_darkback <- list(
  bordercolor = "white",
  font = list(
    family = "gtam",
    size = 15,
    color="white"
  )
)
modeBarButtonsToRemove <- c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                            "select2d", "lasso2d", "autoScale2d",
                            "hoverClosestCartesian",
                            "hoverCompareCartesian", "toggleSpikelines")
modeBarButtonsToRemove_time <- c("select2d","zoom2d","lasso2d", 
                                 "hoverClosestCartesian","autoScale2d",
                            "hoverCompareCartesian", "toggleSpikelines")
legend_layout_top <- list(orientation = "h", 
                          x = 0.5, y=1.2,
                          xanchor="center",
                          bgcolor = alpha('lightgray', 0.4))
legend_layout_bottom <- list(orientation = "h", 
                          x = 0.5, y=-.2,
                          xanchor="center",
                          bgcolor = alpha('lightgray', 0.4))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Convert lines to ggplotly
lines_plotly_style <- function(gg_plot, y_label, plot_type) {

  g <- ggplotly(gg_plot, tooltip = c("x", "y"))
  
  if (plot_type == "year_to_year") {
    traces_lightback <- 2
    traces_darkback <- 1
    traces_to_hide <- 3
    
    g <- g %>%
      style(textposition = "right", traces = traces_to_hide)
    
  } else if (plot_type == "inc_by_type"){
    if (length(g$x$data) == 1) {
      traces_lightback <- 0
      traces_darkback <- 1
    } else if (length(g$x$data) == 2) {
      traces_lightback <- 0
      traces_darkback <- 1:2
    } else if (length(g$x$data) == 3) {
      traces_lightback <- 3
      traces_darkback <- 1:2
    }
    
    traces_to_hide <- 0
    
  } else if (plot_type == "major_minor") {
    traces_lightback <- 1:2
    traces_darkback <- 0
    traces_to_hide <- 3:4
      
    g <- g %>%
      style(textposition = "right", traces = traces_to_hide)
  }
  
  # Replace tooltip key with better names
  for (i in 1:length(g$x$data)) {
    text_rep <- g$x$data[[i]]$text %>%
      gsub("date_to_plot", "Date", .) %>%
      gsub("date", "Date", .)
    
    if (plot_type == "major_minor") {
      inc_type <- ifelse(g$x$data[[i]]$name == "TRUE", "Minor", "Major")
      text_rep <- text_rep %>%
        gsub("n:", paste(inc_type, "Incidents:"), .)
    } else {
      text_rep <- text_rep %>%
        gsub("n:", "Incidents:", .)
    }
    
    if (g$x$data[[i]]$name == "2019") {
      text_rep <- text_rep %>%
        gsub("\\d{4}", "2019", .)
    }
    
    g <- g %>%
      style(text = text_rep, traces=i)
  }
     
  g %>%
    config(modeBarButtonsToRemove = modeBarButtonsToRemove_time) %>%
    layout(legend = legend_layout_bottom) %>%
    style(hoverlabel = label_lightback, traces = traces_lightback) %>%
    style(hoverlabel = label_darkback, traces = traces_darkback) %>%
    style(hoverinfo = "none", traces = traces_to_hide)

}

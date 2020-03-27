#!/usr/local/bin/Rscript
# R script to be run multiple times daily via CRON to 
# query BPD crime incident reports
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Date: March 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)

# Function to get all currently available Boston Police incident data
get_all_incident_data <- function() {
  
  db_filename <- "all_bpd_incidents.rds"
  query_log_filename <- "query_log.rds"
  new_data_log_filename <- "new_data_log.txt"
  
  # Check how many incidents were present after the last query
  n_last_incidents <- readRDS(db_filename) %>%
    nrow()
  
  # Define query parameters
  base_url <- "https://data.boston.gov/api/3/action/"
  search_type <- "datastore_search"  # "datastore_search_sql"
  db_id <- "12cb3883-56f5-47de-afa5-3b1cf61b257b"
  
  n <- 0
  total_rows <- Inf
  df_all <- data.frame()
  while (n < total_rows) {
    request <- GET(url = paste(base_url, search_type, sep=""), 
                   query = list(
                     resource_id = db_id,
                     # filters = '{"YEAR": "2019"}',
                     limit=32000,
                     offset = n)
    )
    print(paste("Made a query to:", request$url))
    
    response_json <- fromJSON(content(request, as = "text", flatten = TRUE))
    
    print(paste("Success?", response_json$success))
    
    if (response_json$success == TRUE) {
      n <- n + 32000
      total_rows <- response_json$result$total
      
      df <- response_json$result$records
      df_all <- rbind(df_all, df)
    } else {
      stop('HTTP request failed.')
    }
  }
  
  # Make sure the number of incidents is either the same or greater than
  # the number upon the last query, or don't rewrite the DB
  if (n < n_last_incidents) {
    stop('Incidents have been removed?')
  }
  
  df_all <- df_all %>%
    mutate(OCCURRED_ON_DATE = ymd_hms(OCCURRED_ON_DATE))
  
  # Rewrite over the old DB to include latest incidents
  print("Updating incident database")
  saveRDS(df_all, db_filename)
  
  # Update log to reflect latest query
  print("Updating query log")
  query_history <- readRDS(query_log_filename) %>%
    rbind(data.frame(query_time=now('America/New_York'))) %>%
    saveRDS(query_log_filename)
  
  # Update log to reflect if there are new data
  if (n > n_last_incidents) {
    print("New data found. Updating new data log")
    line = paste(now('America/New_York'), n, "lines")
    write(line, file = new_data_log_filename, append=TRUE)
  }
}

get_all_incident_data()

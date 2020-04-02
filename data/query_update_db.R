#!/usr/local/bin/Rscript
# R script to update BPD crime incident reports database with web query
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: April 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dplyr)
library(stringr)
library(lubridate)
library(httr)
library(jsonlite)
library(readr)

# Define filenames and column data types
db_filename <- "all_bpd_incidents_cumulative.rds"
query_log_filename <- "query_log.rds"
new_data_log_filename <- "new_data_log.txt"

parse_query_db <- cols(
  STREET = col_character(),
  OFFENSE_DESCRIPTION = col_character(),
  SHOOTING = col_character(),
  OFFENSE_CODE = col_character(),
  DISTRICT = col_character(),
  REPORTING_AREA = col_double(),
  OCCURRED_ON_DATE = col_datetime(format = ""),
  DAY_OF_WEEK = col_character(),
  MONTH = col_double(),
  HOUR = col_double(),
  Long = col_double(),
  YEAR = col_double(),
  Lat = col_double(),
  INCIDENT_NUMBER = col_character(),
  `_id` = col_double(),
  OFFENSE_CODE_GROUP = col_character(),
  UCR_PART = col_character(),
  Location = col_character()
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Helper function: pipe-print
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pipe_print <- function(data, message = "", print_data = F) {
  print(message)
  if (print_data) {
    print(data)
  }
  data
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Function to get all currently available Boston Police incident data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
get_all_incident_data <- function() {
  
  # Check how many incidents were present after the last query
  n_last_incidents <- readLines(new_data_log_filename) %>%
    tail(1) %>%
    str_extract("(?<=\\d{2}\\s)\\d*(?=\\slines)") %>%
    as.numeric()
  
  # Define query parameters
  base_url <- "https://data.boston.gov/api/3/action/"
  search_type <- "datastore_search"  # "datastore_search_sql"
  db_id <- "12cb3883-56f5-47de-afa5-3b1cf61b257b"
  
  # Set query time
  query_time = now('America/New_York')
  
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
  
  n_new <- nrow(df_all)
  
  # Check if the number of incidents is either the same or greater than
  # the number upon the last query
  if (n_new < n_last_incidents) {
    warning(paste('Incidents have been removed? Now', 
                  n_new, ", previously", n_last_incidents))
  }
  
  df_all <- df_all %>%
    mutate(OCCURRED_ON_DATE = ymd_hms(OCCURRED_ON_DATE))
  
  
  # Update log to reflect latest query
  print("Updating query log")
  query_history <- readRDS(query_log_filename) %>%
    rbind(data.frame(query_time=query_time)) %>%
    saveRDS(query_log_filename)
  
  # Update log to reflect if there are new data
  print("Updating data length log")
  line = paste(query_time, n_new, "lines")
  write(line, file = new_data_log_filename, append=TRUE)
  
  return(list("query_time" = query_time, 
              "df_all" = df_all))
  
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Function to integrate new query into existing database
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
merge_into_previous <- function (old_df, new_df, datetime) {
  
  print(paste("Loaded old DF.", nrow(old_df), "incidents found."))
  print(paste("Loaded new DF.", nrow(new_df), "incidents found."))
  
  # Clean new DF to match columns of old DF
  df <- new_df %>% 
    mutate_at(vars(REPORTING_AREA, OFFENSE_CODE, YEAR, MONTH, HOUR, Long, Lat), 
              as.numeric) %>%
    mutate(queried = datetime) %>%
    select(-contains("_id", ignore.case = F), 
           -contains("full_text", ignore.case = F), 
           -OFFENSE_CODE_GROUP, -UCR_PART) %>%
    # Fix timezone of new data to be ET
    mutate(OCCURRED_ON_DATE = force_tz(OCCURRED_ON_DATE, tzone="America/New_York"))
  
  # Calculate how many incidents we expect to see
  old_incidents <- old_df %>% select(INCIDENT_NUMBER, OFFENSE_CODE) %>% unique()
  new_incidents <- df %>% select(INCIDENT_NUMBER, OFFENSE_CODE) %>% unique()
  correct_n_incidents <- rbind(old_incidents, new_incidents) %>%
    filter_all(all_vars(!is.na(.))) %>% # remove all-NA entries
    unique() %>% 
    nrow()
  print(paste("Expecting", correct_n_incidents, "unique entries"))
  
  # Try to create a correct database
  df_all_combined <- bind_rows(old_df, df) %>% # NOT THE SAME AS RBIND()
    # Make sure columns are right type
    pipe_print("Setting offense & reporting code to numeric") %>%
    mutate(OFFENSE_CODE = as.numeric(OFFENSE_CODE), 
           REPORTING_AREA = as.numeric(REPORTING_AREA)) %>%
    # Replace empty values with NA and remove trailing white space
    pipe_print("Cleaning up whitespace") %>%
    mutate_if(is.character, ~na_if(., ' '), ~na_if(., '')) %>%
    mutate_if(is.character, trimws) %>%
    # Replace blank locations with valid Lat/Long
    pipe_print("Replacing empty Lat/Long") %>%
    group_by_at(vars(-queried, -Location, -Long, -Lat, 
                     -OFFENSE_DESCRIPTION, -OFFENSE_CODE)) %>%
    mutate(Location = max(Location, 
                          na.rm=!all(is.na(Location))),
           Long = max(Long, 
                      na.rm=!all(is.na(Long))),
           Lat = max(Lat, 
                     na.rm=!all(is.na(Lat)))) %>%
    # Make sure the offense description matches the code
    pipe_print("Matching offense codes & descriptions") %>%
    group_by_at(vars(-queried, -Location, -Long, -Lat, 
                     -OFFENSE_DESCRIPTION)) %>%
    filter(OFFENSE_DESCRIPTION == max(OFFENSE_DESCRIPTION, 
                                      na.rm=!all(is.na(OFFENSE_DESCRIPTION)))) %>%
    ungroup() %>%
    distinct_at(vars(-queried), .keep_all=T)
  
  
  n_unique <- nrow(df_all_combined)
  
  print(paste("Done merging.", n_unique, "unique incidents found."))
  
  if (n_unique != correct_n_incidents) {
    warning(paste("It does not look like the dataset was reduced to the right number:", 
                  correct_n_incidents, "expected,", nrow(df_all_combined), "received.",
                  "This might mean columns besides location/offense have changed between queries."))
    if (n_unique > (correct_n_incidents * 1.1) | n_unique < (correct_n_incidents * 0.9)) {
      stop(paste("The final incident number differs from expectation by over 10%,",
                 "suggesting a fatal error. Aborting and not updating the database."))
    }
  }
  
  # Verify date shenanigans
  datetime_type_isPOSIX <- df_all_combined %>%
    pull(OCCURRED_ON_DATE) %>%
    head(1) %>%
    is("POSIXct")
  correct_date <- df_all_combined %>% 
    filter(INCIDENT_NUMBER == "I92097173") %>%
    pull(OCCURRED_ON_DATE) %>%
    hour() == 0
  
  if (!datetime_type_isPOSIX) {
    warning("Date is no longer POSIXct. Incorrect conversions could follow.")
  } else if (!correct_date) {
    warning("The date values have gotten fucked up again.")
  }
  
  return(df_all_combined)
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# if __name__ == "__main__":
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (!interactive()) {
  # Load existing DB
  old_df <- readRDS(db_filename)
  
  # Query DB for new entries
  query <- get_all_incident_data()
  datetime <- format(query$query_time, format="%Y%m%d_%H%M")
  
  # Add new query results into DB
  df_all_combined <- merge_into_previous(
    old_df, query$df_all, datetime
  )
  
  # Save out!
  saveRDS(df_all_combined, db_filename)
  print(paste("Saved updated database to", db_filename))
}

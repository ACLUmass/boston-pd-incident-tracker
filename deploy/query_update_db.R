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
library(aws.s3)
library(rvest)

# Initialization --------------------------------------------------------------

# Read environment vars for AWS keys
readRenviron("../.Renviron")

# Set up connection to S3 bucket
aws_s3_bucket <- get_bucket("app-bpd-incidents")

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

# Helper function: pipe-print -------------------------------------------------

pipe_print <- function(data, message = "", print_data = F) {
  print(message)
  if (print_data) {
    print(data)
  }
  data
}

# Function to fix known issues with offense descriptions & white space --------

fix_codes <- function(df) {
  df %>%
    # Fix typos in offense descriptions
    mutate(OFFENSE_DESCRIPTION = case_when(
      OFFENSE_DESCRIPTION == "ANNOYING AND ACCOSTIN" ~ "ANNOYING AND ACCOSTING",
      OFFENSE_DESCRIPTION == "DRUGS - POSS CLASS E INTENT TO MF DIST DISP" ~ "DRUGS - POSS CLASS E - INTENT TO MFR DIST DISP",
      T ~ OFFENSE_DESCRIPTION
    )) %>%
    # Fix descriptions with the wrong code
    mutate(OFFENSE_CODE = case_when(
      OFFENSE_DESCRIPTION == "DRUGS - POSS CLASS B - INTENT TO MFR DIST DISP" ~ 1843,
      OFFENSE_DESCRIPTION == "DRUGS - POSS CLASS E - INTENT TO MFR DIST DISP" ~ 1850,
      OFFENSE_DESCRIPTION == "VAL - OPERATING AFTER REV/SUSP." ~ 2907,
      T ~ OFFENSE_CODE
    ))%>%
    # Fix double white space & trailing white space
    mutate_if(is.character, str_squish)
}

# Function to get all currently available Boston Police incident data ---------

get_all_incident_data <- function(testing=F) {
  
  # Get data log from AWS
  save_object(new_data_log_filename, bucket = aws_s3_bucket, 
              file = new_data_log_filename)
  
  # Check how many incidents were present after the last query
  n_last_incidents <- readLines(new_data_log_filename) %>%
    tail(1) %>%
    str_extract("(?<=\\d{2}\\s)\\d*(?=\\slines)") %>%
    as.numeric()
  
  # Define Boston crime incidents page URL
  boston_crime_incidents_url <- "https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system"
  
  # Scrape download button link from page
  url_to_temp_csv <- xml2::read_html(boston_crime_incidents_url) %>%
    html_nodes(".resource-list") %>%
    html_nodes(".btn-group") %>% 
    `[[`(1) %>%
    html_nodes(".btn") %>%
    `[[`(2) %>%
    html_attr("href")
  
  # Set query time
  query_time = now('America/New_York')
  
  # Download CSV file as df
  df_all <- read_csv(url_to_temp_csv, 
                     col_types = parse_query_db)
  print(paste("Made a query to:", url_to_temp_csv))
  n_new <- nrow(df_all)
  
  # Check if the number of incidents is either the same or greater than
  # the number upon the last query
  if (n_new < n_last_incidents) {
    warning(paste('Incidents have been removed? Now', 
                  n_new, ", previously", n_last_incidents))
  }
  
  df_all <- df_all %>%
    mutate(OCCURRED_ON_DATE = ymd_hms(OCCURRED_ON_DATE))
  
  if (!testing) {
    
    # Update log to reflect latest query
    print("Updating query log")
    query_history <- readRDS(query_log_filename) %>%
      rbind(data.frame(query_time=query_time))
    # Save locally
    saveRDS(query_history, query_log_filename)
    # Save to AWS
    aws_log <- s3saveRDS(query_history, bucket = aws_s3_bucket, 
                         object = query_log_filename)
    if (aws_log) {
      print("Uploaded query log to AWS S3 bucket.")
    } else {
      warning("Failed to upload query log to AWS.")
    }
    
    # Update log to reflect if there are new data
    print("Updating data length log")
    line = paste(query_time, n_new, "lines")
    # Append to file locally
    write(line, file = new_data_log_filename, append=TRUE)
    # Save to AWS
    aws_length <- put_object(new_data_log_filename, bucket = aws_s3_bucket)
    if (aws_length) {
      print("Uploaded data length log to AWS S3 bucket.")
    } else {
      warning("Failed to upload data length log to AWS.")
    }
    
  }

  return(list("query_time" = query_time, 
              "df_all" = df_all))
  
}

# Function to integrate new query into existing database ----------------------

merge_into_previous <- function (old_df, new_df, queried_datetime) {
  
  print(paste("Loaded old DF.", nrow(old_df), "incidents found."))
  print(paste("Loaded new DF.", nrow(new_df), "incidents found."))
  
  # Clean old DF, just in case
  old_df <- old_df %>%
    fix_codes()
  
  # Clean new DF to match columns of old DF
  df <- new_df %>% 
    mutate_at(vars(REPORTING_AREA, OFFENSE_CODE, YEAR, MONTH, HOUR, Long, Lat), 
              as.numeric) %>%
    fix_codes() %>%
    mutate(queried = queried_datetime) %>%
    select(-contains("_id", ignore.case = F), 
           -contains("full_text", ignore.case = F), 
           -OFFENSE_CODE_GROUP, -UCR_PART) %>%
    # Fix timezone of new data to be ET
    mutate(OCCURRED_ON_DATE = force_tz(OCCURRED_ON_DATE, tzone="America/New_York"))
  
  # Calculate how many incidents we expect to see
  old_incidents <- old_df %>% select(INCIDENT_NUMBER, OFFENSE_DESCRIPTION) %>% unique()
  new_incidents <- df %>% select(INCIDENT_NUMBER, OFFENSE_DESCRIPTION) %>% unique()
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

# if __name__ == "__main__": --------------------------------------------------

if (!interactive()) {
  # Load existing DB from AWS
  old_df <- s3readRDS(db_filename, bucket = aws_s3_bucket)
  
  # Check that AWS DB matches local DB?
  old_df_local <- readRDS(db_filename)
  match_dbs <- identical(old_df, old_df_local)
  if (!match_dbs) {
    stop("AWS database does NOT match local database!")
  }
  
  # Query DB for new entries
  query <- get_all_incident_data()
  queried_datetime <- format(query$query_time, format="%Y%m%d_%H%M")
  
  # Add new query results into DB
  df_all_combined <- merge_into_previous(
    old_df, query$df_all, queried_datetime
  )
  
  # Save out locally!
  saveRDS(df_all_combined, db_filename)
  print(paste("Saved updated database to", db_filename))
  
  # Upload to AWS!!
  aws_df <- s3saveRDS(df_all_combined, bucket = aws_s3_bucket, 
                      object = db_filename)
  if (aws_df) {
    print("Uploaded updated database to AWS S3 bucket.")
  } else {
    warning("Failed to upload database to AWS.")
  }
  
}

# Report out memory usage
print(gc())

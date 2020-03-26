library(httr)
library(jsonlite)
library(lubridate)

# Function to get latest Boston Police incident datas
get_incident_data_since_feb <- function() {
  last_month_with_data <- 2
  
  # Check that year is still valid
  if (year(today()) != 2020) {
    stop("Won't work after 2020. Someone tell Lauren.")
  }
  
  months_since_last <- seq(last_month_with_data + 1, month(today()))
  
  filters <- paste0('{"YEAR": "2020", "MONTH": ["', paste(months_since_last, sep='", "'), '"]}')
  
  base_url <- "https://data.boston.gov/api/3/action/"
  search_type <- "datastore_search"  # "datastore_search_sql"
  db_id <- "12cb3883-56f5-47de-afa5-3b1cf61b257b"
  
  n <- 0
  total_rows <- Inf
  df_new <- data.frame()
  while (n < total_rows) {
    request <- GET(url = paste(base_url, search_type, sep=""), 
                   query = list(
                     resource_id = db_id,
                     filters = filters,
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
      df_new <- rbind(df_new, df)
    } else {
      stop('HTTP request failed.')
    }
  }
  
  df_new <- df_new %>%
    mutate(OCCURRED_ON_DATE = ymd_hms(OCCURRED_ON_DATE))
  
  return(df_new)
  
}

# Function to get all currently available Boston Police incident datas
get_all_incident_data <- function() {
  
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
  
  df_all <- df_all %>%
    mutate(OCCURRED_ON_DATE = ymd_hms(OCCURRED_ON_DATE))
  
  return(df_all)
  
}

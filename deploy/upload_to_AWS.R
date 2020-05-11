#!/usr/local/bin/Rscript
# R script to upload local files to BPD app AWS Bucket
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: May 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(aws.s3)

# Read environment vars for AWS keys
readRenviron("../.Renviron")

# Set up connection to S3 bucket
aws_s3_bucket <- get_bucket("app-bpd-incidents")

# Upload database to AWS
aws_df <- put_object(file="all_bpd_incidents_cumulative.rds",
           bucket = aws_s3_bucket)
if (aws_df) {
  print("Uploaded updated database to AWS S3 bucket.")
} else {
  warning("Failed to upload database to AWS.")
}

# Upload query log to AWS
aws_log <- put_object(file="query_log.rds",
                     bucket = aws_s3_bucket)
if (aws_log) {
  print("Uploaded updated query log to AWS S3 bucket.")
} else {
  warning("Failed to upload query log to AWS.")
}

# Upload data length log to AWS
aws_length <- put_object(file="new_data_log.txt",
                     bucket = aws_s3_bucket)
if (aws_length) {
  print("Uploaded updated data length log to AWS S3 bucket.")
} else {
  warning("Failed to upload data length log to AWS.")
}

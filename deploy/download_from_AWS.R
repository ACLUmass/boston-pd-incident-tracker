#!/usr/local/bin/Rscript
# R script to download files from BPD app AWS Bucket to local system
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: May 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(aws.s3)

# Read environment vars for AWS keys
readRenviron("../.Renviron")

# Set up connection to S3 bucket
aws_s3_bucket <- get_bucket("app-bpd-incidents")

# Download database from AWS
save_object("all_bpd_incidents_cumulative.rds",bucket = aws_s3_bucket)

# Download query log from AWS
save_object("query_log.rds", bucket = aws_s3_bucket)

# Download data length log from AWS
save_object("new_data_log.txt", bucket = aws_s3_bucket)

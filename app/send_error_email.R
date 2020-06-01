#!/usr/local/bin/Rscript
# R module to send email
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Author: Lauren Chambers
# Update Date: May 2020
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

library(gmailr)

options(gargle_quiet = F)

gm_auth_configure(path = "credentials.json")
gm_auth(email = "lauren.m.chambers96@gmail.com",
        cache = ".secret", scopes = c("send", "compose"))

send_email <- function() {

  gm_mime() %>%
    gm_to("lchambers@aclum.org") %>%
    gm_from("lauren.m.chambers96@gmail.com") %>%
    gm_subject("[R Shiny Error] BPD Incident Tracker") %>%
    gm_text_body(paste("An error has occured.", 
                       "\n\n\n", 
                       "https://www.shinyapps.io/admin/#/dashboard")) %>%
    # send it
    gm_send_message()
}

# Install required packages if not already installed
if (!require("gmailr")) install.packages("gmailr")
if (!require("logger")) install.packages("logger")

library(gmailr)
library(logger)

# Configure email settings
gm_auth_configure(path = "/path/to/your/credentials.json")  # Gmail OAuth credentials
NOTIFICATION_EMAIL <- "your.email@example.com"

# Configure logging
log_threshold(INFO)
log_appender(appender_file("/var/log/update_data.log"))

# Error handler function
handle_error <- function(e) {
    error_msg <- sprintf("Error in update_data.R: %s\n\nTimestamp: %s", 
                        as.character(e), 
                        format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    
    # Log error
    log_error(error_msg)
    
    # Send email notification
    email <- gm_mime() %>%
        gm_to(NOTIFICATION_EMAIL) %>%
        gm_from("your.service@gmail.com") %>%
        gm_subject("ERROR: update_data.R Script Failed") %>%
        gm_text_body(error_msg)
    
    gm_send_message(email)
}

# Wrap your main script in tryCatch
tryCatch({
    # Your existing update_data.R script goes here
    source("update_data.R")
    
    # Log success
    log_info("Script completed successfully")
}, error = handle_error)

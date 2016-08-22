library(httr)
library(purrr)
library(readr)
library(stringi)
library(urltools)

googlefinance_get_url_as_csv <- function(google_finance_url, cleanupFunction) {
  google_finance_response <- GET(google_finance_url)
  content(google_finance_response, as = "text") %>%
    strsplit(split = "\n") %>%
    unlist %>%
    cleanupFunction %>%
    read_csv(col_names = c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME"))
}

googlefinance_prepare_prices_result <- function(google_finance_response) {
  # get interval in seconds between each data point
  backfill_interval <-  (strsplit(google_finance_response[4], split = "=") %>% unlist)[2] %>% as.integer

  # update data rows
  google_finance_response[-c(1:6)] %>%
    update_timestamps(backfill_interval) %>%
    subset(!is.na(.)) %>%
    stri_c(collapse =  "\n")
}

update_timestamps <- function(data_lines, backfill_interval) {
  base_timestamp <- 0

  for (i in 1:length(data_lines)) {
    # does line contain a new timezone offset? if so we are ignoring this as POSIXct handles timezone conversions
    if (startsWith(data_lines[i],"TIMEZONE_OFFSET")) {
      data_lines[i] <- NA
    }
    else {
      line_as_vector <- strsplit(data_lines[i], split = ",") %>% unlist
      is_start_new_period <- startsWith(line_as_vector[1],"a")
      if (is_start_new_period) {
        base_timestamp <- substr(line_as_vector[1],2,nchar(line_as_vector[1])) %>% as.integer
        line_increment <- 0
      } else {
        line_increment <- (line_as_vector[1] %>% as.integer) * backfill_interval
      }
      timeStamp <- base_timestamp + line_increment
      timeStamp <- as.POSIXct(timeStamp,origin="1970-01-01 00:00.00 UTC", tz = "Europe/London")
      line_as_vector[1] <- as.character.Date(timeStamp)
      data_lines[i] <- paste(line_as_vector, collapse = ",")
    }
  }
  data_lines
}


exchange <- "NYSE"
stock <- "IBM"
intervalInSeconds <- 60
goBackPeriod <- "2d"
base_url <- "http://www.google.com/finance/getprices?x="
stock_url <- paste0(base_url, exchange, "&q=", stock, "&i=", intervalInSeconds, "&p=", goBackPeriod, "&f=d,o,h,l,c,v")
googlefinance_FTSE_PRICES <- stock_url %>% googlefinance_get_url_as_csv(googlefinance_prepare_prices_result)
library(jsonlite)
library(httr)
library(purrr)
library(readr)

GOOGLE_URL <- "http://www.google.com/finance/"

# Columns Friendly Names -----------------------------------------------------
COLUMN_FRIENDLY_NAMES <- new.env();
COLUMN_FRIENDLY_NAMES[["id"]] <- "ID"
COLUMN_FRIENDLY_NAMES[["t"]] <- "StockSymbol"
COLUMN_FRIENDLY_NAMES[["e"]] <- "Index"
COLUMN_FRIENDLY_NAMES[["l"]] <- "LastTradePrice"
COLUMN_FRIENDLY_NAMES[["l_cur"]] <- "LastTradeWithCurrency"
COLUMN_FRIENDLY_NAMES[["ltt"]] <- "LastTradeTime"
COLUMN_FRIENDLY_NAMES[["lt_dts"]] <- "LastTradeDateTime"
COLUMN_FRIENDLY_NAMES[["lt"]] <- "LastTradeDateTimeLong"
COLUMN_FRIENDLY_NAMES[["div"]] <- "Dividend"
COLUMN_FRIENDLY_NAMES[["yld"]] <- "Yield"
COLUMN_FRIENDLY_NAMES[["s"]] <- "LastTradeSize"
COLUMN_FRIENDLY_NAMES[["c"]] <- "Change"
COLUMN_FRIENDLY_NAMES[["cp"]] <- "ChangePercent"
COLUMN_FRIENDLY_NAMES[["el"]] <- "ExtraHrsLastTradePrice"
COLUMN_FRIENDLY_NAMES[["el_cur"]] <- "ExtraHrsLastTradeWithCurrency"
COLUMN_FRIENDLY_NAMES[["elt"]] <- "ExtraHrsLastTradeDateTimeLong"
COLUMN_FRIENDLY_NAMES[["ec"]] <- "ExtraHrsChange"
COLUMN_FRIENDLY_NAMES[["ecp"]] <- "ExtraHrsChangePercent"
COLUMN_FRIENDLY_NAMES[["pcls_fix"]] <- "PreviousClosePrice"


# Public functions ----

symbol_get_info <- function(exchange, stock) {
  paste0(GOOGLE_URL,"info?infotype=infoquoteall&q=") %>%
    paste0(exchange, ":", stock) %>%
    .getinfo_from_url %>%
    fromJSON %>%
    setNames(sapply(names(.),._get_friendly_name))
}

symbol_get_prices <- function(exchange, stock, intervalInSeconds, goBackPeriod) {
  paste0(GOOGLE_URL,"getprices?x=") %>%
    paste0(exchange, "&q=", stock, "&i=", intervalInSeconds, "&p=", goBackPeriod, "&f=d,o,h,l,c,v") %>%
    .getprices_from_url %>%
    ._prepare_prices_result %>%
    read_csv(col_names = c("DATE", "CLOSE", "HIGH", "LOW", "OPEN", "VOLUME"))

}

# URL functions ----

.getprices_from_url <- function(google_finance_url) {
  GET(google_finance_url) %>%
    content(as = "text") %>%
    strsplit(split = "\n") %>%
    unlist
}

.getinfo_from_url<- function(google_finance_url) {
  GET(google_finance_url) %>%
    content(as="text") %>% substring(4)
}

# Utility functions ----
._prepare_prices_result <- function(google_finance_response) {
  # get interval in seconds between each data point
  backfill_interval <-  (strsplit(google_finance_response[4], split = "=") %>% unlist)[2] %>% as.integer

  # update data rows
  google_finance_response[-c(1:6)] %>%
    .update_timestamps(backfill_interval) %>%
    subset(!is.na(.)) %>%
    stri_c(collapse =  "\n")
}

.update_timestamps <- function(data_lines, backfill_interval) {
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
      timeStamp <- as.POSIXct(timeStamp,origin="1970-01-01 00:00.00 UTC", tz = Sys.timezone())
      line_as_vector[1] <- as.character.Date(timeStamp)
      data_lines[i] <- paste(line_as_vector, collapse = ",")
    }
  }
  data_lines
}



._get_friendly_name <- function(name) {
  if(is.null(COLUMN_FRIENDLY_NAMES[[name]]))
    name
  else
    COLUMN_FRIENDLY_NAMES[[name]]
}

# FTSE values ----

# Load FTSE
FTSE <- symbol_get_info("INDEXFTSE", "UKX")
FTSE_PRICES <- symbol_get_prices("INDEXFTSE", "UKX", 60, "2d")

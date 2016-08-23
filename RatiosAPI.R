library(httr)
library(purrr)
library(readr)
library(stringi)
library(urltools)
library(plyr)
library(dplyr)

source("GoogleFinanceAPI.R")

# Determine what grouping to use based on length of interval in seconds
.group_by_vector <- function(interval) {
  if (interval < 60*60)
    list(~YEAR,~MONTH,~DAY, ~HOURS)
  else if (interval< 60*60*24)
    list(~YEAR,~MONTH,~DAY)
  else
    list(~YEAR,~MONTH)
}

symbol_get_efficiency_ratio <- function(prices, group_by_vector) {
  prices_aux <-
    # Break date into individual components
    mutate(prices,
           YEAR=format(DATE,"%Y"),
           MONTH=format(DATE,"%m"),
           DAY=format(DATE,"%e"),
           HOURS = format(DATE,"%I"),
           MINUTES = format(DATE,"%M"))

  prices_aux_open_values <-
    # Group and obtain OPEN value for the group
    prices_aux %>%
    group_by_(.dots = .group_by_vector(intervalInSeconds)) %>%
    filter(row_number() == 1) %>%
    select(OPEN) %>%
    ungroup

  prices_aux_close_values <-
    # Group and obtain CLOSE value for the group
    prices_aux %>%
    group_by_(.dots = .group_by_vector(intervalInSeconds)) %>%
    filter(row_number() == n()) %>%
    select(CLOSE) %>%
    ungroup

  prices_aux_sums <-
    # Group and obtain Cumulative sum of difference OPEN-CLOSE value for the group
    prices_aux %>%
    group_by_(.dots = .group_by_vector(intervalInSeconds)) %>%
    summarise(CUMDIFF=sum(abs(OPEN-CLOSE))) %>%
    ungroup

  prices_aux_open_values %>%
    join(prices_aux_close_values,type="inner") %>%
    join(ungroup(prices_aux_sums), type="inner") %>%
    mutate(ER=abs(OPEN-CLOSE) / ifelse(CUMDIFF==0,1.0,CUMDIFF)) %>%
    select(-CUMDIFF)
}



# FTSE values ----

# Load FTSE
#FTSE <- symbol_get_info("INDEXFTSE", "UKX")
intervalInSeconds <- 60*60*24
FTSE_PRICES <- symbol_get_prices("INDEXFTSE", "UKX", intervalInSeconds , "2Y")
FTSE_PRICES_ER <- symbol_get_efficiency_ratio(FTSE_PRICES, .group_by_vector(intervalInSeconds))
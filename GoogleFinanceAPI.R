# Here's an example URL to pull all historical data for GOOG at daily granularity:
# http://www.google.com/finance/getprices?q=GOOG&x=NASD&i=86400&p=40Y&f=d,c,v,k,o,h,l&df=cpct&auto=0&ei=Ef6XUYDfCqSTiAKEMg
# 
# q - Stock symbol
# x - Stock exchange symbol on which stock is traded (ex: NASD)
# i - Interval size in seconds (86400 = 1 day intervals)
# p - Period. (A number followed by a "d" or "Y", eg. Days or years. Ex: 40Y = 40 years.)
# f - What data do you want? d (date - timestamp/interval, c - close, v - volume, etc...) Note: Column order may not match what you specify here
# df - ??
# auto - ??
# ei - ??
# ts - Starting timestamp (Unix format). If blank, it uses today.



library(jsonlite)
library(httr)

# Format stock_url
base_url <- "http://www.google.com/finance/info?infotype=infoquoteall&q="
exchange <- "INDEXFTSE"
stock <- "UKX"

stock_url <- paste0(base_url, exchange, ":", stock)

stock_quotes_response <- GET(stock_url)


#quote_json_file <- file.path("Data",paste0(stock, ".json"))

#download.file(stock_url, quote_json_file)

stock_quotes_json <- rawToChar(content(stock_quotes_response,as = "raw")[-1][-1][-1])

quotes <- fromJSON(stock_quotes_json)

# display summary of quotes
hist(quotes)

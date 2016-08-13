# check https://rawgit.com/jennybc/googlesheets/master/vignettes/basic-usage.html for basic usage

library("googlesheets")
suppressPackageStartupMessages(library("dplyr"))
options(digits = 5)

ftse_sheet <- gs_title("FTSE Prices")

ftse_prices <- gs_read(ftse_sheet, ws = "FTSE 100")

historical <- gs_read(ftse_sheet, ws = "Historical", range = cell_cols(1:6))

hist(abs(historical$High - historical$Low), 
     breaks = nrow(historical),
     main = "High/Low daily difference",
     xlab = "Date",
     ylab = "Frequency",
     freq = TRUE)

ending_on_a_high <- (historical$High == historical$Close)
hist(as.numeric(ending_on_a_high))

ending_on_a_low <- (historical$Low == historical$Close)
hist(as.numeric(ending_on_a_low))

hist(abs(historical$High - historical$Open), 
     breaks = nrow(historical),
     main = "High/Open daily difference",
     xlab = "Date",
     ylab = "Frequency")
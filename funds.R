#load packages
library(magrittr)
library(readr)
library(dplyr)
library(readr)
library(quantmod)

#get data
#source: http://eoddata.com/stocklist/USMF/F.htm
setwd("~/Documents/funds")
raw <- read.table(file = './funds.txt', sep = '\t', header=TRUE, stringsAsFactors = FALSE) %>% 
  dplyr::filter(grepl(pattern = "Fidelity", x = Name)) %>%
  select(Code, Name)

#get prices
tickers <- raw$Code #[1:10]
#Loop through symbols, fetch prices, and store in myList
myList <-lapply(tickers, function(x) {
  result = tryCatch({
    l <- getSymbols(x, auto.assign = FALSE)
    data.frame(ticker = sub(pattern = '.Close', replacement =  '', x = names(l)[4])
               , dt = as.Date(index(l))
               , close = l[, grepl(pattern = '.*Close', x = names(l)), drop = TRUE])
  }, warning = function(w) {
    list()
  }, error = function(e) {
    list()
  }, finally = {
    list()
  })}
)
#reshape data in tabular form
df <- do.call(rbind, myList)
saveRDS(object = df, file = "~/Documents/funds/df.RDS")

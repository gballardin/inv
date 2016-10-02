#load required packages
library(stockPortfolio) # Base package for retrieving returns
library(ggplot2) # Used to graph efficient frontier
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP
library(magrittr)
library(readxl)
library(quantmod)
library(tidyr)

#source functions
setwd("~/Documents/funds")
source('00-utilities.R')

#get ticker data
# etf_base <- read_excel('Screener-Export Mutual Funds.xls', sheet = 'Current Criteria')
etf_base <- read_excel('Screener-Export ETF.xls', sheet = 'Current Criteria')
etf_base$`ETF Name` <- tolower(etf_base$`ETF Name`)
names(etf_base) <- tolower(names(etf_base))
names(etf_base) <- gsub(pattern = '\\s+', x = names(etf_base), replacement = '_')
etf_base <- dplyr::filter(etf_base, !grepl(pattern = '.*bond.*', x = tolower(etf_base$fund_group)))
etf_base <- dplyr::filter(etf_base, grepl('.*fidelity.*', etf_name)) #|anguard.*
#etf_base <- subset(etf_base, ticker != 'VHT')

#pull data from the Internet
Stocks <- lapply(etf_base$ticker, function(sym) {
  xts_out <- dailyReturn(na.omit(getSymbols(sym, auto.assign=FALSE)))
  colnames(xts_out) <- sym
  xts_out
})

df <- do.call(merge.xts,Stocks)
df <- cbind(date = index(df), as.data.frame(df))
date_threshold <- sapply(df, function(x) sum(is.na(x))) %>% quantile(prob = .9)
keep_ticker <- sapply(df, function(x) sum(is.na(x))) < date_threshold
df <- df[df$date>as.Date('2013-10-24'), keep_ticker]
df <- df[,sapply(df, function(x) (sum(is.na(x)) / length(x)) < .1)]
#filter out some tickers after first iterations (low return or bonds)
#df <- df[, names(df) %in% keepdf$ticker]

returns <- as.matrix(df[complete.cases(df),-1])

# Run the eff.frontier function based on no short and 50% alloc. restrictions
eff <- eff.frontier(returns=returns, short="no", max.allocation=.95,
                    risk.premium.up=1, risk.increment=.001)

# Find the optimal portfolio
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),] %>% 
  tidyr::gather(., key = ticker, value = value) %>% 
  dplyr::filter(!(ticker %in% c('Std.Dev', 'Exp.Return', 'sharpe'))) %>%
  dplyr::arrange(desc(value)) %>% 
  head(100)
eff.optimal.point$chosen <- eff.optimal.point$value>.01


#Plot returns
ftr_df <- tidyr::gather(df, key = ticker, value = value, 2:ncol(df))
ftr_df <- dplyr::inner_join(ftr_df, dplyr::select(etf_base, ticker, expense_ratio), by = 'ticker')
ftr_df <- dplyr::inner_join(ftr_df, dplyr::select(eff.optimal.point, ticker, chosen), by = 'ticker')
dplyr::group_by(ftr_df, ticker) %>%
  dplyr::summarise(mean_ret = mean(value, na.rm = TRUE)
                   , sd_ret = sd(value, na.rm = TRUE)
                   , mean_exp = mean(expense_ratio)
                   , chosen = factor(mean(chosen))
                                     ) %>%
  ggplot(aes(x=sd_ret, y=mean_ret, label = ticker, color = mean_exp, fill = mean_exp, shape = chosen, size =14)) + geom_point() +
  geom_text(check_overlap = TRUE)
head(dplyr::inner_join(eff.optimal.point, dplyr::select(etf_base, ticker, etf_name), by = 'ticker'), 40)
#summary
# keepdf <- dplyr::group_by(ftr_df, ticker) %>%
#   dplyr::filter(mean(value, na.rm = TRUE) > 2e-4 & sd(value, na.rm = TRUE) > 0.006) %>%
#   dplyr::select(ticker) %>%
#   unique

library(corrplot)
M <- cor(returns[,c(2,5,7,10)])
corrplot(M, method="circle")

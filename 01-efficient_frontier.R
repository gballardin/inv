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
etf_base <- etf_base[grepl(pattern = '.*fidelity.*', x = etf_base$`ETF Name`), ]


#pull data from the Internet
tickers <- etf_base$Ticker
Stocks <- lapply(tickers, function(sym) {
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
# nrow(df)
# sum(complete.cases(df))

returns <- as.matrix(df[complete.cases(df),-1])

#Plot returns
ftr_df <- tidyr::gather(df, key = ticker, value = value, 2:ncol(df))
dplyr::group_by(ftr_df, ticker) %>%
  dplyr::summarise(mean_ret = mean(value, na.rm = TRUE), sd_ret = sd(value, na.rm = TRUE)) %>% 
  ggplot(aes(x=sd_ret, y=mean_ret, label = ticker)) + geom_point() +
  geom_text(check_overlap = TRUE)

# Run the eff.frontier function based on no short and 50% alloc. restrictions
eff <- eff.frontier(returns=returns, short="no", max.allocation=.5,
                    risk.premium.up=1, risk.increment=.001)

# Find the optimal portfolio
eff.optimal.point <- eff[eff$sharpe==max(eff$sharpe),]
eff.optimal.point.trans <- t(eff.optimal.point)
eff.optimal.point.trans[eff.optimal.point.trans>.01]

# graph efficient frontier
# Start with color scheme
ealred <- "#7D110C"
ealtan <- "#CDC4B6"
eallighttan <- "#F7F6F0"
ealdark <- "#423C30"

ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_point(alpha=.1, color=ealdark) +
  geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
             color=ealred, size=5) +
  annotate(geom="text", x=eff.optimal.point$Std.Dev,
           y=eff.optimal.point$Exp.Return,
           label=paste("Risk: ",
                       round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
                       round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                       round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
           hjust=0, vjust=1.2) +
  ggtitle("Efficient Frontier\nand Optimal Portfolio") +
  labs(x="Risk (standard deviation of portfolio)", y="Return") +
  theme(panel.background=element_rect(fill=eallighttan),
        text=element_text(color=ealdark),
        plot.title=element_text(size=24, color=ealred))
#ggsave("Efficient Frontier.png")
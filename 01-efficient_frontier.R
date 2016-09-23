#load required packages
library(stockPortfolio) # Base package for retrieving returns
library(ggplot2) # Used to graph efficient frontier
library(reshape2) # Used to melt the data
library(quadprog) #Needed for solve.QP
library(readxl)

#source functions
setwd("~/Documents/funds")
source('00-utilities.R')

#get ticker data
etf_base <- read_excel('Screener-Export Mutual Funds.xls', sheet = 'Current Criteria')

#pull data from the Internet
tickers <- etf_base$Symbol[1:1e2]
Stocks <- lapply(tickers, function(sym) {
  weeklyReturn(na.omit(getSymbols(sym, auto.assign=FALSE)))
  
})
df <- do.call(merge.xts,Stocks)
colnames(df) <- tickers
df <- cbind(date = index(df), as.data.frame(df))
date_threshold <- sapply(df, function(x) sum(is.na(x))) %>% quantile(prob = .1)
df <- df[1:(date_threshold+1), ]
returns <- as.matrix(df[complete.cases(df),-1])

#remove stocks with short lifespan
ftr_df <- group_by(df, ticker) %>% filter(n()>2000)
ftr_df <- droplevels(ftr_df)
table(table(ftr_df$ticker))

#Plot returns
group_by(ftr_df, ticker) %>%
  #filter(ticker %in% stocks) %>%
  mutate(return = log(close) - lag(log(close))) %>%
  summarise(mean_ret = mean(return, na.rm = TRUE), sd_ret = sd(return, na.rm = TRUE)) %>% 
  ggplot(aes(x=sd_ret, y=mean_ret, label = ticker)) + geom_point()

+ geom_text(check_overlap = TRUE)



# Run the eff.frontier function based on no short and 50% alloc. restrictions
eff <- eff.frontier(returns=returns, short="no", max.allocation=.2,
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
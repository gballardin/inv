#source functions
setwd("~/Documents/inv")
source('00-required_packages.R')
source('01-functions.R')

#get ticker data
# etf_base <- read_excel('Screener-Export Mutual Funds.xls', sheet = 'Current Criteria')
etf_base <- get_all_tickers_names(spreadsheet_path = 'Screener-Export ETF.xls')

etf_base <- dplyr::filter(etf_base, !grepl(pattern = '.*bond.*', x = tolower(etf_base$fund_group)))
etf_base <- dplyr::filter(etf_base, grepl('.*fidelity.*|.*anguard.*', etf_name)) #|.*anguard.*
#etf_base <- subset(etf_base, ticker %in% c("FTEC","VGT","VTI","VOT","VBK","VTWO","VCR","IVOO","MGV","VYM","VIG","VGK","VTWG","VAW","VOX","IVOV","VONE","VSS","VDE","VNQ","VT","VUG","VTWV","VIOO","VIS","FCOM","VOE","VONG","VNQI","VIOG","VDC","FIDU","VXUS","FDIS","VIOV","VHT","FMAT"))
#etf_base <- subset(etf_base, ticker != 'VPU' & ticker != 'FNCL')

#pull data from the Internet
Stocks <- lapply(c(etf_base$ticker), function(sym) {
  xts_out <- dailyReturn(na.omit(getSymbols(sym, auto.assign=FALSE, src = 'google')))
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
eff <- eff.frontier(returns=returns, short="no", max.allocation=.35,
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
g <- dplyr::group_by(ftr_df, ticker) %>%
  dplyr::summarise(mean_ret = mean(value, na.rm = TRUE)
                   , sd_ret = sd(value, na.rm = TRUE)
                   , mean_exp = mean(expense_ratio)
                   , chosen = factor(mean(chosen))
                                     ) %>%
  ggplot(aes(x=sd_ret, y=mean_ret, label = ticker, color = mean_exp, fill = mean_exp, shape = chosen, size =14)) + geom_point() +
  geom_text(check_overlap = TRUE)
ggplotly(g)
head(dplyr::inner_join(eff.optimal.point, dplyr::select(etf_base, ticker, etf_name), by = 'ticker'), 40)
#summary
# keepdf <- dplyr::group_by(ftr_df, ticker) %>%
#   dplyr::filter(mean(value, na.rm = TRUE) > 2e-4 & sd(value, na.rm = TRUE) > 0.006) %>%
#   dplyr::select(ticker) %>%
#   unique

library(corrplot)
M <- cor(returns[,c(2,5,7,10,11)])
corrplot(M, method="circle")

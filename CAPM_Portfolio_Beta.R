library(BatchGetSymbols)
library(portfolioBacktest)
library(qrmtools)
library(tidyverse)
library(tidyquant)
library(quantmod)
library(xts)
library(ggfortify)
library(kableExtra)
library(PerformanceAnalytics)
library(pastecs)
library(broom)
library(readxl)
library(lubridate)

first.date <- Sys.Date()-3650
last.date <- Sys.Date()
bench_ticker <- '^GSPC' 
Get

df.SP500 <- GetSP500Stocks()
tickers <- df.SP500$Tickers
tickers16=tickers[16:20]
stocks=BatchGetSymbols(tickers = tickers16,first.date = first.date, last.date = last.date, 
                       bench.ticker = bench_ticker,freq.data = "monthly")
df_sp500 <- BatchGetSymbols(tickers =  '^GSPC', 
                            first.date = first.date, 
                            last.date = last.date,freq.data = "monthly")[[2]]

stocks_df=fortify(stocks$df.tickers)
stocks_df= na.omit(stocks_df)
stocks_df=stocks_df[c("ref.date","ticker","ret.adjusted.prices")]
stock_wide=pivot_wider(stocks_df, names_from = ticker, values_from = ret.adjusted.prices)
stock_wide=na.omit(stock_wide)
date_ydm= as.Date(stock_wide$ref.date)
stock_xts=xts(stock_wide, order.by = date_ydm)
stock_xts$ref.date=NULL
storage.mode(stock_xts)="numeric"

n=ncol(stock_xts)


portfolio_return1<-Return.portfolio(R= stock_xts,weights =  rep(1 / n, n)) 
names(portfolio_return1)= "Portfolio return 1"

portfolio_return2<-Return.portfolio(R= stock_xts,weights =  c(0.1,0.2,0.3,0.3,0.1)) 
names(portfolio_return2)= "Portfolio return 2"

portfolio_return3<-Return.portfolio(R= stock_xts,weights =  c(0.1,0.3,0.2,0.1,0.3)) 
names(portfolio_return3)= "Portfolio return 3"

portfolio_return4<-Return.portfolio(R= stock_xts,weights =  c(0.1,0.15,0.2,0.25,0.3)) 
names(portfolio_return4)= "Portfolio return 4"

portfolio_return5<-Return.portfolio(R= stock_xts,weights =  c(0.05,0.2,0.5,0.2,0.05)) 
names(portfolio_return5)= "Portfolio return 5"

comp<- merge.xts(portfolio_return1,portfolio_return2,portfolio_return3,portfolio_return4,portfolio_return5)

autoplot(comp)
stat.desc(comp)

#-	Estimate the CAPM model using 5 single series and 5 portfolio returns constructed above and plot.
df_sp500=df_sp500[c("ticker","ref.date","ret.adjusted.prices")]
df_sp500=df_sp500[29:121,]
df_sp500_wide=pivot_wider(df_sp500,names_from = ticker,values_from = ret.adjusted.prices)
df_sp500_xts=xts(df_sp500_wide,order.by = date_ydm)
df_sp500_xts$ref.date=NULL
storage.mode(df_sp500_xts)="numeric"
comp1=merge.xts(comp,df_sp500_xts)
comp2=merge.xts(stock_xts,df_sp500_xts)
names(comp1)=c("port1","port2","port3","port4","port5","ret_mkt")
number=c(1,2,3,4,5,6)
storage=list()
comp3=merge.xts(comp,comp2)
#Estimate Beta to stock_wide and port_wide df
for(i in names(comp3)){
  storage[[i]] <- lm(get(i) ~ X.GSPC, comp3)
  coef_beta[[i]]=coef(storage[[i]])[2]
  
  
}
coef_tib=tibble(coef_beta)

summary(storage[["port2"]])$coefficients[2,1]
a=lm(data = comp1, formula =port1 ~ ret_mkt)
b=lm(data = comp1, formula =port2 ~ ret_mkt)
c=lm(data = comp1, formula =port3 ~ ret_mkt)
d=lm(data = comp1, formula =port4 ~ ret_mkt)
e=lm(data = comp1, formula =port5 ~ ret_mkt)
f=lm(data = comp2, formula =ALB ~ X.GSPC)
g=lm(data = comp2, formula =ALGN ~ X.GSPC)
h=lm(data = comp2, formula =ALLE ~ X.GSPC)
i=lm(data = comp2, formula =ARE ~ X.GSPC)
j=lm(data = comp2, formula =LNT ~ X.GSPC)


a=print(coef(a)[2])
b=print(coef(b)[2])
c=print(coef(c)[2])
d=print(coef(d)[2])
e=print(coef(e)[2])
f=print(coef(f)[2])
g=print(coef(g)[2])
h=print(coef(h)[2])
i=print(coef(i)[2])
j=print(coef(j)[2])

my_beta=c(a,b,c,d,e,f,g,h,i,j)
my_beta=as.array(my_beta)
my_beta=as.data.frame.table(my_beta)
my_beta$Var1=c("port1","port2","port3","port4","port5","ALB","ALGN","ALLE","ARE","LNT")



p <- ggplot(my_beta, aes(x = Freq,fill=Var1)) + 
  geom_histogram(bins = 10,binwidth = 0.2)

p



#Estimate the CAPM model for all stocks in Nasdaq.
#nasdaq=tq_exchange("NASDAQ")
#ticker=nasdaq$tickers
nas=read.csv("C:\\Users\\Admin\\Desktop\\data\\nasdaq-listed-symbols_csv.csv")
ticker_nas=nas$Symbol
first_date=Sys.Date()-365*5
last_date=Sys.Date()



df_stocks_nasdaq <- BatchGetSymbols(tickers = ticker_nas, 
                                    first.date = first_date, 
                                    last.date = last_date,
                                    bench.ticker = bench_ticker,
                                    thresh.bad.data = thresh_bad_data,
                                    freq.data = 'monthly')[[2]]



df_nasdaq <- BatchGetSymbols(tickers =  '^NDX', 
                             first.date = first_date, 
                             last.date = last_date,
                             freq.data = 'monthly')[[2]]

idx <- match(df_stocks_nasdaq$ref.date, df_nasdaq$ref.date)
df_stocks_nasdaq$ret_mkt <- df_nasdaq$ret.adjusted.prices[idx]

estimate_beta <- function(df) {
  
  my_model <- lm(data = df, 
                 formula = ret.adjusted.prices ~ ret_mkt)
  
  return(coef(my_model)[2])
}

#Plot Nasdaq beta
my_betas2 <- by(data = df_stocks_nasdaq, 
               INDICES = df_stocks_nasdaq$ticker, 
               FUN = estimate_beta)
subz=subset(da_beta2, my_betas2 >0 & my_betas2 <= 2)
ap <- ggplot(subz, aes(x = Freq)) + 
  geom_histogram(bins = 20)
print(ap)

#portfolio beta 
#portfolio beta is equal the weighted-average of the beta of all individual stocks in portfolio

#port 1 with 0<beta<0.4 stocks
da_beta2= as.data.frame.table(my_betas2)
sub1=subset(da_beta2, my_betas2 >0 & my_betas2 <= 0.4)
port1=sum(sub1$Freq)/234

#port2
sub2=subset(da_beta2, my_betas2 >0.4 & my_betas2 <= 0.7)
port2=sum(sub2$Freq)/355

#port3
sub3=subset(da_beta2, my_betas2 >0.7 & my_betas2 <= 1)
port3=sum(sub3$Freq)/393

#port4
sub4=subset(da_beta2, my_betas2 >1 & my_betas2 <= 1.3)
port4=sum(sub4$Freq)/278

#port5
sub5=subset(da_beta2, my_betas2 >= 1.3)
port5=sum(sub5$Freq)/341




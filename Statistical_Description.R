install.packages("kableExtra", dependencies = TRUE)
install.packages("pastecs")
install.packages("xfun")
library(kableExtra)
library(quantmod)
library(dplyr)
library(qrmtools)
library(qrmdata)
library(fredr)
library(ggfortify)
library(tidyverse)
library(xts)
library(broom)
library(PerformanceAnalytics)
library(dygraphs)
library(pastecs)
library(pdfetch)
#1) import, clean data and plot

#a) import data
symbols<- c("FB", "AMZN", "NFLX", "MSFT", "^IXIC")
tidyquant::tq_get(x = symbols, from = "2017-01-01", to = "2021-08-10") -> data_raw
data_raw=data_raw[c("symbol","date","adjusted")]

#b)clean data
pivot_wider(data_raw,names_from = symbol, values_from = adjusted) -> data_wide_price
date_ymd<- as.Date(data_wide_price$date)
stock_indice<-data_wide_price %>% select(symbols[1:5])

#c) transform into timeseries
xts(stock_indice, order.by = date_ymd)-> stock_xts
names(stock_xts) = c("FB", "AMZN", "NFLX", "MSFT", "NASDAQ")
index(stock_xts) = as.Date(index(stock_xts))

##C? th??? t?m l???i c?c bu???c t??? a) d???n c) b???ng c?u l???nh sau:
symbols<- c("FB", "AMZN", "NFLX", "MSFT", "^IXIC")
stock<-pdfetch_YAHOO(symbols, fields = "adjclose", from= Sys.Date()-365*2, to=Sys.Date())
names(stock)=c("FB", "AMZN", "NFLX", "MSFT", "NASDAQ")
stock_indice= stock$FB

#d) Plot the price
stocks_series2 = tidy(stock_xts) %>%
ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "Top Four US Tech Comany and NASDAQ ",subtitle = "End of Day Adjusted Prices",caption = " Source: Yahoo Finance") +xlab("Date") + ylab("Price") +scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange","Yellow"))
stocks_series2

#e)Plot the return
stock_xts_return=returns(stock_xts, method = "simple")
stocks_series3=tidy(stock_xts_return) %>%
ggplot(aes(x=index,y=value, color=series))+geom_line()+facet_grid(series~., scales = "free")+labs(title="TOP 4 stock and NASDAQ return", subtitle = "End of day adjusted", caption = "Source:YAHOO Finance")+xlab("date")+ylab("return")+ scale_color_manual(values =c("Red", "Black", "DarkBlue","Orange","Yellow") )
stocks_series3
 summary(stock_xts)
#2) summary statistics
df=fortify(stock)
df=df[, 2:6]

#a) statistics of price
summary(stock_indice)
kbl(x = summary(df), caption = "Table 1: Statistics of price", escape = TRUE) %>%
kable_classic(full_width = TRUE, html_font = "Source Sans Pro")

#b) statistics of returns
r=returns(stock_xts)
kbl(x=summary(r), caption = "Table 2: Statistics of return",escape = TRUE) %>%
kable_classic(full_width=TRUE,html_font = "Source Sans Pro")

#3) Compute portfolio

#Slice 4 column of 4 stocks in stock_xts_return 
stock_return<- stock_xts_return[, 1:4] 

#create a weighted portfolio and calculate portfolio return
portfolio_return2<-Return.portfolio(R= stock_return,weights = c(0.7,0.1,0.15,0.05)) 
names(portfolio_return2)= "Portfolio return 2"

#create an equally weighted portfolio and calculate portfolio return
portfolio_return1<-Return.portfolio(R= stock_return,weights = c(0.25,0.25,0.25,0.25))
names(portfolio_return1)="Portfolio return 1"

#Merge the two portfolio
comp<- merge.xts(portfolio_return2,portfolio_return1)

#plot the return
autoplot(comp)+labs(title = "Compare 2 portfolio" )+ xlab("Date")+ylab("Return")

#show statitics of the 2 portfolio
stat.desc(comp)

#tinytex::install_tinytex()
options(warn=-1)
library(qrmtools)
library(tidyverse)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(broom)
library(readxl)
library(stargazer)
library(pastecs)
library(tseries)
library(vars)
library(strucchange)
library(sarbcurrent)
library(reshape2)

hhh=ur.df(daily_return$UK, type="trend", selectlags = "BIC")
plot(hhh)
summary(hhh)
index_daily=read_xlsx("C:/Users/Admin/Desktop/indexdaily.xlsx",col_names = TRUE)
index_daily=na.omit(index_daily)
index_daily$Date=as.Date(index_daily$Date)
stargazer(stat.desc(index_daily[, 2:7]),type = "text",title = "Daily data",summary = TRUE,rownames = FALSE)

index_monthly=read_xlsx("C:/Users/Admin/Desktop/indexmonthly.xlsx",col_names = TRUE)
index_monthly=na.omit(index_monthly)
stargazer(stat.desc(index_monthly),type = "text",title = "Monthly data",summary = FALSE,rownames = TRUE)

#Calculate return and plot
#index_daily=mutate(index_daily, Date=as.Date(Date, format="%b %d, %Y),.keep="unused,.before=1) change from character date to numeric date
#index_daily=mutate(index_daily, Date=as.Date(Date))
#plot daily return
date_ydm=as.Date(index_daily$Date)
daily_xts=xts(index_daily,order.by = date_ydm)
storage.mode(daily_xts)="numeric"
daily_return=returns(daily_xts,method = c("diff"))
daily_return$Date=NULL
autoplot(daily_xts)#plot daily price
daily_return_plot = tidy(daily_return) %>%
  ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "6 Advanced country ",subtitle = "End of Day Daily data",caption = " Source: MSCI") +xlab("Date") + ylab("Price") +scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange","Yellow","Pink"))
daily_return_plot
stat.desc(daily_return)#summary statistics

df=melt(index_daily,id="Date")
ggplot(data=df, aes(x=Date,y=value))

#plot monthly return
date_ydm2=as.Date(index_monthly$Date)
monthly_xts=xts(index_monthly,order.by = date_ydm2)
monthly_xts$Date=NULL
storage.mode(monthly_xts)="numeric"
monthly_return=returns(monthly_xts,method = "diff")
autoplot(monthly_xts)#plot monthly price
monthly_return_plot = tidy(monthly_return) %>%
  ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "6 Advanced Country ",subtitle = "End of Day Monthy Data",caption = " Source: MSCI") +xlab("Date") + ylab("Monthly return") +scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange","Yellow","Pink"))
monthly_return_plot
stat.desc(monthly_return)#summary statistics

#Normality for monthly return
daily_xts$Date=NULL
norm_daily_price=apply(daily_xts, 2, jarque.bera.test)
norm_daily_return=apply(daily_return, 2, jarque.bera.test)
norm_monthly_price=apply(monthly_xts, 2, jarque.bera.test)
norm_monthly_return=apply(monthly_return, 2, jarque.bera.test)

norm_daily_price_p=sapply(norm_daily_price,  '[[' , "p.value")
norm_daily_return_p=sapply(norm_daily_return,  '[[' , "p.value")
norm_monthly_price_p=sapply(norm_monthly_price,  '[[' , "p.value")
norm_monthly_return_p=sapply(norm_monthly_return,  '[[' , "p.value")
##Comment: reject all the NULL hypothesis-each series has a normal distribution for all kind of data

#Ljung-box test
LB_daily_price=apply(daily_xts, 2,Box.test, lag=10, type="Ljung-Box")
LB_daily_price_pvalue=sapply(LB_daily_price,  '[[' , "p.value")
a=as.data.frame(LB_daily_price_pvalue)
## LB test on daily price of 6 countries show significant autocorrelation at 5%

LB_daily_return= apply(daily_return, 2,Box.test, lag=10, type="Ljung-Box")
LB_daily_return_pvalue<- sapply(LB_daily_return,   `[[`, "p.value")
b=as.data.frame(LB_daily_return_pvalue)
acf(daily_return$UK)
## Although I have take the first differnce but LB test show that 
## there is some significant autocorrelation between series. 
## I have also plotted the acf of daily return 
## but it also show autocorelation within the series.
## Turn out that this has nothing to do with stationarity. 
## The p-value is smaller than 5% so i conclude that the daily_return dataset has autocorrelation
## The sample is quite large, correlation tend to 1, 
## hence, the test statistic will exceed the chi-square critical value
## That's why p-value is near zero and we reject the Null.


LB_monthly_price=apply(monthly_xts, 2,Box.test, lag=9, type="Ljung-Box")
LB_monthly_price_pvalue=sapply(LB_monthly_price,  '[[' , "p.value")
C=as.data.frame(LB_monthly_price_pvalue)
acf(monthly_return$GERMANY)
##LB test on monthly price of 6 countries show significant autocorrelation at 5%

LB_monthly_return= apply(monthly_return, 2,Box.test, lag=10, type="Ljung-Box")
LB_monthly_return_pvalue<- sapply(LB_monthly_return,   `[[`, "p.value")
d=as.data.frame(LB_monthly_return_pvalue)
acf(monthly_return$GERMANY)
##LB test on monthly return show no significant autocorrelation at 5% within each countries series

#Stationary test using KPSS
daily_price_kpsss= apply(daily_xts, 2, ur.kpss)
daily_price_kpsss
daily_return_kpss=apply(daily_return, 2, ur.kpss)
daily_return_kpss
monthly_price_kpss=apply(monthly_xts,2, ur.kpss)
monthly_price_kpss
monthly_return_kpss=apply(monthly_return, 2, ur.kpss)
monthly_return_kpss

## Each countries in 2 data set (daily price and monthly price) 
##has a critical value that greater than the 5% LM statistics
## So each series in the 3 data set is non-stationary at the level of 5%
## Each series in monthly return and daily return has a critical value less than critical value of 5%
## =>Can't reject the null=>stationary
autoplot(monthly_return)

#Structural break
daily_xts$Date=NULL
times=c(1:1044)

#breakpoint in daily price data
#UK_daily_price= breakpoints(daily_xts$UK ~ times, h=150)
#UK_daily_price$breakpoints
#CAD_daily_price= breakpoints(daily_xts$CANADA ~ times, h=150)
#CAD_daily_price$breakpoints
#FRA_daily_price= breakpoints(daily_xts$FRANCE ~ times, h=150 )
#FRA_daily_price$breakpoints
#GER_daily_price= breakpoints(daily_xts$GERMANY ~ times, h=150)
#GER_daily_price$breakpoints
#JAP_daily_price= breakpoints(daily_xts$JAPAN ~ times, h=150)
#JAP_daily_price$breakpoints  
#USA_daily_price= breakpoints(daily_xts$USA ~ times, h=150)
#USA_daily_price$breakpoints
##I have found many breakpoint in all series. Most of the break point around in the early of 2020 and 2018
## In 2018, the bond yield is inverted so many investors were afraid of a recession
## and they dumped away their stock and price drop immediately
## In early 2020, we had COVID outbreak and investors again dumped their stock.
##I have to markdown the code because my computer is not strong enough to compile the code

#breakpoint in daily return data
time2=c(1:1043)
#UK_return=breakpoints(daily_return$UK ~ time2, h=27 )
#UK_return$breakpoints
#CAD_return=breakpoints(daily_return$CANADA ~ time2, h=27 )
#CAD_return$breakpoints
#FRA_return=breakpoints(daily_return$FRANCE ~ time2, h=27 )
#FRA_return$breakpoints
#GER_return=breakpoints(daily_return$GERMANY ~ time2, h=27 )
#GER_return$breakpoints
#JAP_return=breakpoints(daily_return$JAPAN ~ time2, h=27 )
#JAP_return$breakpoints
#USA_return=breakpoints(daily_return$USA ~ time2, h=27 )
#USA_return$breakpoints
##Comment: I can't find any breakpoint due to the volatility clustering in daily return. 
##I have to markdown the code because my computer is not strong enough to compile the code


#breakpoint in monthly price
time4=c(1:180)
UK_monthly=breakpoints(monthly_xts$UK ~ time4, h=27)
UK_monthly$breakpoints
CAD_monthly=breakpoints(monthly_xts$CANADA ~ time4, h=27)
CAD_monthly$breakpoints
FRA_monthly=breakpoints(monthly_xts$FRANCE ~ time4, h=27)
FRA_monthly$breakpoints
GER_monthly=breakpoints(monthly_xts$GERMANY ~ time4, h=27)
GER_monthly$breakpoints
JAP_monthly=breakpoints(monthly_xts$JAPAN ~ time4, h=27)
JAP_monthly$breakpoints
USA_monthly=breakpoints(monthly_xts$USA ~ time4, h=27)
USA_monthly$breakpoints
##comment: monthly price also has many breakpoint but they spread along the time period 
##so this won't cause volatility in monthly return

#Breakpoint
time3=c(1:179)
UK_return_monthly=breakpoints(monthly_return$UK ~ time3, h=26)
UK_return_monthly$breakpoints
CAD_return_monthly=breakpoints(monthly_return$CANADA ~ time3, h=26)
CAD_return_monthly$breakpoints
FRA_return_monthly=breakpoints(monthly_return$FRANCE ~ time3, h=26)
FRA_return_monthly$breakpoints
GER_return_monthly=breakpoints(monthly_return$GERMANY ~ time3, h=26)
GER_return_monthly$breakpoints
JAP_return_monthly=breakpoints(monthly_return$JAPAN ~ time3, h=26)
JAP_return_monthly$breakpoints
USA_return_monthly=breakpoints(monthly_return$USA ~ time3, h=26)
USA_return_monthly$breakpoints
##comment: I only found a breakpoint in UK monthly return. 
## Because we take the monthly data so the effect of 2008 crisis, 
## 2020 COVID outbreak is reduced (it spread for all month )

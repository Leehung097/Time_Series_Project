rm(list = ls())
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
library(reshape2)
library(urca)
library(forecast)
library(tsDyn)
yearly_index= read_xlsx("C:/Users/Admin/Desktop/indexyearly.xlsx",col_names = TRUE)
yearly_index$Date=as.Date(yearly_index$Date)
yearly_index=na.omit(yearly_index)
monthly_index= read_xlsx("C:/Users/Admin/Desktop/indexmonthly2.xlsx",col_names = TRUE)
monthly_index$Date=as.Date(monthly_index$Date)


#Plot the return and price of each dataset

#Change to time series
date_ydm1= as.Date(yearly_index$Date)
date_ydm2= as.Date(monthly_index$Date)
yearly_xts= xts(yearly_index,order.by = date_ydm1)
monthly_xts=xts(monthly_index, order.by = date_ydm2)


#Rename 
names(yearly_index)=c("date","GER","FRA","UK","CAD","USA","JAP")
names(yearly_xts)=c("date","GER","FRA","UK","CAD","USA","JAP")
names(monthly_index)=c("date","GER","FRA","UK","CAD","USA","JAP")
names(monthly_xts)=c("date","GER","FRA","UK","CAD","USA","JAP")

#Calculate yearly return
storage.mode(yearly_xts)="numeric"
yearly_return=returns(yearly_xts, method="diff")
yearly_return$date=NULL

#Calculate monthly return
storage.mode(monthly_xts)="numeric"
monthly_return=returns(monthly_xts, method="diff")
monthly_return$date=NULL

#Plot yearly price
df1=melt(yearly_index,id = "date")
names(df1)=c("date","Countries","Price")
ggplot(data = df1, aes(x=date, y=Price)) + geom_line(aes(colour=Countries))+labs(title = "6 Advanced country ",subtitle = "End of year",caption = " Source: MSCI")

#Plot monthly price
df2=melt(monthly_index, id="date")
names(df2)=c("date","Countries","Price")
ggplot(data = df2, aes(x=date, y=Price)) + geom_line(aes(colour=Countries))+labs(title = "6 Advanced country ",subtitle = "End of month",caption = " Source: MSCI")

#Plot yearly return
yearly_return_plot = tidy(yearly_return) %>%
  ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "6 Advanced country ",subtitle = "End of year",caption = " Source: MSCI") +xlab("Date") + ylab("Return") +scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange","Yellow","Pink"))
yearly_return_plot

#plot monthly return
monthly_return_plot = tidy(monthly_return) %>%
  ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "6 Advanced Country ",subtitle = "End of month",caption = " Source: MSCI") +xlab("Date") + ylab("Monthly return") +scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange","Yellow","Pink"))
monthly_return_plot


#Engle-Granger 2 steps method

##1st step: check if all 6 countries are I(1)
adf1=ur.za(log(monthly_xts$USA),model = "both",lag = 1)
summary(adf1)
#with structural break zivot test indicate that log USA is I(1)


adf2=ur.za(log(monthly_xts$GER), model = "intercept",lag = 1)
summary(adf2)
#Teststatistic: -4.0838 > -5.08 (5%)
#zivot test with H0:unit root with break , we can reject it and GER is a stationary with 1 time break in the level
#log GER is I(0)


adf3=ur.za(log(monthly_xts$JAP), model = "both",lag = 1)
summary(adf3)
#zivot suggest JAP is I(1)

adf4=ur.za(log(monthly_xts$UK),model = "both",lag = 1)
summary(adf4)
#log UK is I(0)

adf5=ur.za(log(monthly_xts$CAD), model = "both",lag = 1)
summary(adf5)
#log CAD is I(0) IN Zivot test

adf6=ur.za(log(monthly_xts$FRA),model = "both",lag = 1)
summary(adf6)
#log FRA is I(0)

###Conclusion: only log(usa) and log(jap) is I(1)=> they may have cointegration


#Test wether error term of USA~JAP is I(0)
usa1=log(monthly_xts$USA)
jap1=log(monthly_xts$JAP)
ger1=log(monthly_xts$GER)
cad1=log(monthly_xts$CAD)
fra1=log(monthly_xts$FRA)
uk1=log(monthly_xts$UK)
data= merge(usa1,jap1,ger1,fra1,uk1,cad1)

jap_res_log=lm(USA~JAP, data = data)
error.jap.return.log= ur.df(jap_res_log$residuals, lags = 1,type="none")
summary(error.jap.return.log)
#cointegration at the level of 5% using EG ADF statistic using log price (-3.64 < -3.5 at 5%)



#select lag + long-run relationship + VECM using USA~GER (copy this code for the rest 4 pairs )

## SELECT LAG
usa_jap=monthly_index[c("JAP","USA")]
info.bv <- VARselect(ts(usa_jap), lag.max = 12, type = "none")
info.bv$selection #Use lag 1

## LONG-RUN 
jap.lr=lm(USA~JAP, data=monthly_xts)
summary(jap.lr)#the coefficent is insignificant(too close to 0) =>the series may have a long-run relationship 
               #because as times increase, the difference will converged and no longer change.
##ECM
usa=diff(log(monthly_xts$USA))
jap=diff(log(monthly_xts$JAP))
us.d <- usa[3:145,]
jap.d <- jap[3:145,]
error.ecm1 <- jap.lr$residuals[-1:-2]
usa.d1 <- usa[2:144,]
jap.d1 <- jap[2:144,]
ecm.jap <- lm(us.d~error.ecm1+usa.d1+jap.d1)
summary(ecm.jap)
#the log(usa) is weakly exogenous with respect to the cointegrating parameters 
#since it does not adjust to past deviations from long-run equilibrium

ecm.jap1 <- lm(jap.d~error.ecm1+usa.d1+jap.d1)
summary(ecm.jap1)
#the log(jap) is strongly exogenous with respect to the cointegrating parameters 
#since it adjusts to past deviations from long-run equilibrium



#Johansen procedure
VARselect(data,lag.max = 12,type = "trend")
trace.3 <- ca.jo(data, type="trace", ecdet="trend", K=2)
summary(trace.3)
## test statistics under the null of r=0 is 131.19>114.9 (5%)=>Exist cointergating vector along the 6 variables
## It suggests that there is 1 coitegrating vector r<=1 (87.29<87.31 at 5%)
## This test-statistics is kind of close to the critical value. So if we aren't so trict, we can conclude r=2

model=VECM(data,lag = 1, r=1, include = "trend",estim = "2OLS")
summary(model)
#Cointegrating vector (estimated by 2OLS):
#USA        JAP        GER       FRA         UK        CAD
#r1   1 0.02128102 -0.5109791 0.3853819 -0.8103477 -0.1256806
#Equation JAP 0.0230(0.0468)      0.0142(0.0076).    -3.0e-05(9.3e-05) 
#I found an integrating vector and a significant level of log(JAP) long run relationship with log(USA)
#This is consistent with the EG 2-step I have conducted above (only log jap and log usa cointegrated)




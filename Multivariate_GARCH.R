rm(list = ls())
options(warn=-1)
library(qrmtools)
library(tidyverse)
library(xts)
library(rugarch)
library(broom)
library(readxl)
library(pastecs)
library(vars)
library(reshape2)
library(forecast)
library(dplyr)
library(stargazer)
library(astsa)
library(PerformanceAnalytics)
library(rmgarch)
daily_index= read_xlsx("C:/Users/Admin/Desktop/indexdaily.xlsx", col_names = TRUE,sheet = "sheet2")
daily_index=na.omit(daily_index)
date_ydm= as.Date(daily_index$Date)
daily.xts=xts(daily_index, order.by = date_ydm)
daily.xts$Date=NULL
storage.mode(daily.xts)="numeric"
daily.return=returns(daily.xts,method = "logarithmic")
stargazer(daily_index[1:20,], type = "text", summary = FALSE)

#Plot the return
daily_return_plot = tidy(daily.return) %>%
  ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "3 Advanced country ",subtitle = "End of day",caption = " Source: MSCI") +xlab("Date") + ylab("Return") +scale_color_manual(values = c("Red", "Black", "DarkBlue"))
daily_return_plot

#Summary staistics
stargazer(stat.desc(daily_index[-1]), type = "text")

#Plot cross correlation
pairs(as.matrix(-daily.return), main = "Scatter plot matrix of risk-factor changes", gap = 0, pch = ".")
acfm(-daily.return,max.lag = 10)


#ARCH effect
a=VAR(daily.return,p = 1)
arch.test(a)
#Have heteroscedasticity (pvalue<1%)

#Univariate GARCH
##Choose the distribution. 

###UK series
# a) Assuming normal distribution
norm <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "norm")
uk_norm=ugarchfit(norm, daily.return$UK)
      #Adjusted Pearson Goodness-of-Fit Test: 
      #P-value is smaller than 5%->reject the null->Normal dist is not approriate
plot(uk_norm, which=8)#The plot also show some skewness
plot(uk_norm, which=9)#QQ-plot show fatter tail

# b) Assuming skew t-distribution distribution
t_dist <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
uk_t=ugarchfit(t_dist, daily.return$UK)
      #Adjusted Pearson Goodness-of-Fit Test: 
      #P-value is larger than 5%->accept the null->Skew t-dist is approriate
      #The skew is 0.85<1-> error has a skew distribution
plot(uk_t, which=8)#The plot also show some skewness
plot(uk_t, which=9)#QQ-plot show a more fitted line
#CONCLUSION: Use skew t-dist for the error term for UK

###GERMANY series
# a) Assuming normal distribution
norm1 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "norm")
ger_norm=ugarchfit(norm1, daily.return$GERMANY)
        #Adjusted Pearson Goodness-of-Fit Test: 
        #P-value is smaller than 5%->reject the null->Normal dist is not appropriate
plot(ger_norm, which=8)#The plot also show some skewness
plot(ger_norm, which=9)#QQ-plot show fatter tail

# b) Assuming skew t-distribution distribution
t_dist1 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
ger_t=ugarchfit(t_dist1, daily.return$GERMANY)
    #Adjusted Pearson Goodness-of-Fit Test: 
    #P-value is smaller than 5%->reject the null->Skew t-dist is inappropriate
plot(ger_t, which=8)#The plot also show some skewness
plot(ger_t, which=9)#QQ-plot doesn't seem to have a better fit

#c) Assuming skew ged-distribution distribution
sged=ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
ger.sged=ugarchfit(sged, daily.return$GERMANY)
        #Adjusted Pearson Goodness-of-Fit Test: 
        #P-value is larger than 5%->accept the null->Skew GED is appropriate
plot(ger.sged, which=8)#The plot also show some skewness
plot(ger.sged, which=9)#QQ-plot seems to have a better fit
#CONCLUSION: Use skew generalized error dist for the error term for GER

###USA series
# a) Assuming normal distribution
norm2 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "norm")
usa_norm=ugarchfit(norm2, daily.return$USA)
        #Adjusted Pearson Goodness-of-Fit Test: 
        #P-value is smaller than 5%->reject the null->Normal dist is not appropriate
plot(usa_norm, which=8)#The plot also show some skewness
plot(usa_norm, which=9)#QQ-plot show fatter tail

# b) Assuming skew t-distribution
t.dist2 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
usa_t=ugarchfit(t.dist2, daily.return$USA)
    #Adjusted Pearson Goodness-of-Fit Test: 
    #P-value is smaller than 5%->reject the null->skew t-dist is not appropriate
plot(usa_t, which=8)#The plot also show some skewness
plot(usa_t, which=9)#QQ-plot show fatter tail

# c) Assuming skew ged-distribution
ged.dist2 <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
usa_ged=ugarchfit(ged.dist2, daily.return$USA)
      #Adjusted Pearson Goodness-of-Fit Test: 
      #P-value is smaller than 5%->reject the null->skew dist is not appropriate
plot(usa_ged, which=8)#The plot also show some skewness
plot(usa_ged, which=9)#QQ-plot show fatter tail
#CONCLUSION : using Adjusted Pearson Goodness-of-Fit Test result in no appropriate distribution
            # So I will use the usa_t model with has the smallest AIC information criteria (-6.74) 
            # The usa_t has a skew t-distribution

##Select best GARCH model

###UK series, all use GARCH(1,1)
# 1) sGARCH with skew t-dist 
uk_t@fit$coef#alpha1 and beta1 are larger than 0
#alpha1+beta1<1->shocks to the conditional variance will be highly persistent
#alpha1 p-value=17%=>alpha1 is insignificant at 5%
#joint effect of sign bias test is significant at 10%
#Indicate an asymmetric model
#CONCLUSION: sGARCH is not the best model

# 2)iGARCH
igarch <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
uk_igarch=ugarchfit(igarch, daily.return$UK)
uk_igarch@fit$coef
uk_igarch@fit$tval
#CONCLUSION: alpha1 is not significant so iGARCH is not the best model

# 3)eGARCH
egarch <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
uk_egarch=ugarchfit(egarch, daily.return$UK)
uk_egarch@fit$coef
uk_egarch@fit$tval
#The gamma1 coef(p-value=44.8%) is insignificant at 1%=>eGARCH model can't capture the leverage effect
#Also, alpha1<0=>violate the condition of GARCH estimate
#CONCLUSION: eGARCH is not the best model

# 4)GJR-GARCH
gjr <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
uk_gjr=ugarchfit(gjr, daily.return$UK)
uk_gjr@fit$coef
uk_gjr@fit$tval
#Again, gamma1 and alpha1 is insignificant at 5%
#I=1: if epsilon<0 (negative shock)
#I=0: otherwise
#gjr-GARCH is not the best model

# 5) apARCH
aparch <- ugarchspec(variance.model = list(model = "apARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
uk_ap=ugarchfit(aparch, daily.return$UK)
uk_ap@fit$persistence
uk_ap@fit$coef
uk_ap@fit$tval
plot(uk_ap, which=3)
plot(uk_ap, which=12)
plot(uk_ap, which="all")
# alpha1 and beta1 are larger than 0 and all significant at 5%
# alpha1+beta1<1 (show by the persistent)
# gamma1>0 and significant, which reflect the leverage effect.
# Negative news has stronger impact than possitive news
# delta>1 also indicate that bad news have greater impact than good news (power>1)
# This maybe a good model

# 6) fGARCH
f_garch=ugarchspec(variance.model = list(model = "fGARCH",garchOrder = c(1, 1),submodel="ALLGARCH"),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
uk_f=ugarchfit(f_garch, daily.return$UK)
uk_f@fit$tval
plot(uk_f, which=3)
plot(uk_f, which=12)
plot(uk_f, which="all")
uk_f@fit$coef
# alpha1 and beta1 are larger than 0 and all significant at 5%
# alpha1+beta1<1 (show by the persistent)
# alpha1=0.064 indicate that today volatility is based very little on yesterday price
# beta1= 0.91 indicate that today volatility is based largely on yesterday volatility
# Or in other word, shock is persistent
# eta11 is insignificant
# eta21>0 and significant, which reflect the leverage effect.
# Negative news has stronger impact than positive news
# delta>1 also indicate that bad news have greater impact than good news (power>1)
# This maybe a good model too
# But the Bayesian Information criteria for aparch is smaller than fgarch (-6.445<-6.440)
# FINAL CONCLUSION: APARCH is the best model for UK

###GER series, all use GARCH(1,1)
# 1) sGARCH with skew t-dist 
ger.sged@fit$coef#alpha1 and beta1 are larger than 0
ger.sged@fit$tval#all coef is significant at 5%
ger.sged@fit$persistence#alpha1+beta1<1->shocks to the conditional variance will be highly persistent
#joint effect of sign bias test is significant at 5%=>indicating an asymmetric model
#CONCLUSION: sGARCH may not be a good model

# 2)iGARCH
igarch1 <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
ger_igarch=ugarchfit(igarch1, daily.return$GERMANY)
ger_igarch@fit$tval
#CONCLUSION: alpha1 is not significant so iGARCH is not the best model

# 3)eGARCH
egarch1 <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
ger_egarch=ugarchfit(egarch1, daily.return$GERMANY)
ger_egarch@fit$tval
ger_egarch@fit$coef
plot(ger_egarch, which=12)
#The gamma1>0 so the model can't capture the leverage effect of stock
#the news impact curve show no impact of positive shock=>contradict to gamma1
#Also, alpha1<0=>violate the condition of GARCH estimate
#t-statistics of Beta1 is too high=>there maybe some bias in estimation
#sign bias diagnostic is significant of 10%=> indicating mispecification of the model.
#CONCLUSION: eGARCH is not the best model

# 4)GJR-GARCH
gjr1 <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
ger_gjr=ugarchfit(gjr1, daily.return$GERMANY)
ger_gjr@fit$coef
ger_gjr@fit$tval
plot(ger_gjr, which=12)
#The news impact curve also confront the same problem with eGARCH
#I=1: if epsilon<0 (negative shock)
#I=0: otherwise
#Again, alpha1 is insignificant at 5%
#gjr-GARCH is not the best model

# 5) apARCH
aparch1 <- ugarchspec(variance.model = list(model = "apARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
ger_ap=ugarchfit(aparch, daily.return$GERMANY)
ger_ap@fit$persistence
ger_ap@fit$coef
ger_ap@fit$tval
plot(ger_ap, which=12)
# alpha1 and beta1 are larger than 0 and all significant at 5%
# alpha1+beta1<1 (show by the persistent)
# gamma1=1 and have large t-statistics=>The estimate is bias
# This not a good model

# 6) fGARCH
f_garch1=ugarchspec(variance.model = list(model = "fGARCH",garchOrder = c(1, 1),submodel="ALLGARCH"),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sged")
ger_f=ugarchfit(f_garch1, daily.return$GERMANY)
ger_f@fit$coef
ger_f@fit$tval
ger_f@fit$persistence
#all the coef is significant at 1%
#eta11<0 indicate an asymmetric model, in which good news has more effect 
#in volatility than bad news
plot(ger_f, which=3)
plot(ger_f, which=12)
#Good news have more effect than bad news
tt=sigma(ger_f)
bb=sigma(ger_ap)
APARCH_BLACK_fGARCH_RED=cbind(bb,tt,abs(daily.return$GERMANY))
plot(APARCH_BLACK_fGARCH_RED)
#Compare to the APARCH, fGARCH capture a larger spike
#Alkaine=-6.37<-6.36(APARCH)
# alpha1=0.3 indicate that today volatility is based largely on yesterday price
# beta1= 0.69 indicate that today volatility is based largely on yesterday volatility
# Or in other word, shock is persistent
#CONCLUSION: fGARCH is the best model for GER

###USA series, all use GARCH(1,1)
# 1) sGARCH with skew t-dist 
usa_t@fit$coef#alpha1 and beta1 are larger than 0
usa_t@fit$tval#all coef is significant at 5%
usa_t@fit$persistence#alpha1+beta1<1->shocks to the conditional variance will be highly persistent
#joint effect of sign bias test is significant at 5%=>indicating an asymmetric model
#CONCLUSION: sGARCH may not be a good model

# 2)iGARCH
igarch2 <- ugarchspec(variance.model = list(model = "iGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
usa_igarch=ugarchfit(igarch2, daily.return$USA)
usa_igarch@fit$tval
#CONCLUSION: joint effect of sign bias test also significant so iGARCH is not the best model

# 3)eGARCH
egarch2 <- ugarchspec(variance.model = list(model = "eGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
usa_egarch=ugarchfit(egarch2, daily.return$USA)
usa_egarch@fit$tval
usa_egarch@fit$coef
plot(ger_egarch, which=12)
#The gamma1>0 so the model can't capture the leverage effect of stock
#Also, alpha1<0=>violate the condition of GARCH estimate
#t-statistics of Beta1 is too high=>there maybe some bias in estimation
#CONCLUSION: eGARCH is not the best model

# 4)GJR-GARCH
gjr2 <- ugarchspec(variance.model = list(model = "gjrGARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
usa_gjr=ugarchfit(gjr2, daily.return$USA)
usa_gjr@fit$coef
usa_gjr@fit$tval
usa_gjr@fit$persistence
plot(usa_gjr, which=12)
plot(usa_gjr, which=3)
#I=1: if epsilon<0 (negative shock)
#I=0: otherwise
#gamma1>0=>leverage effect exist
#Persistent is less than 1
#alpha1 is insignificant
#sign bias test significant at 10%=>indicating not the best asymmetric model
#gjr-GARCH may not be the best model

# 5) apARCH
aparch2 <- ugarchspec(variance.model = list(model = "apARCH",garchOrder = c(1, 1)),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
usa_ap=ugarchfit(aparch2, daily.return$USA)
usa_ap@fit$persistence
usa_ap@fit$coef
usa_ap@fit$tval
plot(usa_ap, which=3)
plot(usa_ap, which=12)
# alpha1 and beta1 are larger than 0 and all significant at 5%
# persistence<1 (show by the persistence)
# gamma1=0.93 indicate that bad news have approximately as double 
# impact on volatility as good news (show by the graph)
# No sign bias is significant at 10%=>apARCH fit better gjrGARCH
#CONCLUSION: This is the best model so far

# 6) fGARCH
f_garch2=ugarchspec(variance.model = list(model = "fGARCH",garchOrder = c(1, 1),submodel="ALLGARCH"),mean.model = list(armaOrder = c(0, 0),include.mean = TRUE),distribution.model = "sstd")
usa_f=ugarchfit(f_garch2, daily.return$USA)
usa_f
#Convergence problem occur(persistence>1)=>fGARCH is not a good model
cc=sigma(usa_gjr)
ll=sigma(usa_ap)
APARCH_BLACK_gjrGARCH_RED=cbind(ll,cc,abs(daily.return$USA))
plot(APARCH_BLACK_gjrGARCH_RED)
#Both model can capture much of the spike (GJR seem to be a bit more)
#But alpha1 in GJR is insignificant
# alpha1=0.1 indicate that today volatility is based very little on yesterday price
# beta1= 0.8 indicate that today volatility is based largely on yesterday volatility
# Or in other word, shock is persistent
#CONCLUSION: APARCH is the best model for USA

###CONCLUSION
### sstd dist for UK and USA, sged dist for GER
### APARCH FOR USA AND UK, fGARCH for GER

#DCC AND CCC
##DCC estimate
marginspec <- multispec(c(aparch,f_garch1,aparch2))
mspec <- dccspec(marginspec, dccOrder = c(1,1,1), model = "DCC", distribution = "mvt")
mod=dccfit(mspec,daily.return)
coef(mod)
plot(mod, which=5)
uk.resi.dcc=mod@model$residuals[,1]
ger.resi.dcc=mod@model$residuals[,2]
usa.resi.dcc=mod@model$residuals[,3]            
plot(uk.resi.dcc, type="l")
title(main = "Uk dcc residual")
plot(ger.resi.dcc, type="l")
title(main = "GER dcc residual")
plot(usa.resi.dcc, type="l")
title(main = "USA dcc residual")
plot(uk.resi.dcc+ger.resi.dcc+usa.resi.dcc, type="l")
### The DCC residuals are all stationary so we can continue to interpret the result
### Most of the coefficients are significant
### dccalpha1 and dccbeta1 is approximately the same of each coef of the univariate of each series
### dcca1+dccb1<1 and both or significantly different from 0
### Indicating that UK, GER and USA inter correlation is changing overtime
### dcca1 indicate show the jointly instant disturbance of UK,GER,USA
### dccb1 indicate the jointly persistence of UK,GER,USA

#Plot the conditional correlation
uk_usa=rcor(mod)
cor_uk_usa=uk_usa[3,1,]
cor_uk_ger=uk_usa[2,1,]
cor_ger_usa=uk_usa[3,2,]
kkkk=cbind(cor_uk_ger,cor_uk_usa,cor_ger_usa)
kkkk=ts(kkkk)
conditional_cor_plot = tidy(kkkk) %>%
  ggplot(aes(x=index,y=value, color=series)) +geom_line() +facet_grid(series~.,scales = "free")+labs(title = "3 Advanced country ",subtitle = "Conditional correlation",caption = " Source: MSCI") +xlab("Date") + ylab("Correlation") +scale_color_manual(values = c("Red", "Darkblue", "Orange"))
conditional_cor_plot
cor(daily.return$GERMANY, daily.return$USA)
cor(daily.return$UK, daily.return$USA)
cor(daily.return$GERMANY, daily.return$UK)
### The correlation of ger_usa fluctuate around its long term correlation (around 0.59)
### The correlation of uk_ger fluctuate around its long term correlation (around 0.8)
### The correlation of usa_ ukfluctuate around its long term correlation (around 0.58)
### In the early 2020 (covid outbreak), correlation of UK and USA, USA and GER is pretty low
### (the downward spike between 500 and 750) indicate that we can use UK and GER to hedge the risk
### from the negative shock of USA
### in the Third quarter of 2020, UK and USA, USA and GER has a high correlation
### We can include all of the three into our portfolio to maximize our profit
### when there is crisis, make sure not to add both UK and GER in your portfolio
### CONCLUSION: when the conditional correlation of the 2 time series is low, we cant use 
###             these 2 series to make a hedging portfolio 
###             when the conditional correlation is high, we can include these 2 asset 
###             to maximize our profit.


###CCC estimate
copspec <- cgarchspec(uspec = marginspec,
                      distribution.model = list(copula = "mvt", method = "ML",
                                                time.varying = FALSE, transformation = "parametric"))
mod2 <- cgarchfit(copspec, daily.return)
plot(rcor(mod2))#correlation is constant->CCC

#Compare the two method
cbind(coef(mod), coef(mod2))
cbind(mod@mfit$llh,mod2@mfit$llh)
###CCC have a lower log-likelihood than DCC
###The last 4 coef are different 
###DCC is a better model and it can capture the dynamic correlation
###Which is better to make a portfolio
###CONCLUSION: choose DCC

#VaR of univariate GARCH for UK vs that of ew_Dcc
plot(uk_ap, which=2)
title(sub = "UK APARCH VAR")
plot(mod, which=5)


#VaR of univariate GARCH for GER vs that of ew_dcc
plot(ger_f, which=2)
title(sub = "GER APARCH VAR")
plot(mod, which=5)

#VaR of univariate GARCH for USA vs that of ew_dcc
plot(usa_ap, which=2)
title(sub = "USA APARCH VAR")
plot(mod, which=5)

### The difference between 2 approach is that 
### DCC ew VaR will calculate the weighted of mu, shape and weighted conditional std
### univariate VaR is calculated on their own conditional std 
### It doesn't account for the correlation effect condition on time
### That's why at some time VaR of ew_DCC is larger and some is smaller 
### Depend on the inter conditional correlation of each asset in the port. 


### Show are the code and equation to calculate equally weighted VaR at 1% (DCC)
### port= Return.portfolio(daily.return,weights = c(1/3,1/3,1/3))
### mu.= 1/3 * mod@mfit$coef[["[UK].mu"]]+ 1/3 *mod@mfit$coef[["[GERMANY].mu"]]+ 1/3 *mod@mfit$coef[["[USA].mu"]]
### sig..=Return.portfolio(sigma(mod),weights = c(1/3,1/3,1/3))
### nu. <- mod@mfit$coef[["[Joint]mshape"]]
### VaR.. <- as.numeric(mu. + sig.. * sqrt((nu.-2)/nu.) * qt(0.01, df = nu.))
### VaR... <- as.numeric(mu. + sig.. * sqrt((nu.-2)/nu.) * qt(0.99, df = nu.))
### ew_var_dcc=cbind(VaR..,VaR..., port)
### plot(ew_var_dcc, type='l')
### title(main = "VAR estimate by dcc")
### plot(mod, which=5)



  ## Create a custom function to calculate the descriptive data. 
  ##Source from Econometric Methods in Business and Economics-Winter 24/25-Prof. Dr. Robert Jung and Domenic Franjic-Solution Ex I.
  makeSummary <- function(data){
    
    # Extract the dimensions of the data set
    no_of_cols <- ncol(data)
    
    # Initialise an object that will be returned from the function
    summary_out <- matrix(NaN, no_of_cols, 9)
    
    # Start for loop over the columns of data #
    
    for(curr_col in 1:no_of_cols){
      
      # Compute the mean and store it in the return object
      summary_out[curr_col, 1] <- round(mean(data[, curr_col]),digits =4)
      
      # Compute the sd and store it
      summary_out[curr_col, 2]  <- round(sd(data[, curr_col]),digits = 4)
      
      # Compute the percentiles and store them
      per <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
      summary_out[curr_col, 3:9] <- round(quantile(data[, curr_col], per), digits = 6)
      
    }
    # End for loop over the columns of data #
    
    # Name the rows of the return object
    rownames(summary_out) <- colnames(data)
    
    # return
    return(summary_out)
    
  }
# Load  packages
library(dplyr)
library(texreg)
library(car)
library(plm)
library(ggplot2)
library(data.table)
library(plm)
library(lmtest)

## Task 1
### Task 1.1
# Add helper function and load library
# Download R-helper functions
source("https://ilias.uni-hohenheim.de/data/UHOH/lm_data/lm_2016122/prepare_r_packages_and_helper_functions.R")
# Include R-helper functions
source("r_helper_functions.R")
# Load data into R
crime_dat<-read.table(file = "F:/Hohenheim/WS 24-25/Econometrics/Midterm/Assignment_data_2425.txt",
           header = TRUE,
           sep = ",")
# filter out the data for 2 years- 2018 and 2024 and display some of the data
crime_dat_2018<-filter(crime_dat, crime_dat$year==2018)
crime_dat_2024<-filter(crime_dat, crime_dat$year==2024)
head(crime_dat_2018)
head(crime_dat_2024)
# Descriptive statistics of year 2018
summary_2018<-as.data.frame(makeSummary(crime_dat_2018))
colnames(summary_2018)<-c("mean", "std", "10%", "25%", "40%", "50%", "60%","75%", "90%")
summary_2018
# Descriptive statistics of the year 2024
summary_2024<-as.data.frame(makeSummary(crime_dat_2024))
colnames(summary_2024)<-c("mean", "std", "10%", "25%", "40%", "50%", "60%","75%", "90%")
summary_2024
# key figures for the probability of conviction (x2) and the probability of prison sentence (x3) in the year 2018 
pro_x2_x3_2018 <-t(summary_2018[5:6,]) #extract line 5-6 and every column in the summary statistics of 2018
rownames(pro_x2_x3_2018)<-colnames(summary_2018) #Rename the row by taking the column name of summary_2018 data set
colnames(pro_x2_x3_2018)<-c("probability of conviction (x2)","probability of prison sentence (x3)")# Rename the column
pro_x2_x3_2018
# key figures for the probability of conviction (x2) and the probability of prison sentence (x3) in the year 2024
pro_x2_x3_2024 <-t(summary_2024[5:6,]) #extract line 5-6 and every column in the summary statistics of 2024
rownames(pro_x2_x3_2024)<-colnames(summary_2024) #Rename the row by taking the column name of summary_2024 data set
colnames(pro_x2_x3_2024)<-c("probability of conviction (x2)","probability of prison sentence (x3)")# Rename the column
pro_x2_x3_2024
# Plot the histogram to see the distribution
par(mfrow = c(2, 2),
    mar = c(2, 2, 3, 2)) #plot parameter
#Histogram
hist(crime_dat_2018$x2, main="probability of conviction (x2) 2018", xlab = "")
hist(crime_dat_2018$x3, main = "probability of prison sentence (x3) 2018",xlab = "")
hist(crime_dat_2024$x2, main="probability of conviction (x2) 2024",xlab = "")
hist(crime_dat_2024$x3, main = "probability of prison sentence (x3) 2024",xlab = "")

### Task1.2
# the average of probability of conviction (x2) in 2018
avg_x2_2018<-pro_x2_x3_2018[1,1]
avg_x2_2018
# Regress x2 on binary variable d1 to examine if there is any difference in the average of x2 in urban and non-urban
lm_result_1<-lm(x2~d1, data = crime_dat_2018)
summary(lm_result_1)

### Task 1.3
# the average of probability of prison in 2024
avg_x3_2024<-pro_x2_x3_2024[1,2]
log(avg_x3_2024)
# Regress x3 on binary variable d1 to examine if there is any difference in the average of x3 in urban and non-urban.
lm_result_2<-lm(x3~d1, data = crime_dat_2024)
summary(lm_result_2)



## Task2
### Task 2.1
# Transform relevant variables into natural logarithms
crime_dat <- crime_dat %>%
  mutate(
    ln_y = log(y),
    ln_x1 = log(x1),
    ln_x2 = log(x2),
    ln_x3 = log(x3),
    ln_x6 = log(x6)
  )
# Estimate the regression model (1) using OLS
lm_crime <- lm(
  ln_y ~ ln_x1 + ln_x2 + ln_x3 + x4 + I(x4^2) + x5 + ln_x6 + d1,
  data = crime_dat)
summary(lm_crime)

###Task 2.2
#Intercept = -2.483396

###Task 2.3
# Given values
beta_hat <- -0.025365      # Estimated coefficient
beta_null <- 0.03       # Hypothesized value
se_beta <- 0.005072       # Standard error
# Calculate test statistic
t_stat <- (beta_hat - beta_null) / se_beta
t_stat
# Degrees of freedom
n <- nrow(crime_dat)    # Number of observations
k <- 8                  # Number of predictors (including intercept)
df <- n - k
# Calculate p-value
p_value <- 2 * pt(-abs(t_stat), df = df)
p_value
#Reject the null hypothesis

###Task 2.4
# Full (unrestricted) model 
unrestricted_model <- lm_ct_fun(ln_y ~ ln_x1 + ln_x2 + ln_x3 + x4 + I(x4^2) + x5 + ln_x6 + d1, data = crime_dat, hc.type = "HC1")
# Restricted model (exclude x4,x4^2 and x6)
restricted_model <- lm(ln_y ~ ln_x1 + ln_x2+ln_x3 +x5 + d1, data = crime_dat)
# Compare RSS values
rss_unrestricted <- sum(residuals(unrestricted_model)^2)
rss_restricted <- sum(residuals(restricted_model)^2)
# Number of restrictions
q <- 3 #For x4,x4^2 and x6
n <- nrow(crime_dat) # Number of observations
k <- length(coef(unrestricted_model))-1# Number of predictors (excluding intercept)
# F-statistic
f_stat <- ((rss_restricted - rss_unrestricted) / q) / (rss_unrestricted / (n - k-1))
f_stat
# Calculate p-value
p_value <- pf(f_stat, df1 = q, df2 = n - k-1, lower.tail = FALSE)
p_value
#Reject null hypothesis

###Task 2.5
crime_dat$z <- log(crime_dat$x6) + crime_dat$d1
# Estimate the regression model with z
model_with_z <- lm(ln_y ~ ln_x1 + ln_x2 + ln_x3 + x4 + I(x4^2) + x5+ ln_x6+z , data = crime_dat)
summary(model_with_z)
# Extract the coefficient and standard error for z
beta_x6 <- coef(model_with_z)["ln_x6"]
se_x6 <- summary(model_with_z)$coefficients["ln_x6", "Std. Error"]
se_x6
# Calculate the t-statistic
t_stat <- beta_x6 / se_x6
t_stat
# Calculate the p-value for a two-tailed test
p_value <- 2*pnorm(t_stat, mean = 0, sd = 1, lower.tail = TRUE)
cat("Test Statistic:", t_stat, "\n")
cat("P-value:", p_value, "\n")
# reject null hypothesis



### Task 3
# Entity fixed effect model.
plm_crime_entity <-plm_ct_fun(ln_y~ln_x1+ln_x2+ln_x3+x4+I(x4^2)+x5+ln_x6+d1,data = crime_dat,index = c("id","year"),effect = "individual",model ="within", hc.type = "HC1")
# Time fixed effect model
plm_crime_time <-plm_ct_fun(ln_y~ln_x1+ln_x2+ln_x3+x4+I(x4^2)+x5+ln_x6+d1,data = crime_dat,index = c("id","year"),effect = "time",model ="within", hc.type = "HC1")
# OLS model
lm_crime_1 <-unrestricted_model

# Creating parameters for the regression result table.
se_1<-lm_crime_1$ct[,2]
se_2<-plm_crime_entity$ct[,2]
se_3<-plm_crime_time$ct[,2]
pval_1<-lm_crime_1$ct[,4]
pval_2<-plm_crime_entity$ct[,4]
pval_3<-plm_crime_time$ct[,4]

# Creating a result table comparing model 1-task 2 (OLS model), model 2-task 3 (Fixed effect model), and model 3-time fixed effect
screenreg(list(lm_crime_1, plm_crime_entity, plm_crime_time),
          override.se = (list(se_1,se_2,se_3)),
          override.pvalues = (list(pval_1,pval_2,pval_3)),
          caption = "Estimation Results",
          caption.above = TRUE,
          label = "Tab03",
          custom.model.names = c("Model 1-OLS","Model 2-Entity fixed effect", "Model 3-Time fixed effect"),
          custom.coef.names = c("intercept","ln_x1","ln_x2","log_x3","x4","x4^2","x5","ln_x6","d1"),
          float.pos = "h",
          single.row = TRUE,
          booktabs = TRUE,
          dcolumn = TRUE,
          use.packages = FALSE,
          ci.force = FALSE,
          digits = 3,
          include.rsquared = TRUE,
          include.adjrs = TRUE,
          include.nobs = TRUE,
          doctype = FALSE)




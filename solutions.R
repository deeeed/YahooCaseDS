install.packages("forecast")
install.packages("FinancialMath")
install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("smooth")
install.packages("Mcomp")
install.packages("ggfortify")

library(ggfortify)

require("forecast")
require("FinancialMath")
require("ggplot2")
require("scales")
require("xts")
require("grid")
require("gridExtra")
require(smooth)
require(Mcomp)

theme_set(theme_minimal())
# cleanup env
rm(list = ls())

data_monthly_audience <- read.csv("tumblr_worldwide_monthly_direct_audience.csv", header = TRUE)
data_us_audience <- read.csv("tumblr_us_monthly_direct_audience.csv", header = TRUE)
# Load yahoo's forecast to compare valuation models - Not used for predictions
data_yahoo_forecast <-read.csv("model_pop_forecasts.csv", header = FALSE)

ts_world_audience <- ts(data_monthly_audience, start=c(2010, 4), frequency=12) 
ts_us_audience <- ts(data_us_audience, start=c(2010, 4), frequency=12) 
ts_yahoo_forecast <- ts(data_yahoo_forecast, start=c(2010, 4), frequency=12) 
autoplot(ts_world_audience, facets = TRUE)
data_yahoo_forecast

# For the following we will only focus on the "People" WolrdWide part of our data because the financial valuation is based on this number and its forecasts.
ts_world_data<-ts_world_audience[,"People"] # For worldwide data
ts_us_data<-ts_us_audience[,"People"] # For US data


data_world <- data.frame(Y=as.matrix(ts_world_audience), date=time(ts_world_audience))
data_us <- data.frame(Y=as.matrix(ts_us_audience), date=time(ts_world_audience))
ggplot() + 
  geom_line(data = data_world, aes(y=Y.People, x=date, color="World")) + 
  geom_line(data = data_us, aes(y=Y.People, x=date, color="US")) +
  scale_y_continuous(labels=comma) +
  ggtitle("People Worldwide unsing Tumblr") + xlab("") + ylab("")

ggsubseriesplot(ts_world_data) + ggtitle("People Worldwide unsing Tumblr") + xlab("Month") + ylab("Thousands")
View(ts_yahoo_forecast)

# We can see an upward trend and no seasonality in the plot.
# Let's confirm this by doing decompositions of the time series
#plot various decompositions into error/noise, trend and seasonality
fit_classical_additive <- decompose(ts_world_data, type="additive") #decompose using "classical" method, additive form
plot_additive <- autoplot(fit_classical_additive) + scale_y_continuous(labels=comma) + ggtitle("Classical 'additive' decomposition ") 
fit_classical_multiplicative <- decompose(ts_world_data, type="multiplicative") #decompose using "classical" method, multiplicative form
plot_multiplicative <- autoplot(fit_classical_multiplicative) + scale_y_continuous(labels=comma) + ggtitle("Classical 'multiplicative' decomposition ")
fit_stl <- stl(ts_world_data, t.window=12, s.window="periodic", robust=TRUE) #decompose using STL (Season and trend using Loess)
plot_stl <- autoplot(fit_stl) + scale_y_continuous(labels=comma)  + scale_y_continuous(labels=comma)  + ggtitle("STL decomposition ")
grid.arrange(grobs=list(plot_multiplicative, plot_additive, plot_stl), ncol=3) 


# Now we notice a pattern in the seasonal plot, what does it mean?
# Most likely students will use less the application during the summer when they are not in school

# One thing to worry about is the decrease trend at the end. Should it be ignored?


#### Before to look at our forecasts, we should check the monthly growth on the existing data
# and compare it to the 1.35% used by Yahoo
monthly_growth_world <- ts(data_monthly_audience$People, start = c(2010, 4), frequency = 12)
monthly_growth_world <- (lag(monthly_growth_world)/monthly_growth_world - 1) * 100
monthly_growth_us <- ts(data_us_audience$People, start = c(2010, 4), frequency = 12)
monthly_growth_us <- (lag(monthly_growth_us)/monthly_growth_us - 1) * 100

average_world_growth <- mean(monthly_growth_world) # 5.6%
average_us_growth <- mean(monthly_growth_us) # 4.8%

plot_growth_world <- autoplot(average_world_growth, ts.geom = 'bar') + ggtitle("Monthly Growth (World)")
plot_growth_us <- autoplot(plot_growth_us, ts.geom = 'bar') + ggtitle("Monthly Growth (US)")
grid.arrange(grobs=list(plot_growth_world, plot_growth_us)) 
# We can see that yearly growth trend is already 'dampening'


##############################################################
##############################################################
# let's start our forecast with some benchmarking values using simple methods
horizon <- 7 + 9*12 # 7months in 2013 + 9 years from 2014 to 2022

autoplot(ts_world_data) +
  autolayer(meanf(ts_world_data, h=horizon),
            series="Mean", PI=FALSE) +
  autolayer(rwf(ts_world_data, h=horizon),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(ts_world_data, drift=TRUE, h=horizon),
            series="Drift", PI=FALSE) +
  ggtitle("People Worldwide unsing Tumblr") +
  xlab("Month") + ylab("Thousands") +
  guides(colour=guide_legend(title="Forecast"))

# several exponential smoothing models to forecast the increase in the traffic
level<-c(0.8,0.95) # Confidence Interval 80%, 95%

"AAN damped=false"
fit_drift <- rwf(ts_world_data, drift=TRUE, h=horizon)

fit_ZZZ <- ets(ts_world_data, model="ZZZ", damped=FALSE)
fit_ZZZ_damped <- ets(ts_world_data, model="ZZZ", damped=TRUE)
fit_AAN <- ets(ts_world_data, model="AAN", damped=FALSE) # Additive noise, Additive Trend, No seasonality
fit_AAN_damped <- ets(ts_world_data, model="AAN", damped=TRUE) # Additive noise, Additive Trend, No seasonality
fit_AAZ <- ets(ts_world_data, model="AAZ", damped=FALSE) # Additive Noise, Additive Trend, Automatic Seasonality
fit_AAZ_damped <- ets(ts_world_data, model="AAZ", damped=TRUE) # Additive Noise, Additive Trend, Automatic Seasonality
fit_us_AAZ <- ets(ts_us_data, model="AAZ", damped=FALSE) # Additive Noise, Additive Trend, Automatic Seasonality
fit_us_AAZ_damped <- ets(ts_us_data, model="AAZ", damped=TRUE) # Additive Noise, Additive Trend, Automatic Seasonality

fit_MAZ <- ets(ts_world_data, model="MAZ", damped=FALSE) 
fit_MAZ_damped <- ets(ts_world_data, model="MAZ", damped=TRUE) 

fit_MMN <- ets(ts_world_data, model="MMN", damped=FALSE)
fit_MMN_damped <- ets(ts_world_data, model="MMN", damped=TRUE)
fit_us_MMN <- ets(ts_us_data, model="MMN", damped=FALSE)
fit_us_MMN_damped <- ets(ts_us_data, model="MMN", damped=TRUE)

fit_MMZ <- ets(ts_world_data, model="MMZ", damped=FALSE)
fit_MMZ_damped <- ets(ts_world_data, model="MMZ", damped=TRUE)
fit_TBATS <- tbats(ts_world_data)

fit_arima <- auto.arima(ts_world_data) #automatically fits the ARIMA model (auto-regressive integrated moving average)
fit_arima <- auto.arima(ts_world_data, seasonal = TRUE)

pred_drift <- forecast(fit_drift)

pred_ZZZ <- forecast(fit_ZZZ, h=horizon, level=level)
pred_ZZZ_damped <- forecast(fit_ZZZ_damped, h=horizon, level=level)
pred_AAN <- forecast(fit_AAN, h=horizon, level=level)
pred_AAN_damped <- forecast(fit_AAN_damped, h=horizon, level=level)

pred_AAZ <- forecast(fit_AAZ, h=horizon, level=level)
pred_AAZ_damped <- forecast(fit_AAZ_damped, h=horizon, level=level)
pred_us_AAZ <- forecast(fit_us_AAZ, h=horizon, level=level)
pred_us_AAZ_damped <- forecast(fit_us_AAZ_damped, h=horizon, level=level)

pred_MAZ <- forecast(fit_MAZ, h=horizon, level=level)
pred_MAZ_damped <- forecast(fit_MAZ_damped, h=horizon, level=level)


pred_MMN <- forecast(fit_MMN, h=horizon, level=level)
pred_MMN_damped <- forecast(fit_MMN_damped, h=horizon, level=level)
pred_us_MMN <- forecast(fit_us_MMN, h=horizon, level=level)
pred_us_MMN_damped <- forecast(fit_us_MMN_damped, h=horizon, level=level)

pred_MMZ <- forecast(fit_MMZ, h=horizon, level=level)
pred_MMZ_damped <- forecast(fit_MMZ_damped, h=horizon, level=level)
#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) models
pred_tbats <- forecast(fit_TBATS, h=horizon, level=level)
pred_arima <- forecast(fit_arima, h=horizon, level=level)
# $fitted is existing values while $mean are forecasted results

plot_drift<-autoplot(fit_drift, xlab="Year", ylab="Drift Benchmark") + scale_y_continuous(labels=comma) + labs(colour = "legend title") 

plot_drift

plot_zzz<-autoplot(pred_ZZZ, xlab="Year", ylab="Predicted 'ZZZ'") + scale_y_continuous(labels=comma)
plot_zzz_damped<-autoplot(pred_ZZZ_damped, xlab="Year", ylab="Predicted 'ZZZ+damped'") + scale_y_continuous(labels=comma)
grid.arrange(grobs=list(plot_zzz,plot_zzz_damped), ncol=2)

plot_AAN<-autoplot(pred_AAN, xlab="Year", ylab="Predicted 'AAN'") + scale_y_continuous(labels=comma)
plot_AAN_damped<-autoplot(pred_AAN_damped, xlab="Year", ylab="Predicted 'AAN+damped'") + scale_y_continuous(labels=comma)
grid.arrange(grobs=list(plot_AAN,plot_AAN_damped), ncol=2)

plot_AAZ<-autoplot(pred_AAZ, xlab="Year", ylab="Predicted 'AAZ'") + scale_y_continuous(labels=comma)
plot_AAZ_damped<-autoplot(pred_AAZ_damped, xlab="Year", ylab="Predicted 'AAZ+damped'") + scale_y_continuous(labels=comma)
plot_MAZ<-autoplot(pred_MAZ, xlab="Year", ylab="Predicted 'MAZ'") + scale_y_continuous(labels=comma)
plot_MAZ_damped<-autoplot(pred_MAZ_damped, xlab="Year", ylab="Predicted 'MAZ+damped'") + scale_y_continuous(labels=comma)


plot_MMN<-autoplot(pred_MMN, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)
plot_MMN_damped<-autoplot(pred_MMN_damped, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)

plot_MMZ <- autoplot(pred_MMZ, xlab="Year", ylab="Predicted 'MMZ'") + scale_y_continuous(labels=comma)
plot_MMZ <- autoplot(pred_MMZ_damped, xlab="Year", ylab="Predicted 'MMZ+damped'") + scale_y_continuous(labels=comma)

plot_tbats <- autoplot(pred_tbats, xlab="Year", ylab="Predicted 'TBATS'") + scale_y_continuous(labels=comma)
plot_arima <- autoplot(pred_arima, xlab="Year", ylab="Predicted 'ARIMA'") + scale_y_continuous(labels=comma)

# Global view of our models
lay <- rbind(c(1,1),
             c(2,3),
             c(4,5),
             c(6,7),
             c(8,9),
             c(10,11))
grid.arrange(grobs=list(plot_drift, plot_zzz,plot_zzz_damped, plot_AAZ,plot_AAZ_damped,plot_MAZ, plot_MAZ_damped, plot_MMN, plot_MMN_damped, plot_arima, plot_tbats), ncol=2, nrow=6, layout_matrix=lay)
################################
################################
################################

### VALUATION automation function to quickly check the forecast instead of going through excel
valuation <- function(prediction, debug) {
  
  if(missing(debug)) {
    debug<-FALSE  
  }
  
  cashFlows<-c()

  # Initial us population ratio 33%
  usPeopleRatio <-0.33
  # decline in US vs World ratio per year
  declineUSRatio<-0.0167 
  peopleToUserRatio<-2
  engagementMultiplier <- 4.5
  usRevenuePerUser <- 13.58
  rowRevenuePerUser <- 3.21
  usRevenueIncrease <- 0.3
  rowRevenueIncrease <- 0.03
  cashMarginPercent <- 0.317 # 31.7% based on Facebook 2012 annual report
  discountRate <- 0.1 # 10%
  perpetuityGrowthRate <- 0.03 # 3%
  
  for(year in 2013:2022) {
    # Use a variable for begin of year to tell the first month of our prediction in 2013
    beginOfYearMonth <- 1
    endOfYearMonth <- 12
    
    if(year==2013) { 
      # According to Yahoo forecast we only average user values from June to Dec for 2013
      beginOfYearMonth <- 6
    } else {
      # Don't increase Revenue per User on Year1
      usRevenuePerUser <- usRevenuePerUser+usRevenueIncrease
      rowRevenuePerUser <- rowRevenuePerUser+rowRevenueIncrease
      # Adjust ratio for the current year
      usPeopleRatio <- usPeopleRatio - declineUSRatio
    }
    
    # Compute year values adjusted by 10,000,000 people
    peoplePerYear <- mean(window(prediction, c(year,beginOfYearMonth), c(year,endOfYearMonth))) / 1000000
    usPeoplePerYear <- peoplePerYear*usPeopleRatio 
    rowPeoplePerYear <- peoplePerYear-usPeoplePerYear 
    usUsers <- usPeoplePerYear / peopleToUserRatio
    rowUsers <- rowPeoplePerYear / peopleToUserRatio
    
    revenue <- (usRevenuePerUser * usUsers + rowRevenuePerUser * rowUsers) / engagementMultiplier
    if(year==2013) {
      # Adjust revenue on 2013 because we only have 7months of data
      revenue<-revenue * 7/12
    }
    
    operatingCF <- revenue * cashMarginPercent
    terminalValue <- 0
    
    if(year==2022) {
      # Compute terminal value and add it to the operating cash flows
      terminalValue <- operatingCF * (1+perpetuityGrowthRate) /(discountRate-perpetuityGrowthRate) 
      # cat(sprintf(" terminalValue = %s \n ", terminalValue ))
    } 
    totalCF <- operatingCF + terminalValue
    
    if(debug) {
      cat(sprintf("################ %d ##############\n", year))
      cat(sprintf(" people Worldwide=%s \n Percentage US People=%s%% \n usPeoplePerYear=%s \n rowPeoplePerYear=%s \n usRevenuePerUser=%s \n rowRevenuePerUser=%s \n usUsers=%s \n rowUsers=%s \n Revenue=%s \n Operating Cash Flows = %s \n Terminal Value = %s \n Total Cash Flows = %s \n", peoplePerYear, round(usPeopleRatio*100), usPeoplePerYear, rowPeoplePerYear, usRevenuePerUser, rowRevenuePerUser, usUsers, rowUsers, revenue, operatingCF, terminalValue, totalCF))
    }
    cashFlows<-append(cashFlows, totalCF)
  }
  
  require("FinancialMath")
  # cfs<-c(20,39,46,54,64,76,90,105,124,2277)
  firmValue<-NPV(cf0=0, cf=cashFlows, times=c(1,2,3,4,5,6,7,8,9,10), i=discountRate, plot=FALSE)
  return(cbind(firmValue=firmValue))
}

valuationAdvanced <- function(predictionWorld, predictionUS, debug) {
  
  if(missing(debug)) {
    debug<-FALSE  
  }
  
  cashFlows<-c()
  
  peopleToUserRatio<-2
  engagementMultiplier <- 4.5
  usRevenuePerUser <- 13.58
  rowRevenuePerUser <- 3.21
  usRevenueIncrease <- 0.3
  rowRevenueIncrease <- 0.03
  cashMarginPercent <- 0.317 # 31.7% based on Facebook 2012 annual report
  discountRate <- 0.1 # 10%
  perpetuityGrowthRate <- 0.03 # 3%
  
  for(year in 2013:2022) {
    
    # Use a variable for begin of year to tell the first month of our prediction in 2013
    beginOfYearMonth <- 1
    endOfYearMonth <- 12
    
    if(year==2013) { 
      # According to Yahoo forecast we only average user values from June to Dec for 2013
      beginOfYearMonth <- 6
    } else {
      # Don't increase Revenue per User on Year1
      usRevenuePerUser <- usRevenuePerUser+usRevenueIncrease
      rowRevenuePerUser <- rowRevenuePerUser+rowRevenueIncrease
    }
    
    # Compute year values adjusted by 1,000,000 people
    peoplePerYear <- round(mean(window(predictionWorld, c(year,beginOfYearMonth), c(year,endOfYearMonth))) / 1000000)
    usPeoplePerYear <- round(mean(window(predictionUS, c(year,beginOfYearMonth), c(year,endOfYearMonth))) / 1000000)
    rowPeoplePerYear <- peoplePerYear-usPeoplePerYear 
    usUsers <- usPeoplePerYear / peopleToUserRatio
    rowUsers <- rowPeoplePerYear / peopleToUserRatio
    
    revenue <- round((usRevenuePerUser * usUsers + rowRevenuePerUser * rowUsers) / engagementMultiplier, 0)
    if(year==2013) {
      # Adjust revenue on 2013 because we only have 7months of data
      revenue<-revenue * 7/12
    }
    
    operatingCF <- round(revenue * cashMarginPercent)
    terminalValue <- 0
    
    if(year==2022) {
      # Compute terminal value and add it to the operating cash flows
      terminalValue <- operatingCF * (1+perpetuityGrowthRate) /(discountRate-perpetuityGrowthRate) 
      # cat(sprintf(" terminalValue = %s \n ", terminalValue ))
    } 
    totalCF <- operatingCF + terminalValue
    
    if(debug) {
      cat(sprintf("################ %d ##############\n", year))
      cat(sprintf(" people Worldwide=%s \n Percentage US People=%s%% \n usPeoplePerYear=%s \n rowPeoplePerYear=%s \n usRevenuePerUser=%s \n rowRevenuePerUser=%s \n Revenue=%s \n Operating Cash Flows = %s \n Terminal Value = %s \n Total Cash Flows = %s \n", peoplePerYear, round(usPeoplePerYear*100/peoplePerYear), usPeoplePerYear, rowPeoplePerYear, usRevenuePerUser, rowRevenuePerUser, revenue, operatingCF, terminalValue, totalCF))
    }
    cashFlows<-append(cashFlows, totalCF)
  }
  
  require("FinancialMath")
  # cfs<-c(20,39,46,54,64,76,90,105,124,2277)
  firmValue<-NPV(cf0=0, cf=cashFlows, times=c(1,2,3,4,5,6,7,8,9,10), i=discountRate, plot=FALSE)
  return(cbind(firmValue=firmValue))
}

#######################################
############ VALUATION ################
#######################################

valuation(ts_yahoo_forecast, debug=FALSE) #  1230

valuation(pred_drift$mean) # 930
valuation(pred_ZZZ$mean) # 703

valuation(pred_AAZ$mean) # 1004
valuation(pred_AAZ_damped$mean) # 485

valuation(pred_AAZ$mean) # 1004
valuation(pred_AAZ_damped$mean) # 485
valuationAdvanced(pred_AAZ$mean, pred_us_AAZ$mean, debug=TRUE) # 1338

valuation(pred_MMZ$mean) # 546
valuation(pred_MMN_damped$mean) # 513

valuationAdvanced(pred_MMN$mean, pred_us_MMN$mean) # 515
valuationAdvanced(pred_MMN$mean, pred_us_MMN_damped$mean) # 620

valuation(pred_arima$mean) # 515
valuation(pred_tbats$mean) # 454

# Arrange the data for plotting
data <- data.frame(Date=time(ts_world_audience), People=data_monthly_audience$People)
ts_yahoo_small <- window(ts_yahoo_forecast, start=c(2013,6))
yahoo <- data.frame(Date=time(ts_yahoo_small), People=ts_yahoo_small[,1])
drift <- data.frame(Date=time(pred_drift$mean), People=fit_drift$mean)

pred_MMN_damped$mean
time(pred_MMN_damped)
time(pred_MMN_damped$mean)
mmn_damped <- data.frame(Date=time(pred_MMN_damped$mean), People=pred_MMN_damped$mean)
arima = data.frame(Date=time(pred_arima$mean), People=pred_arima$mean)
tbats = data.frame(Date=time(pred_tbats$mean), People=pred_tbats$mean)

ggplot() +  
  geom_line(data = data, aes(y=People, x=Date)) +
  geom_line(data=yahoo, aes(y=People, x=Date, color="Yahoo - DCF=1,230 Million")) +
  geom_line(data=drift, aes(y=People, x=Date, color="Drift - DCF=930 Million")) +
  geom_line(data=mmn_damped, aes(y=People, x=Date, color="MMNdamped - DCF=513 Million")) +
  geom_line(data=arima, aes(y=People, x=Date, color="ARIMA - DCF=515 Million")) +
  geom_line(data=tbats, aes(y=People, x=Date, color="TBATS - DCF=454 Million")) +
  scale_y_continuous(labels=comma) +
  ggtitle("Tumblr Valuation Scenarios") + xlab("") + ylab("")
  
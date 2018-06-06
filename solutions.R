install.packages("FinancialMath")
install.packages("rmarkdown")

require("forecast")
require("FinancialMath")
require(ggplot2)
require("scales")
require("xts")
require(grid)
require(gridExtra)

theme_set(theme_minimal())
# cleanup env
rm(list = ls())

data_monthly_audience <- read.csv("tumblr_worldwide_monthly_direct_audience.csv", header = TRUE)
data_yahoo_forecast <-read.csv("model_pop_forecasts.csv", header = FALSE)

ts_monthy_audience <- ts(data_monthly_audience, start=c(2010, 4), frequency=12) 
ts_yahoo_forecast <- ts(data_yahoo_forecast, start=c(2010, 4), frequency=12) 
plot(ts_monthy_audience)
data_yahoo_forecast

# For the following we will only focus on the "People" WolrdWide part of our data because the financial valuation is based on this number and its forecasts.
tsdata<-ts_monthy_audience[,"People"]
tsdata
ts_yahoo_forecast

autoplot(tsdata) + ggtitle("People Worldwide unsing Tumblr") + xlab("Month") + ylab("Thousands")
ggsubseriesplot(tsdata) + ggtitle("People Worldwide unsing Tumblr") + xlab("Month") + ylab("Thousands")

# We can see an upward trend and no seasonality in the plot.
# Let's confirm this by doing decompositions of the time series
#plot various decompositions into error/noise, trend and seasonality
fit_classical_multiplicative <- decompose(tsdata, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit_classical_multiplicative)
fit_classical_additive <- decompose(tsdata, type="additive") #decompose using "classical" method, additive form
plot(fit_classical_additive)
fit_stl <- stl(tsdata, t.window=12, s.window="periodic", robust=TRUE) #decompose using STL (Season and trend using Loess)
plot(fit_stl)
# Now we notice a pattern in the seasonal plot, what does it mean?
# Most likely students will use less the application during the summer when they are not in school

# One thing to worry about is the decrease trend at the end. Should it be ignored?

# let's start our forecast with some benchmarking values using simple methods
horizon <- 7 + 9*12 # 7months in 2013 + 9 years from 2014 to 2022

autoplot(tsdata) +
  autolayer(meanf(tsdata, h=horizon),
            series="Mean", PI=FALSE) +
  autolayer(rwf(tsdata, h=horizon),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(tsdata, drift=TRUE, h=horizon),
            series="Drift", PI=FALSE) +
  ggtitle("People Worldwide unsing Tumblr") +
  xlab("Month") + ylab("Thousands") +
  guides(colour=guide_legend(title="Forecast"))

# several exponential smoothing models to forecast the increase in the traffic
damped<-TRUE
#level<-c(0.2, 0.4, 0.6, 0.8) # what is this for???  
level<-c(0.8,0.95) # what is this for???  
?ets
"AAN damped=false"
fit_drift <- rwf(tsdata, drift=TRUE, h=horizon)
fit_mean <- meanf(tsdata, h=horizon)
fit_naive <- rwf(tsdata, h=horizon)
fit_ZZZ <- ets(tsdata, model="ZZZ", damped=FALSE)
fit_ZZZ_damped <- ets(tsdata, model="ZZZ", damped=TRUE)
fit_AAN <- ets(tsdata, model="AAN", damped=FALSE) # Additive noise, Additive Trend, No seasonality
fit_AAN_damped <- ets(tsdata, model="AAN", damped=TRUE) # Additive noise, Additive Trend, No seasonality
fit_AAZ <- ets(tsdata, model="AAZ", damped=FALSE) # Additive Noise, Additive Trend, Automatic Seasonality
fit_AAZ_damped <- ets(tsdata, model="AAZ", damped=TRUE) # Additive Noise, Additive Trend, Automatic Seasonality
fit_MMN <- ets(tsdata, model="MMN", damped=FALSE)
fit_MMN_damped <- ets(tsdata, model="MMN", damped=TRUE)
fit_MMZ <- ets(tsdata, model="MMZ", damped=FALSE)
fit_MMZ_damped <- ets(tsdata, model="MMZ", damped=TRUE)
fit_TBATS <- tbats(tsdata)

pred_drift <- forecast(fit_drift)
pred_mean <- forecast(fit_mean)
pred_naive <- forecast(fit_naive)
pred_ZZZ <- forecast(fit_ZZZ, h=horizon, level=level)
pred_ZZZ_damped <- forecast(fit_ZZZ_damped, h=horizon, level=level)
pred_AAN <- forecast(fit_AAN, h=horizon, level=level)
pred_AAN_damped <- forecast(fit_AAN_damped, h=horizon, level=level)
pred_AAZ <- forecast(fit_AAZ, h=horizon, level=level)
pred_AAZ_damped <- forecast(fit_AAZ_damped, h=horizon, level=level)
pred_MMN <- forecast(fit_MMN, h=horizon, level=level)
pred_MMN_damped <- forecast(fit_MMN_damped, h=horizon, level=level)
pred_MMZ <- forecast(fit_MMZ, h=horizon, level=level)
pred_MMZ_damped <- forecast(fit_MMZ_damped, h=horizon, level=level)
#Create a trigonometric box-cox autoregressive trend seasonality (TBATS) models
pred_tbats <- forecast(fit_TBATS, h=horizon, level=level)
# $fitted is existing values while $mean are forecasted results

plot_drift<-autoplot(fit_drift)
plot_mean<-autoplot(fit_mean)
plot_naive<-autoplot(fit_naive)
grid.arrange(grobs=list(plot_drift, plot_mean, plot_naive)) 

plot_zzz<-autoplot(pred_ZZZ, xlab="Year", ylab="Predicted 'ZZZ'") + scale_y_continuous(labels=comma)
plot_zzz_damped<-autoplot(pred_ZZZ_damped, xlab="Year", ylab="Predicted 'ZZZ+damped'") + scale_y_continuous(labels=comma)
grid.arrange(grobs=list(plot_zzz,plot_zzz_damped), ncol=2)

plot_AAN<-autoplot(pred_AAN, xlab="Year", ylab="Predicted 'AAN'") + scale_y_continuous(labels=comma)
plot_AAN_damped<-autoplot(pred_AAN_damped, xlab="Year", ylab="Predicted 'AAN+damped'") + scale_y_continuous(labels=comma)
grid.arrange(grobs=list(plot_AAN,plot_AAN_damped), ncol=2)

plot_AAZ<-autoplot(pred_AAZ, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)
plot_AAZ_damped<-autoplot(pred_AAZ_damped, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)

plot_MMN<-autoplot(pred_MMN, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)
plot_MMN_damped<-autoplot(pred_MMN_damped, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)

autoplot(pred_MMZ, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)
autoplot(pred_tbats, xlab="Year", ylab="Predicted 'People'") + scale_y_continuous(labels=comma)





valuation(ts_yahoo_forecast, debug=TRUE)

year<-2013
beginOfYearMonth<-6
endOfYearMonth<-12

pred_AAN$mean
#mean(window(ts_yahoo_forecast, start=c(year,beginOfYearMonth), end=c(year, endOfYearMonth) ))
#mean(window(pred_AAN$mean, start=c(year,beginOfYearMonth), end=c(year, endOfYearMonth) ))

valuation(pred_drift$mean)
valuation(pred_ZZZ$mean)
valuation(pred_AAZ$mean)
valuation(pred_MMN$mean)
valuation(pred_MMZ$mean)

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
    # Adjust ratio for the current year
    usPeopleRatio <- usPeopleRatio - declineUSRatio
    
    # Use a variable for begin of year to tell the first month of our prediction in 2013
    beginOfYearMonth <- 1
    endOfYearMonth <- 12
    
    if(year==2013) { 
      # According to Yahoo forecast we only average user values from June to Dec for 2013
      beginOfYearMonth <- 6
      sub<-window(prediction, c(year,beginOfYearMonth), c(year,endOfYearMonth))
      m<-mean(mean(sub))
      # cat(sprintf("year=%s && begin=%s && end=%s && sub=%s && mean=%s\n", year, beginOfYearMonth, endOfYearMonth, sub, m))
    } else {
      # Don't increase Revenue per User on Year1
      usRevenuePerUser <- usRevenuePerUser+usRevenueIncrease
      rowRevenuePerUser <- rowRevenuePerUser+rowRevenueIncrease
    }
    
    # Compute year values adjusted by 10,000,000 people
    peoplePerYear <- round(mean(window(prediction, c(year,beginOfYearMonth), c(year,endOfYearMonth))) / 1000000)
    usPeoplePerYear <- round(peoplePerYear*usPeopleRatio )
    rowPeoplePerYear <- peoplePerYear-usPeoplePerYear 
    usUsers <- usPeoplePerYear / peopleToUserRatio
    rowUsers <- rowPeoplePerYear / peopleToUserRatio
  
    revenue <- round((usRevenuePerUser * usUsers + rowRevenuePerUser * rowUsers) / engagementMultiplier, 0)
   
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
      cat(sprintf(" people Worldwide=%s \n Percentage US People=%s%% \n usPeoplePerYear=%s \n rowPeoplePerYear=%s \n usRevenuePerUser=%s \n rowRevenuePerUser=%s \n Revenue=%s \n Operating Cash Flows = %s \n Terminal Value = %s \n Total Cash Flows = %s \n", peoplePerYear, round(usPeopleRatio*100), usPeoplePerYear, rowPeoplePerYear, usRevenuePerUser, rowRevenuePerUser, revenue, operatingCF, terminalValue, totalCF))
    }
    cashFlows<-append(cashFlows, totalCF)
  }
  
  require("FinancialMath")
  # cfs<-c(20,39,46,54,64,76,90,105,124,2277)
  firmValue<-NPV(cf0=0, cf=cashFlows, times=c(1,2,3,4,5,6,7,8,9,10), i=discountRate, plot=FALSE)
  return(cbind(firmValue=firmValue))
}


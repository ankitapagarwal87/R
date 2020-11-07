#Reading Walmart Data and loading libraries

rm(list=ls())

library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)
library(forecast)
library(tseries)

setwd("E:\\Simplilearn\\Data Science with R\\Project\\Walmart")

walmart<- read.csv("Walmart_Store_sales.csv")
head(walmart)

# Basic Statistics tasks: -

#conversion of date into date format

walmart$Date<- as.Date(walmart$Date, format = "%d-%m-%Y")
head(walmart)

# 1. Which store has maximum sales. 

max_sales_store <- aggregate(Weekly_Sales ~ Store, FUN = sum, data = walmart)
max_sales_store[(max_sales_store$Weekly_Sales == max(max_sales_store$Weekly_Sales)),]

# 2. Which store has a maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation.

mean_sales<-mean(walmart$Weekly_Sales)
sd_sales<-sd(walmart$Weekly_Sales)
zscore<- (walmart$Weekly_Sales-mean_sales)/sd_sales

max_sd_store <- aggregate(Weekly_Sales ~ Store, FUN = sd, data = walmart)
max_sd_store[(max_sd_store$Weekly_Sales == max(max_sd_store$Weekly_Sales)),]

max_zscore_store <- aggregate(zscore ~ Store, FUN = max, data = walmart)
max_zscore_store[(max_zscore_store$zscore == max(max_zscore_store$zscore)),]

Coefficient_variation<-sd_sales/mean_sales*100
Coefficient_variation

# 3. Which store/s has a good quarterly growth rate in Q3'2012.

yq<-as.yearqtr(walmart$Date)
head(format(yq, format = "%y/0%q"))

max_sales_qtr <- aggregate(Weekly_Sales ~ Store+yq, FUN = sum, data = walmart)
max_sales_qtr[(max_sales_qtr$Weekly_Sales == max(max_sales_qtr$Weekly_Sales)),]

qtrdata = max_sales_qtr %>% 
  group_by(Store) %>%
  arrange(yq) %>%
  mutate(qOverq=Weekly_Sales/lag(Weekly_Sales,1))

subset_Qtr3 <- subset(qtrdata, yq == "2012 Q3")

max_sales_Q3_2012 <- aggregate(qOverq ~ Store+yq, FUN = max, data = subset_Qtr3)
max_sales_Q3_2012[(max_sales_Q3_2012$qOverq == max(max_sales_Q3_2012$qOverq)),]

# 4. Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in a non-holiday season for all stores together.

Nonholiday<- subset(walmart, Holiday_Flag == FALSE)
mean_nonholiday<-mean(Nonholiday$Weekly_Sales)
mean_nonholiday

Holiday<- subset(walmart, Holiday_Flag == TRUE)
diff_meansales<-Holiday$Weekly_Sales-mean_nonholiday

Holiday<-cbind(Holiday,diff_meansales)
Holiday_meansales <- aggregate(diff_meansales ~ Date, FUN = sum, data = Holiday)

max_meansales_date <- Holiday_meansales [with(Holiday_meansales ,order(-diff_meansales)),]
max_meansales_date <- max_meansales_date[1:10,]
max_meansales_date

# 5. Provide a monthly and semester view of sales in units and give insights.

monthly<- as.yearmon(walmart$Date, format = "%d-%m-%Y")
monthly<-cbind(walmart,monthly)

monthmax<- aggregate(Weekly_Sales ~ Store+monthly, FUN = sum, data = monthly)
monthmin<- aggregate(Weekly_Sales ~ Store+monthly, FUN = sum, data = monthly)
monthmax1<- aggregate(Weekly_Sales ~ monthly, FUN = sum, data = monthly)
monthmin1<- aggregate(Weekly_Sales ~ monthly, FUN = sum, data = monthly)

monthmax[(monthmax$Weekly_Sales == max(monthmax$Weekly_Sales)),]
monthmin[(monthmin$Weekly_Sales == min(monthmin$Weekly_Sales)),]
monthmax1[(monthmax1$Weekly_Sales == max(monthmax1$Weekly_Sales)),]
monthmin1[(monthmin1$Weekly_Sales == min(monthmin1$Weekly_Sales)),]

#semester

dt<- as.yearmon(walmart$Date, format = "%d-%m-%Y")

Date2period <- function(x = walmart$Date, period = 6, sep = " S") {
  paste(as.integer(dt), (cycle(dt) - 1) %/% period + 1, sep = sep)
}

semester <- Date2period(walmart$Date)
semester1<-cbind(walmart,semester)

semestermax<- aggregate(Weekly_Sales ~ Store+semester, FUN = sum, data = semester1)
semestermin<- aggregate(Weekly_Sales ~ Store+semester, FUN = sum, data = semester1)
semestermax1<- aggregate(Weekly_Sales ~ semester, FUN = sum, data = semester1)
semestermin1<- aggregate(Weekly_Sales ~ semester, FUN = sum, data = semester1)

semestermax[(semestermax$Weekly_Sales == max(semestermax$Weekly_Sales)),]
semestermin[(semestermin$Weekly_Sales == min(semestermin$Weekly_Sales)),]
semestermax1[(semestermax1$Weekly_Sales == max(semestermax1$Weekly_Sales)),]
semestermin1[(semestermin1$Weekly_Sales == min(semestermin1$Weekly_Sales)),]

# Statistical Model: - 

# For Store 1 - Build prediction models to forecast demand

# 6. Linear Regression - Utilize variables like date and restructure dates as 1 for 5 Feb 2010(starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.

Store1<- subset(walmart, Store == 1)
Store1<- Store1 %>% mutate(WeekNum = 1:n())
head(Store1%>%select(WeekNum,Date,Weekly_Sales))

model<-lm(Weekly_Sales ~ CPI+Unemployment+Fuel_Price, data = Store1)
summary (model)

# 7. Time series forecasting model - Hypothesize if the data is fit for time series analysis - check for white noise probability test
# 8. Make adjustments in historical data for events like holidays, if applicable
# 9. Build ARIMA model to forecast 6 months i.e., input utilize only till April 2012.
# 10. Predict next 6 months i.e., June to Oct 2010. Check for MAPE.
# 11. Select the model which gives best accuracy.

# Time series forecasting model:-

ts_data <- ts(Store1$Weekly_Sales,start=c(2010,6), end=c(2012,43), frequency = 52)
ts_data
plot.ts(ts_data)
abline(reg = lm(ts_data~time(ts_data)))
plot(decompose(ts_data))

kpss.test(ts_data)
adf.test(ts_data,alternative="stationary")

nsdiffs(ts_data)
ndiffs(ts_data)

kpss.test(diff(log(ts_data)))
adf.test(diff(log(ts_data)),alternative="stationary")

Acf(diff(log(ts_data)), lag.max=52,plot=TRUE, main = "ACF Plot")
Pacf(diff(log(ts_data)),lag.max=52,plot=TRUE, main = "PACF Plot")

### Preparing Train and Test data.

train_sales<-window(diff(log(ts_data)),start=c(2010,6),end=c(2012,17),frequency=52)
test_sales<-window(diff(log(ts_data)),start=c(2012,18),end=c(2012,43), frequency=52)

# Fit the ARMA model

fit = auto.arima(train_sales)
summary(fit)

box<-Box.test(residuals(fit),lag=52,type="Ljung-Box")
box

# Predict using the model

pred <- forecast(fit,h=25)
plot(pred,main="Predicition from Auto ARIMA for Weekly Sales")

lines(test_sales,col="Red",lwd=3)

accuracy(pred,test_sales)

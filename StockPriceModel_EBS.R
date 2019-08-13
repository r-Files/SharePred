#Financial data from Yahoo! Finance - also refer to finance.yahoo.com/lookup

#############################################################
#############      Necessary packages:      #################
if(!("pacman" %in% installed.packages()[,"Package"])) install.packages("pacman")
pacman::p_load(quantmod, ggplot2, magrittr, forecast, lubridate, tseries)
#############################################################

#############################################################
#############      Parameter definition:    #################
p.value <- 0.05
train.test.split <- 0.8
ts_sampling_per <- 'monthly'
ts_sampling_meth <- 'mean'
start_date <- as.Date("2009-12-24")
end_date <- as.Date(today(tzone="GMT"))
transformation <- 1
forecast_error_hor <- 3
#############################################################

#Get the index value of the DAX (=performance index) from Yahoo Finance
#The object 'GDAXI' will be of type xts and contain 6 columns (5 price types + volume)
getSymbols("^GDAXI", src = "yahoo", from = start_date, to = end_date)
summary(GDAXI)

sum(is.na(GDAXI))
#remove NA (happens e.g. Tag der Dt. Einheit --> no big loss)
GDAXI<-na.omit(GDAXI)

#Plot for first rough check
plot(GDAXI[, "GDAXI.Close"], main = "DAX")

#Forecast time series: Aggregation of close price as specified
GDAXI_close <- GDAXI[,4]
switch(ts_sampling_per,
       weekly = GDAXI_close <- apply.weekly(GDAXI_close, ts_sampling_meth),
       daily = GDAXI_close <-GDAXI_close,
       monthly = GDAXI_close <-apply.monthly(GDAXI_close, ts_sampling_meth))

#Convert the xts object to the ts class:
DAX.end <- floor(train.test.split*length(GDAXI_close)) #select the first x% of the data
switch(ts_sampling_per,
       weekly = pred <- c(freq = 52, start.train=c(year(start(GDAXI_close)),week(start(GDAXI_close))), start.test=c(year(start(GDAXI_close[DAX.end+1,])),week(start(GDAXI_close[DAX.end+1,])))),
       daily =  pred <- c(freq = 365, start.train=c(year(start(GDAXI_close)),day(start(GDAXI_close))), start.test=c(year(start(GDAXI_close[DAX.end+1,])),day(start(GDAXI_close[DAX.end+1,])))),
       monthly = pred <- c(freq = 12, start.train=c(year(start(GDAXI_close)),month(start(GDAXI_close))), start.test=c(year(start(GDAXI_close[DAX.end+1,])),month(start(GDAXI_close[DAX.end+1,])))))

DAX <- ts(as.numeric(GDAXI_close),start=c(pred[2],pred[3]),frequency = pred[1])
try(if(class(DAX) != "ts") stop("Conversion into Time Series failed"))

##Test formally for stationarity
adf.test.stat <- tseries::adf.test(DAX,alternative="stationary") #Check if there is a trend in the TS

if(adf.test.stat$p.value > p.value && transformation){
  DAX <- log(DAX)
  DAX <- diff(DAX, lag = 1)
  plot(DAX)
  adf.test.stat.reform <- tseries::adf.test(DAX,alternative="stationary") #Check if TS is stationary
}
try(if(adf.test.stat.reform$p.value > p.value) stop("Time Series couldn't be made stationary"))

#Split time series in train and test set --> as usual with Time Series, the chronologically first x% of data points is the train set
DAX.train <- ts(DAX[1:DAX.end], start = c(pred[2],pred[3]), frequency = pred[1]) #assign the first x% of the data to the train set
DAX.test <- ts(DAX[(DAX.end+1):length(DAX)], start = c(pred[4],pred[5]), frequency = pred[1]) #assign the most recent (1-x)% to the test set

#Check if the data separation went well - if not, interrupt
test <- length(DAX) - (length(DAX.test)+length(DAX.train))
try(if(test != 0) stop("Train and test set do not make the total set"))
try(if(end(DAX.test)[1] != year(today()) || end(DAX.test)[2] != week(today())) stop("Date of object ts not consistent") )

#Holt-Winter forecast
forecast.HW <- HoltWinters(DAX.train,alpha=TRUE, beta = FALSE, gamma = TRUE, seasonal = "additive") 
forecast.DAX <- forecast(forecast.HW,h=length(DAX.test))
plot(forecast.DAX,ylim=c(-0.5,0.5))
lines(DAX.test, col = "red")
hw.mse <- 1/forecast_error_hor * sum((DAX.test[1:forecast_error_hor]-forecast.DAX$mean[1:forecast_error_hor])^2)

#ARIMA forecast
forecast.model.arima <- auto.arima(DAX.train)
forecast.DAX <- forecast(forecast.model.arima,h=length(DAX.test))
plot(forecast.DAX)#,ylim=c(-0.5,0.5))
lines(DAX.test, col = "red")
arima.mse <- 1/forecast_error_hor * sum((DAX.test[1:forecast_error_hor]-forecast.DAX$mean[1:forecast_error_hor])^2)

#ets forecast
forecast.expsmoothingsstate <- ets(DAX.train,model="ANA",opt.crit = "mse")
forecast.DAX <- forecast(forecast.expsmoothingsstate,h=length(DAX.test))
plot(forecast.DAX)#,ylim=c(-0.5,0.5))
lines(DAX.test, col = "red")
ets.mse <- 1/forecast_error_hor * sum((DAX.test[1:forecast_error_hor]-forecast.DAX$mean[1:forecast_error_hor])^2)

mse.res <- c(hw.mse,arima.mse,ets.mse)

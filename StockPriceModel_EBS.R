#Financial data from Yahoo! Finance - also refer to finance.yahoo.com/lookup

#############################################################
#############      Parameter definition:    #################
list.of.packages <- c("quantmod", "ggplot2","magrittr","forecast","lubridate","tseries")
ts_sampling_per <- 'weekly'
ts_sampling_meth <- 'mean'
start_date <- as.Date("2009-12-24")
end_date <- as.Date(today(tzone="GMT"))
#############################################################

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library('quantmod')
library('ggplot2')
library('magrittr')
library('forecast')
library('lubridate')
library('tseries')

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

#Divide data in train and test set --> as usual with Time Series, the chronologically first 80% of data points is the train set
GDAXI_close.end <- floor(0.8*length(GDAXI_close)) #select the first 80% of the data
GDAXI_close.train <- GDAXI_close[1:GDAXI_close.end,] #assign the first 80% of the data to the train set
GDAXI_close.test <- GDAXI_close[(GDAXI_close.end+1):length(GDAXI_close),] #assign the most recent 20% to the test set

#Check if the data separation went well - if not, interrupt
test <- length(GDAXI_close) - (length(GDAXI_close.test)+length(GDAXI_close.train))
try(if(test != 0) stop("Train and test set do not make the total set"))

#Convert the xts object to the ts class:

switch(ts_sampling_per,
       weekly = pred <- c(freq = 52, start.train=c(year(start(GDAXI_close.train)),week(start(GDAXI_close.train))), start.test=c(year(start(GDAXI_close.test)),week(start(GDAXI_close.test)))),
       daily =  pred <- c(freq = 365, start.train=c(year(start(GDAXI_close.train)),day(start(GDAXI_close.train))), start.test=c(year(start(GDAXI_close.test)),day(start(GDAXI_close.test)))),
       monthly = pred <- c(freq = 12, start.train=c(year(start(GDAXI_close.train)),month(start(GDAXI_close.train))), start.test=c(year(start(GDAXI_close.test)),month(start(GDAXI_close.test)))))

DAX <- ts(as.numeric(GDAXI_close),start=c(pred[2],pred[3]),frequency = pred[1])
DAX.train <-ts(as.numeric(GDAXI_close.train), start = c(pred[2],pred[3]), frequency = pred[1])
DAX.test <-ts(as.numeric(GDAXI_close.test), start = c(pred[4],pred[5]), frequency = pred[1])

#Check if conversion went well
try(if(class(DAX.train) != "ts" || class(DAX.test) != "ts") stop("Conversion into Time Series failed"))
try(if(end(DAX.test)[1] != year(today()) || end(DAX.test)[2] != week(today())) stop("Date of object ts not consistent") )

DAX.train.components <- decompose(DAX.train)
plot(DAX.train.components)


##Test formally for stationarity
adf_test_trend <- tseries::adf.test(DAX,alternative="stationary") #Check if there is a trend in the TS
adf_test_vola <- tseries::adf.test(DAX,alternative="explosive") #Check if there is a change in sd over time in TS

##Make the TS stationary
## tbd

#DAX.train%>% 
 #HoltWinters(beta = TRUE, gamma = TRUE) %>% 
  #forecast(h=length(DAX.test)) %>% 
  #plot()
#lines(DAX.test, col = "red")


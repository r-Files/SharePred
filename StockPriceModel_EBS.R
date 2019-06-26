#Financial data from Yahoo! Finance - also refer to finance.yahoo.com/lookup

#install.packages('quantmod')
#install.packages('ggplot2')
#install.packages('magrittr')

library('quantmod')
library('ggplot2')
library('magrittr')

start_date <- as.Date("2010-01-01")
end_date <- as.Date("2019-06-21")

#Get the index value of the DAX (=performance index) from Yahoo Finance
#The object 'GDAXI' will be of type xts and contain 6 columns (5 price types + volume)
getSymbols("^GDAXI", src = "yahoo", from = start_date, to = end_date)
summary(GDAXI)

sum(is.na(GDAXI))
#remove NA (happens e.g. Tag der Dt. Einheit --> no big loss)
na.omit(GDAXI)

#Plot for first rough check
plot(GDAXI[, "GDAXI.Close"], main = "DAX")

#first model
getSymbols("^VIX", src = "yahoo", from = start_date, to = end_date)
sum(is.na(VIX))
plot(VIX[, "VIX.Close"], main = "VIX")

first_mod <- specifyModel(Next(OpCl(GDAXI)) ~ Lag(OpHi(GDAXI),0:3) + Hi(VIX))
buildModel(first_mod,method='lm',training.per=c('2010-01-01','2019-06-20'))




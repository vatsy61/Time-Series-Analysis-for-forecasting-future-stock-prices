install.packages("forecast")
library("forecast")
sink("TimeSeriesAnalysis.txt")
ms <- read.csv("MS.csv")
print(ms)
typeof(ms)
MsPrices = data.frame(ms['Date'],ms['Adj.Close'])
print(MsPrices)
plot(MsPrices)
MS <- which(MsPrices$Date == '2017-01-03')
MsPrices[MS,]
train = MsPrices[1:MS,2]
print(train)
test = MsPrices[MS:60,2]
test
tts <- ts(test,frequency = 12,start = c(2017,2))
tts1 <- ts(test,frequency = 12,start = c(2017,1))
tts
tts1
plot(MsPrices)
lines(MsPrices)
mstsmain <- ts(MsPrices$Adj.Close,frequency = 12,start = c(2013,1))
msts <- ts(train,frequency = 12,start = c(2013,1))
print(msts)
plot.ts(msts)
acf <- acf(msts,lag.max = 20)
pacf <- pacf(msts,lag.max = 20)
station <- Box.test(msts, lag = 20, type = "Ljung-Box")
station
mstscomp <- decompose(msts)
plot(mstscomp$seasonal)
plot(mstscomp$trend)
msfit <- auto.arima(msts)
summary(msfit)
tsdiag(msfit)
msfc <- forecast(msfit,h=12)
msfcted <- msfc$mean
msfcted
plot(msfc)
lines(mstsmain,col="red")

msfit2 <- ma(msts,order=1)
msfit2
msfc2 <- forecast(msfit2,h=12)
msfc2
plot(msfc2)
sink()

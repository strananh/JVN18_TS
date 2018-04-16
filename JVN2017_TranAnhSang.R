library("readxl")
library("xlsx")
library("TTR")
library("forecast")
library("tseries")
library("rugarch")

#READ DATA
#setwd("<directory contains *.csv file>")
data <- read.csv( file = "monthly-lake-erie-levels-1921-19.csv",
                  header = FALSE,
                  skip = 1,
                  col.names = c("time", "lakeLevel"),
                  colClasses = c("character", "numeric"))
data <- na.omit(data)
dataLength <- length(data$time)
dataLength

# NUMBER DAYS OF MONTH
numberOfDays <- function(date) {
  m <- format(date, format = "%m")
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }
  return(as.integer(format(date - 1, format = "%d")))
}

# CHANGE TYPE OF DATA$TIME TO DATE & ADD NUMBER DAYS OF MONTH
for (i in 1:dataLength) {
  temp = data[["time"]][i]
  data[["time"]][i] <- paste(temp, "-01", sep = "")
}
temp <- as.Date(data$time, format = "%Y-%m-%d")
data$time <- temp

# GET SUBSET OF PERIOD [JAN 1936 - DEC 1956]
data <- subset(data, as.numeric(format(data$time, "%Y")) > 1935)
data <- subset(data, as.numeric(format(data$time, "%Y")) < 1957)

# PLOT TIME SERIES
lakeLevel <- ts(data = data$lakeLevel, frequency = 12, start = c(1936, 1), end = c(1956, 12))
dataLength <- length(lakeLevel)

plot.ts(lakeLevel, lty = 2,
        xlab = "year",ylab = "lake level",main = "Monthly Lake Erie Levels 1936 - 1956")
points(lakeLevel, pch = "*")
grid()

# SHOW ACF, PACF
par(mfrow = c(1,2))
acfVar <- acf(lakeLevel, plot = FALSE, lag.max = 24)
acfVar$lag <- acfVar$lag * 12
plot(acfVar, main = "ACF of Monthly Lake Erie Levels")

pacfVar <- pacf(lakeLevel, plot = FALSE, lag.max = 24)
pacfVar$lag <- pacfVar$lag * 12
plot(pacfVar, main = "PACF of Monthly Lake Erie Levels")
par(mfrow = c(1,1))

# SHOW DECOMPOSED COMPONENTS 
comps <- decompose(lakeLevel)
plot(comps)

# SEASONAL PLOT
s = 1
e = s + 11
plot(c(1:12), subset(lakeLevel, start = s, end = e),
     type = "l",
     xlab = "month", ylab = "lake level", main = "Monthly Lake Erie Levels",
     ylim = c(10,20))
i = 3
while(e < dataLength) {
  s = e + 1
  e = s + 11
  subset <- subset(lakeLevel, start = s, end = e, lty = 2)
  lines(c(1:12), subset, lty = 1)
}

# DEFINE FUNCTION TO PLOT HISTOGRAM OF RESIDUALS
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# SPLIT DATA INTO 2 SETS: TRAINING SET, TEST SET
train <- subset(lakeLevel, end = floor(dataLength * 0.8))
trainLength <- length(train)
endOfTrain <- subset(train, start = floor(trainLength * 0.9))
test  <- subset(lakeLevel, start = floor(dataLength * 0.8) + 1)
plot(test, type = "l")
testLength <- length(test)

# TRY FITTING WITH AR(1)
ar1Result <- arima(train, order = c(1,0,0))
ar1Result
# PLOT TRAINING SET + FITTED MODEL
plot(train, lty = 2, type = "l",
     xlab = "month", ylab = "lake level", main = "Monthly Lake Erie Levels")
points(train, pch = "*")
lines(as.numeric(time(train)), train - ar1Result$residuals, lty = 1)
leg <- legend("topleft",
              c("Fitted", "Observed"), lty = c(1, 2, 2), cex = 0.8, plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.7)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Fitted", "Observed"), lty = c(1, 2),  cex = 0.8, y.intersp = 0.6)
grid()

# EVALUATE FITTED MODEL WITH TEST SET
ar1Test <- forecast(ar1Result, h = testLength, level = 95)

# PLOT TEST SET + FITTED MODEL
plot(test, lty = 2, type = "l",
     xlab = "month",ylab = "lake level",main = "Monthly Lake Erie Levels",
     xlim = c(1951, 1957), ylim = c(10, 25), col="blue")
lines(endOfTrain, lty = 2)
points(test, pch = "*", col = "blue")
points(endOfTrain, pch="*")
lines(as.numeric(time(test)), ar1Test$mean,  lty = 1, col = "red")
lines(as.numeric(time(test)), ar1Test$upper, lty = 2, col = "red")
lines(as.numeric(time(test)), ar1Test$lower, lty = 2, col = "red")
leg <- legend("topleft",
              c("Predited", "Test set", "95% confidence interval"), 
              lty = c(1, 2, 2), cex = 0.8, col = c("red", "blue", "red"),
              plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.5)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Predited", "Test set", "95% confidence interval"), 
       lty = c(1, 2, 2),  cex = 0.8, col = c("red", "blue", "red"), y.intersp = 0.6)
grid()

# CALCULATE SSE, RMSE, MAPE, AIC
sse <- sum((test - ar1Test$mean)*(test - ar1Test$mean))
sse
# rmse
rmse <- sqrt(mean(sse))
rmse
# mape
mape <- mean(abs((test - ar1Test$mean)/test)) * 100
mape
# aic
aic <- AIC(ar1Result)
aic

# PLOT HISTOGRAM OF RESIDUALS
plotForecastErrors(na.omit(ar1Result$resid))

# TRY FITTING WITH AR(12)
ar12Result <- arima(train, order = c(12,0,0))
ar12Result
# PLOT TRAINING SET + FITTED MODEL
plot(train, lty = 2, type = "l",
     xlab = "month", ylab = "lake level", main = "Monthly Lake Erie Levels")
points(train, pch = "*")
lines(as.numeric(time(train)), train - ar12Result$residuals, lty = 1)
leg <- legend("topleft",
              c("Fitted", "Observed"), lty = c(1, 2, 2), cex = 0.8, plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.7)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Fitted", "Observed"), lty = c(1, 2),  cex = 0.8, y.intersp = 0.6)
grid()

# EVALUATE FITTED MODEL WITH TEST SET
ar12Test <- forecast(ar12Result, h = testLength, level = 95)

# PLOT TEST SET + FITTED MODEL
plot(test, lty = 2, type = "l",
     xlab = "month",ylab = "lake level",main = "Monthly Lake Erie Levels",
     xlim = c(1951, 1957), ylim = c(10, 25), col="blue")
lines(endOfTrain, lty = 2)
points(test, pch = "*", col = "blue")
points(endOfTrain, pch="*")
lines(as.numeric(time(test)), ar12Test$mean,  lty = 1, col = "red")
lines(as.numeric(time(test)), ar12Test$upper, lty = 2, col = "red")
lines(as.numeric(time(test)), ar12Test$lower, lty = 2, col = "red")
leg <- legend("topleft",
              c("Predited", "Test set", "95% confidence interval"), 
              lty = c(1, 2, 2), cex = 0.8, col = c("red", "blue", "red"),
              plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.5)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Predited", "Test set", "95% confidence interval"), 
       lty = c(1, 2, 2),  cex = 0.8, col = c("red", "blue", "red"), y.intersp = 0.6)
grid()

# CALCULATE SSE, RMSE, MAPE, AIC
sse <- sum((test - ar12Test$mean)*(test - ar12Test$mean))
sse
# rmse
rmse <- sqrt(mean(sse))
rmse
# mape
mape <- mean(abs((test - ar12Test$mean)/test)) * 100
mape
# aic
aic <- AIC(ar12Result)
aic

# PLOT HISTOGRAM OF RESIDUALS
plotForecastErrors(na.omit(ar12Result$resid))

# TRY FITTING WITH EXPONENTIAL SMOOTHING
expResult <- ets(train, model = "ANN")
expResult

# PLOT TRAINING SET + FITTED MODEL
plot(train, lty = 2, type = "l",
     xlab = "month", ylab = "lake level", main = "Monthly Lake Erie Levels")
points(train, pch = "*")
lines(as.numeric(time(train)), train - expResult$residuals, lty = 1)
leg <- legend("topleft",
              c("Fitted", "Observed"), lty = c(1, 2, 2), cex = 0.8, plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.7)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Fitted", "Observed"), lty = c(1, 2),  cex = 0.8, y.intersp = 0.6)
grid()

# EVALUATE FITTED MODEL WITH TEST SET
expTest <- forecast(expResult, h = testLength, level = 95)
expTest

# PLOT TEST SET + FITTED MODEL
plot(test, lty = 2, type = "l",
     xlab = "month",ylab = "lake level",main = "Monthly Lake Erie Levels",
     xlim = c(1951, 1957), ylim = c(10, 30), col="blue")
lines(endOfTrain, lty = 2)
points(test, pch = "*", col = "blue")
points(endOfTrain, pch="*")
lines(as.numeric(time(test)), expTest$mean,  lty = 1, col = "red")
lines(as.numeric(time(test)), expTest$upper, lty = 2, col = "red")
lines(as.numeric(time(test)), expTest$lower, lty = 2, col = "red")
leg <- legend("topleft",
              c("Predited", "Test set", "95% confidence interval"), 
              lty = c(1, 2, 2), cex = 0.8, col = c("red", "blue", "red"),
              plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.5)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Predited", "Test set", "95% confidence interval"), 
       lty = c(1, 2, 2),  cex = 0.8, col = c("red", "blue", "red"), y.intersp = 0.6)
grid()

# sse
sse <- sum((test - expTest$mean)*(test - expTest$mean))
sse
# rmse
rmse <- sqrt(mean(sse))
rmse
# mape
mape <- mean(abs((test - expTest$mean)/test)) * 100
mape
# aic
aic <- AIC(expResult)
aic

# TRY FITTING WITH HOLT-WINTERS
hwResult <- ets(train, model = "AAA")
hwResult

# PLOT TRAINING SET + FITTED MODEL
plot(train, lty = 2, type = "l",
     xlab = "month", ylab = "lake level", main = "Monthly Lake Erie Levels")
points(train, pch = "*")
lines(as.numeric(time(train)), train - hwResult$residuals, lty = 1)
leg <- legend("topleft",
              c("Fitted", "Observed"), lty = c(1, 2, 2), cex = 0.8, plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.7)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Fitted", "Observed"), lty = c(1, 2),  cex = 0.8, y.intersp = 0.6)
grid()

# EVALUATE FITTED MODEL WITH TEST SET
hwTest <- forecast(hwResult, h = testLength, level = 95)
hwTest

# PLOT TEST SET + FITTED MODEL
plot(test, lty = 2, type = "l",
     xlab = "month",ylab = "lake level",main = "Monthly Lake Erie Levels",
     xlim = c(1951, 1957), ylim = c(10, 30), col="blue")
lines(endOfTrain, lty = 2)
points(test, pch = "*", col = "blue")
points(endOfTrain, pch="*")
lines(as.numeric(time(test)), hwTest$mean,  lty = 1, col = "red")
lines(as.numeric(time(test)), hwTest$upper, lty = 2, col = "red")
lines(as.numeric(time(test)), hwTest$lower, lty = 2, col = "red")
leg <- legend("topleft",
              c("Predited", "Test set", "95% confidence interval"), 
              lty = c(1, 2, 2), cex = 0.8, col = c("red", "blue", "red"),
              plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.5)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Predited", "Test set", "95% confidence interval"), 
       lty = c(1, 2, 2),  cex = 0.8, col = c("red", "blue", "red"), y.intersp = 0.6)
grid()

# sse
sse <- sum((test - hwTest$mean)*(test - hwTest$mean))
sse
# rmse
rmse <- sqrt(mean(sse))
rmse
# mape
mape <- mean(abs((test - hwTest$mean)/test)) * 100
mape
# aic
aic <- AIC(hwResult)
aic

# GET DIFFERENCE ORDER 1 FROM TRAINING SET
trainDiff1 <- diff(train, differences = 1, lag = 1)
plot(trainDiff1, main = "Differenced of Monthly Lake Erie Levels")

# TEST FOR STATIONARY
adf.test(trainDiff1, alternative = c("stationary"))

# PLOT ACF & PACF
par(mfrow = c(1,2))
acfResult <-  acf(trainDiff1, lag.max = 12, plot = FALSE)
acfResult$lag <- acfResult$lag * 12
plot(acfResult, main = "ACF of 1st order difference")

pacfResult <- pacf(trainDiff1, lag.max = 12, plot = FALSE)
pacfResult$lag <- pacfResult$lag * 12
plot(pacfResult, main = "PACF of 1st order difference")
par(mfrow = c(1,1))

armResult <- arima(train, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 3)))
summary(armResult)
plot(armResult$residuals)
plotForecastErrors(armResult$residuals)

# TRY FITTING WITH ARIMA
armResult <- auto.arima(train)
armResult
plot(armResult$residuals)
plotForecastErrors(armResult$residuals)

armResult <- arima(train, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 3)))

# PLOT TRAINING SET + FITTED MODEL
plot(train, lty = 2, type = "l",
     xlab = "month", ylab = "lake level", main = "Monthly Lake Erie Levels")
points(train, pch = "*")
lines(as.numeric(time(train)), fitted(armResult), lty = 1)
leg <- legend("topleft",
              c("Fitted", "Observed"), lty = c(1, 2, 2), cex = 0.8, plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.7)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Fitted", "Observed"), lty = c(1, 2),  cex = 0.8, y.intersp = 0.6)
grid()

# EVALUATE FITTED MODEL WITH TEST SET
armTest <- forecast(armResult, h = testLength, level = 95)
armTest

# PLOT TEST SET + FITTED MODEL
plot(test, lty = 2, type = "l",
     xlab = "month",ylab = "lake level",main = "Monthly Lake Erie Levels",
     xlim = c(1951, 1957), ylim = c(10, 30), col="blue")
lines(endOfTrain, lty = 2)
points(test, pch = "*", col = "blue")
points(endOfTrain, pch="*")
lines(as.numeric(time(test)), armTest$mean,  lty = 1, col = "red")
lines(as.numeric(time(test)), armTest$upper, lty = 2, col = "red")
lines(as.numeric(time(test)), armTest$lower, lty = 2, col = "red")
leg <- legend("topleft",
              c("Predited", "Test set", "95% confidence interval"), 
              lty = c(1, 2, 2), cex = 0.8, col = c("red", "blue", "red"),
              plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.5)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Predited", "Test set", "95% confidence interval"), 
       lty = c(1, 2, 2),  cex = 0.8, col = c("red", "blue", "red"), y.intersp = 0.6)
grid()

# sse
sse <- sum((test - armTest$mean)*(test - armTest$mean))
sse
# rmse
rmse <- sqrt(mean(sse))
rmse
# mape
mape <- mean(abs((test - armTest$mean)/test)) * 100
mape
# aic
aic <- AIC(armResult)
aic

# GARCH
# PLOT ACF & PACF
par(mfrow = c(1,2))
acfResult <-  acf(trainDiff1, lag.max = 12, plot = FALSE)
acfResult$lag <- acfResult$lag * 12
plot(acfResult, main = "ACF of 1st order difference")

acfResult <- acf(trainDiff1 * trainDiff1, lag.max = 12, plot = FALSE)
acfResult$lag <- acfResult$lag * 12
plot(acfResult, main = "PACF of 1st order difference")
par(mfrow = c(1,1))

# TRY FITTING WITH GARCH
gResult <- ugarchfit(spec = ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1, 1)),
                                       mean.model = list(armaOrder = c(1, 1), include.mean = TRUE)), 
                     data = trainDiff1)
gResult

# PLOT TRAINING SET + FITTED MODEL
plot(trainDiff1, lty = 2, type = "l")
points(trainDiff1, pch = "*")
lines(as.numeric(time(trainDiff1)), gResult@fit$fitted.values, lty = 1)
leg <- legend("topleft",
              c("Fitted", "Observed"), lty = c(1, 2, 2), cex = 0.8, plot = FALSE)
leftx <- leg$rect$left
rightx <- (leg$rect$left + leg$rect$w * 0.7)
topy <- leg$rect$top
bottomy <- (leg$rect$top - leg$rect$h * 0.7)
legend(x = c(leftx, rightx), y = c(topy, bottomy),
       c("Fitted", "Observed"), lty = c(1, 2),  cex = 0.8, y.intersp = 0.6)
grid()

aic <- (-2*likelihood(fit)) + 2*(length(fit@fit$coef))
aic

gTest <- ugarchforecast(fitORspec = fit, n.ahead = testLength - 1)
plot(gTest)

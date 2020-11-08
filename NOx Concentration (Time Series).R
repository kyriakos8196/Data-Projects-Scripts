rm(list=ls())
setwd("C:/Users/Kyriakos/Desktop")
aq = read.csv("AirQualityCourseworkNew.csv")



library("zoo")
library("timeSeries")
library("xts")
library("tseries")
library("forecast")
library("ggplot2")
library("corrplot")
library("ggfortify")

#Replacing missing values with mean
aq[aq == "-200"] = NA
for(i in 1:ncol(aq)){
  aq[is.na(aq[,i]), i] = round(mean(aq[,i], na.rm = TRUE))
}

#Setting up time series
aq$Date=as.Date(aq$Date, format = "%d/%m/%Y")
aq_order= aq$Date
aq1 = aq[-c(1)]
ts_aq = as.xts(aq1, order.by = aq_order)

#Splitting data
test = window(ts_aq,start="2005-01-01",end="2005-01-31")
train = window(ts_aq,start="2004-03-10",end="2004-12-31")

#Exploratory Analysis
str(train)
head(train)
hist(train$NOx,main="NOx Distribution",xlab="NOx",col="red")
hist(train$Temp,main="Temperature Ditribution",xlab="Temp",col="red")
plot(train$NOx,col="blue")
boxplot(as.vector(train$NO2))
boxplot(as.vector(train$NOx))
boxplot(as.vector(train$Temp))
boxplot(as.vector(train$RH))
boxplot(as.vector(train$AH))

#Time series plot
autoplot(as.ts(train)) + ggtitle("Measures of Air Quality") +
theme(plot.title = element_text(hjust = 0.5))

#Correlation Matrix
M = cor(train)
corrplot(M)
corrplot.mixed(M)

acf(train$NOx,na.action = na.pass)
attr(train, 'frequency') <- 7 #setting frequency to 7
plot(decompose(as.ts(train$NOx)))

#Separating NOx
ts_nox = train[, setdiff(colnames(train),c("NO2","Temp","RH","AH"))]

#Checking for stationarity
adf.test(ts_nox)
kpss.test(ts_nox)
tsdisplay(na.omit(diff(ts_nox,7)))
BoxCox.lambda(ts_nox)
tsdisplay(na.omit(diff(BoxCox(ts_nox,lambda =0.1154567 ),7)))
adf.test(na.omit(diff(BoxCox(ts_nox,lambda =0.1154567 ),7)))
kpss.test(na.omit(diff(BoxCox(ts_nox,lambda =0.1154567 ),7)))
tsdisplay(na.omit(diff(BoxCox(I(ts_nox^2),lambda =0.1154567),7)))

#Applying BoxCox
tsnoxbox = BoxCox(ts_nox,lambda = 0.1154567)
acf(na.omit(diff(I(tsnoxbox^2),7)))
pacf(na.omit(diff(I(tsnoxbox^2))))


#Modelling


#ARIMA and sARIMA
#Model 1
fit1 = Arima(tsnoxbox,order=c(2,0,2),seasonal = c(2,1,2))
checkresiduals(fit1$residuals)
pred = forecast(fit1,h=31)
preddf = data.frame(preds = as.numeric(coredata(pred$mean)))
actuals = data.frame(coredata(test$NOx))
actuals_preds = data.frame(cbind(actuals=actuals$NOx, predicteds=preddf$preds))
actuals_preds$predicteds = InvBoxCox(actuals_preds$predicteds,lambda = 0.1154567)
rmse = sqrt(sum((actuals_preds$predicteds - actuals_preds$actuals)^2)/length(actuals_preds$actuals))
autoplot(pred)+ggtitle("sARIMA(2,0,2)(2,1,2) Forecast")

#Model 2
fit2 = Arima(tsnoxbox,order=c(2,1,2),seasonal=c(1,0,2),xreg=train$NO2+train$RH+train$Temp)
checkresiduals(fit2$residuals)
pred = forecast(fit2,h=31,xreg=(test$NO2+test$RH+test$Temp))
preddf = data.frame(preds = as.numeric(coredata(pred$mean)))
actuals = data.frame(coredata(test$NOx))
actuals_preds = data.frame(cbind(actuals=actuals$NOx, predicteds=preddf$preds))
actuals_preds$predicteds = InvBoxCox(actuals_preds$predicteds,lambda = 0.1154567)
rmse = sqrt(sum((actuals_preds$predicteds - actuals_preds$actuals)^2)/length(actuals_preds$actuals))
autoplot(pred)+ggtitle("sARIMA(2,1,2)(1,0,2) Forecast")

#Model 3
fit3 = Arima(I(tsnoxbox^2),order=c(3,1,2),seasonal=c(1,0,1),xreg = train$NO2+train$RH+train$Temp+train$AH+(train$RH*train$AH))
checkresiduals(fit3$residuals)
pred = forecast(fit3,h=31,xreg=(test$NO2+test$RH+test$Temp+test$AH))
preddf = data.frame(preds = as.numeric(coredata(pred$mean)))
actuals = data.frame(coredata(test$NOx))
actuals_preds = data.frame(cbind(actuals=actuals$NOx, predicteds=preddf$preds))
actuals_preds$predicteds = InvBoxCox(sqrt(actuals_preds$predicteds),lambda = 0.1154567)
rmse = sqrt(sum((actuals_preds$predicteds - actuals_preds$actuals)^2)/length(actuals_preds$actuals))
autoplot(pred)+ggtitle("sARIMA(3,1,2)(1,0,1) Forecast")

#Model 4
fit4 = Arima(I(tsnoxbox^2),order=c(3,0,2),seasonal=c(2,1,2),xreg = train$NO2+train$RH+train$Temp+train$AH+(train$RH*train$AH))
checkresiduals(fit4$residuals)
pred = forecast(fit4,h=31,xreg=(test$NO2+test$RH+test$Temp+test$AH))
preddf = data.frame(preds = as.numeric(coredata(pred$mean)))
actuals = data.frame(coredata(test$NOx))
actuals_preds = data.frame(cbind(actuals=actuals$NOx, predicteds=preddf$preds))
actuals_preds$predicteds = InvBoxCox(sqrt(actuals_preds$predicteds),lambda = 0.1154567)
rmse = sqrt(sum((actuals_preds$predicteds - actuals_preds$actuals)^2)/length(actuals_preds$actuals))
autoplot(pred)+ggtitle("sARIMA(3,0,2)(2,1,2) Forecast")

#Model 5
fit5 = Arima(I(tsnoxbox^2),order=c(2,0,2),seasonal=c(2,1,2),xreg = train$NO2+train$RH+train$Temp+train$AH+(train$Temp*train$AH))#best
checkresiduals(fit5$residuals)
pred = forecast(fit5,h=31,xreg=(test$NO2+test$RH+test$Temp+test$AH))
preddf = data.frame(preds = as.numeric(coredata(pred$mean)))
actuals = data.frame(coredata(test$NOx))
actuals_preds = data.frame(cbind(actuals=actuals$NOx, predicteds=preddf$preds))
actuals_preds$predicteds = InvBoxCox(sqrt(actuals_preds$predicteds),lambda = 0.1154567)
#log actuals_preds$predicteds = exp(actuals_preds$predicteds)
rmse = sqrt(sum((actuals_preds$predicteds - actuals_preds$actuals)^2)/length(actuals_preds$actuals))
autoplot(pred)+ggtitle("sARIMA(2,0,2)(2,1,2) Forecast")
Box.test(fit5$residuals,type = "Ljung-Box")
qqnorm(fit5$residuals)
qqline(fit5$residuals)

#forecast vs actual
predsts = xts(actuals_preds,order.by=index(test))
autoplot(as.ts(predsts)) +ggtitle("Forecasts vs Actuals") +
  theme(plot.title = element_text(hjust = 0.5))

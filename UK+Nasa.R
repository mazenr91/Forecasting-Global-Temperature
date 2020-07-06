library(readxl)
library(forecast)

#Importing the UK Met Data file

file<-read.csv("C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Group Assignment 2\\UK Met Office_Data.csv")
file$Median.of.the.100.ensemble.member.time.series<-(file$Median.of.the.100.ensemble.member.time.series+14)
file_ts<-ts(file$Median.of.the.100.ensemble.member.time.series,start = 1850,frequency = 12)

#Checking the Class of UK Met Data File
class(file_ts)

head(file)
str(file)
tail(file)

#Decompose the data to understand the Error, Seasonality and trend 
fit1<-decompose(file_ts,type="multiplicative")
plot(fit1)

fit2<-decompose(file_ts,type="additive")
plot(fit2)

fit3<-stl(file_ts,t.window = 12,s.window = "periodic")
plot(fit3)


#---- ETS Model(Exponential Smoothning Model)
fit_AAN<-ets(file_ts,model="AAN",damped = FALSE)
fit_MMN<-ets(file_ts,model="MMN",damped = FALSE)
fit_ZZZ<-ets(file_ts,model="ZZZ",damped=  FALSE)
fit_MAN<-ets(file_ts,model="MAN",damped = FALSE)
fit_AAA<-ets(file_ts,model="AAA",damped = FALSE)


#---- Prediction for 969 months using ETS Model
fit_AAN_pred<-forecast(fit_AAN,h=969,level=c(0.8,0.95))
fit_MMN_pred<-forecast(fit_MMN,h=969,level=c(0.8,0.95))
fit_ZZZ_pred<-forecast(fit_ZZZ,h=969,level=c(0.8,0.95))
fit_MAN_pred<-forecast(fit_MAN,h=969,level=c(0.8,0.95))
fit_AAA_pred<-forecast(fit_AAA,h=969,level=c(0.8,0.95))

#----- Plotting the graphs
par(mfrow=c(1,2))
plot(fit_AAN_pred,xlab="year",ylab="Temp")
plot(fit_MMN_pred,xlab="year",ylab="Temp")
plot(fit_ZZZ_pred,xlab="year",ylab="Temp")
plot(fit_MAN_pred,xlab="year",ylab="Temp")
plot(fit_AAA_pred,xlab="year",ylab="Temp")



###---tbats


fit_tbats<-tbats(file_ts)
fit_tbats

fit_tbats_pred<-forecast(fit_tbats,h=969)

par(mfrow=c(1,1))
plot(fit_tbats_pred,xlab="year",ylab="Predicted Temp")


#----Cross validation of ETS Model to find the best model, Train & Test
f_AAN<-function(y,h)forecast((ets(y,model = "AAN")),h=h)
errors_AAN<-tsCV(file_ts,f_AAN,h=25,window = 1000)

f_MMN<-function(y,h)forecast((ets(y,model = "MMN")),h=h)
errors_MMN<-tsCV(file_ts,f_MMN,h=25,window = 1000)

f_ANA<-function(y,h)forecast((ets(y,model = "ANA")),h=h)
errors_ANA<-tsCV(file_ts,f_ANA,h=25,window = 1000)

f_MAN<-function(y,h)forecast((ets(y,model = "MAN")),h=h)
errors_MAN<-tsCV(file_ts,f_MAN,h=25,window = 1000)

f_AAA<-function(y,h)forecast((ets(y,model = "AAA")),h=h)
errors_AAA<-tsCV(file_ts,f_AAA,h=25,window = 1000)


mean(abs(errors_AAN/file_ts),na.rm=TRUE)*100
mean(abs(errors_MMN/file_ts),na.rm =TRUE)*100
mean(abs(errors_ANA/file_ts),na.rm=TRUE)*100
mean(abs(errors_MAN/file_ts),na.rm=TRUE)*100
mean(abs(errors_AAA/file_ts),na.rm=TRUE)*100



#----Cross validation of tbats Model to find the best model, Train & Test

f_tbats<-function(y,h)forecast(tbats(y),h=h)
errors_tbats<-tsCV(file_ts,f_tbats,h=25,window = 1000)

mean(abs(errors_tbats/file_ts),na.rm=TRUE)*100

#-------Arima


par(mfrow=c(1,2))

plot(file_ts,xlab="Year",ylab="Temp")
plot(log(file_ts),xlab="Year",ylab="Temp")

plot(diff(log(file_ts)),xlab="Year",ylab="Annual Change in Temp")


file_ts_decompose<-decompose(file_ts,type = "additive")
file_ts_decompose<-decompose(file_ts,type = "multiplicative")

plot(file_ts_decompose)

file_ts_decompose<-stl(file_ts,s.window = "periodic",t.window=12)

par(mfrow=c(1,3))
Acf(file_ts,main="")
Acf(log(file_ts),main="")
Acf(diff(log(file_ts),12),main="")

par(mfrow=c(1,2))
pacf(log(file_ts),main="")
pacf(diff(log(file_ts),12),main="")


#Arima Prediction with out log
fit1<-auto.arima(file_ts,seasonal = TRUE)
plot(fit1)
par(mfrow=c(1,1))
plot(residuals(fit1))
Acf(residuals(fit1))
fit1

prediction1<-forecast(fit1,969)

par(mfrow=c(1,2))
plot(prediction1)
prediction1

prediction1$mean<-prediction1$mean-14
prediction1$x<-prediction1$x-14
prediction1$lower<-prediction1$lower-14
prediction1$upper<-prediction1$upper-14

#Arima with out log cross validation
f_arima<-function(y,h)forecast(auto.arima(y),h=h)
errors_arima<-tsCV(file_ts,f_arima,h=25,window = 1000)

mean(abs(errors_arima/file_ts),na.rm=TRUE)*100


write.csv(prediction1, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Group Assignment 2\\UK_Met_Arima1.csv") 



#Arima Prediction With log
fit2<-auto.arima(log(file_ts),seasonal = TRUE)
plot(fit2)

plot(residuals(fit2))
Acf(residuals(fit2))
fit2

prediction2<-forecast(fit2,969)

prediction2$mean<-exp(prediction2$mean)
prediction2$x<-exp(prediction2$x)
prediction2$lower<-exp(prediction2$lower)
prediction2$upper<-exp(prediction2$upper)

prediction2$mean<-prediction2$mean-14
prediction2$x<-prediction2$x-14
prediction2$lower<-prediction2$lower-14
prediction2$upper<-prediction2$upper-14

#Arima with log cross Validation
f_arima<-function(y,h)forecast(auto.arima(log(y)),h=h)
errors_arima<-tsCV(file_ts,f_arima,h=25,window = 1000)

mean(abs(errors_arima/log(file_ts)),na.rm=TRUE)*100


plot(prediction2)

write.csv(prediction2, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Group Assignment 2\\UK_Met_Arima_with_log_1.csv") 


##------Importing Nasa File

file_nasa<-read.csv("C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Group Assignment 2\\nasadata.csv")
file_nasa$Temp.C<-as.character(file_nasa$Temp.C)
file_nasa$Temp.C<-as.numeric(file_nasa$Temp.C)
file_nasa$Temp.C <- (file_nasa$Temp.C+14)
file_nasa_ts<-ts(file_nasa$Temp.C,start = 1880,frequency = 12)


#Checking the Class of Nasa Data File
class(file_nasa_ts)

head(file_nasa)
str(file_nasa)
tail(file_nasa)

#Decompose the data to understand the Error, Seasonality and trend 
fit3<-decompose(file_nasa_ts,type="multiplicative")
plot(fit3)

fit4<-decompose(file_nasa_ts,type="additive")
plot(fit4)

fit5<-stl(file_nasa_ts,t.window = 12,s.window = "periodic")
plot(fit5)


#---- ETS Model(Exponential Smoothning Model)
fit_nasa_AAN<-ets(file_nasa_ts,model="AAN",damped = FALSE)
fit_nasa_MMN<-ets(file_nasa_ts,model="MMN",damped = FALSE)
fit_nasa_ZZZ<-ets(file_nasa_ts,model="ZZZ",damped=  FALSE)
fit_nasa_MAN<-ets(file_nasa_ts,model="MAN",damped = FALSE)
fit_nasa_AAA<-ets(file_nasa_ts,model="AAA",damped = FALSE)


#---- Prediction for 960 months using ETS Model
fit_nasa_AAN_pred<-forecast(fit_nasa_AAN,h=969,level=c(0.8,0.95))
fit_nasa_MMN_pred<-forecast(fit_nasa_MMN,h=969,level=c(0.8,0.95))
fit_nasa_ZZZ_pred<-forecast(fit_nasa_ZZZ,h=969,level=c(0.8,0.95))
fit_nasa_MAN_pred<-forecast(fit_nasa_MAN,h=969,level=c(0.8,0.95))
fit_nasa_AAA_pred<-forecast(fit_nasa_AAA,h=969,level=c(0.8,0.95))

#----- Plotting the graphs
par(mfrow=c(1,2))
plot(fit_nasa_AAN_pred,xlab="year",ylab="Temp")
plot(fit_nasa_MMN_pred,xlab="year",ylab="Temp")
plot(fit_nasa_ZZZ_pred,xlab="year",ylab="Temp")
plot(fit_nasa_MAN_pred,xlab="year",ylab="Temp")
plot(fit_nasa_AAA_pred,xlab="year",ylab="Temp")



###---tbats


fit_nasa_tbats<-tbats(file_nasa_ts)
fit_nasa_tbats

fit_nasa_tbats_pred<-forecast(fit_nasa_tbats,h=969)

par(mfrow=c(1,1))
plot(fit_nasa_tbats_pred,xlab="year",ylab="Predicted Temp")


#----Cross validation of ETS Model to find the best model, Train & Test
f_nasa_AAN<-function(y,h)forecast((ets(y,model = "AAN")),h=h)
errors_nasa_AAN<-tsCV(file_nasa_ts,f_nasa_AAN,h=25,window = 1000)

f_nasa_MMN<-function(y,h)forecast((ets(y,model = "MMN")),h=h)
errors_nasa_MMN<-tsCV(file_nasa_ts,f_nasa_MMN,h=25,window = 1000)

f_nasa_ANA<-function(y,h)forecast((ets(y,model = "ANA")),h=h)
errors_nasa_ANA<-tsCV(file_nasa_ts,f_nasa_ANA,h=25,window = 1000)

f_nasa_MAN<-function(y,h)forecast((ets(y,model = "MAN")),h=h)
errors_nasa_MAN<-tsCV(file_nasa_ts,f_nasa_MAN,h=25,window = 1000)

f_nasa_AAA<-function(y,h)forecast((ets(y,model = "AAA")),h=h)
errors_nasa_AAA<-tsCV(file_nasa_ts,f_nasa_AAA,h=25,window = 1000)


mean(abs(errors_nasa_AAN/file_nasa_ts),na.rm=TRUE)*100
mean(abs(errors_nasa_MMN/file_nasa_ts),na.rm =TRUE)*100
mean(abs(errors_nasa_ANA/file_nasa_ts),na.rm=TRUE)*100
mean(abs(errors_nasa_MAN/file_nasa_ts),na.rm=TRUE)*100
mean(abs(errors_nasa_AAA/file_nasa_ts),na.rm=TRUE)*100



#----Cross validation of tbats Model to find the best model, Train & Test

f_nasa_tbats<-function(y,h)forecast(tbats(y),h=h)
errors_nasa_tbats<-tsCV(file_nasa_ts,f_nasa_tbats,h=25,window = 1000)

mean(abs(errors_nasa_tbats/file_nasa_ts),na.rm=TRUE)*100

#-------Arima


par(mfrow=c(1,2))

plot(file_nasa_ts,xlab="Year",ylab="Temp")
plot(log(file_nasa_ts),xlab="Year",ylab="Temp")

plot(diff(log(file_nasa_ts)),xlab="Year",ylab="Annual Change in Temp")


file_nasa_ts_decompose<-decompose(file_nasa_ts,type = "additive")
file_nasa_ts_decompose<-decompose(file_nasa_ts,type = "multiplicative")

plot(file_nasa_ts_decompose)

file_nasa_ts_decompose<-stl(file_nasa_ts,s.window = "periodic",t.window=12)

par(mfrow=c(1,3))
Acf(file_nasa_ts,main="")
Acf(log(file_nasa_ts),main="")
Acf(diff(log(file_nasa_ts),12),main="")

par(mfrow=c(1,2))
pacf(log(file_nasa_ts),main="")
pacf(diff(log(file_nasa_ts),12),main="")


#Arima Prediction with out log

fit_nasa_1<-auto.arima(file_nasa_ts,seasonal = TRUE)
plot(fit_nasa_1)
par(mfrow=c(1,1))
plot(residuals(fit_nasa_1))
Acf(residuals(fit_nasa_1))
fit_nasa_1

prediction_nasa_1<-forecast(fit_nasa_1,969)

par(mfrow=c(1,2))
plot(prediction_nasa_1)
prediction_nasa_1

prediction_nasa_1$mean<-prediction_nasa_1$mean-14
prediction_nasa_1$x<-prediction_nasa_1$x-14
prediction_nasa_1$lower<-prediction_nasa_1$lower-14
prediction_nasa_1$upper<-prediction_nasa_1$upper-14


#Arima with out log cross validation
f_nasa_arima<-function(y,h)forecast(auto.arima(y),h=h)
errors_nasa_arima<-tsCV(file_nasa_ts,f_nasa_arima,h=25,window = 1000)

mean(abs(errors_nasa_arima/file_nasa_ts),na.rm=TRUE)*100


write.csv(prediction_nasa_1, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Group Assignment 2\\Nasa_Met_Arima1.csv") 


#Arima Prediction With log
fit_nasa_2<-auto.arima(log(file_nasa_ts),seasonal = TRUE)
plot(fit_nasa_2)

plot(residuals(fit_nasa_2))
Acf(residuals(fit_nasa_2))
fit_nasa_2

prediction_nasa_2<-forecast(fit_nasa_2,969)

prediction_nasa_2$mean<-exp(prediction_nasa_2$mean)
prediction_nasa_2$x<-exp(prediction_nasa_2$x)
prediction_nasa_2$lower<-exp(prediction_nasa_2$lower)
prediction_nasa_2$upper<-exp(prediction_nasa_2$upper)


prediction_nasa_2$mean<-prediction_nasa_2$mean-14
prediction_nasa_2$x<-prediction_nasa_2$x-14
prediction_nasa_2$lower<-prediction_nasa_2$lower-14
prediction_nasa_2$upper<-prediction_nasa_2$upper-14

prediction_nasa_2

#Arima with log cross Validation
f_nasa_arima<-function(y,h)forecast(auto.arima(log(y)),h=h)
errors_nasa_arima<-tsCV(file_nasa_ts,f_nasa_arima,h=25,window = 1000)

mean(abs(errors_nasa_arima/log(file_nasa_ts)),na.rm=TRUE)*100

plot(prediction_nasa_2)

write.csv(prediction_nasa_2, file = "C:\\Users\\anuj\\Documents\\Anuj\\MMA\\Predictive Modeling\\Group Assignment 2\\Nasa_Met_Arima_with_log_1.csv") 

getwd()
setwd('/Users/constantinarhip/Documents/Crystal/Project')
#####General Overview
#In this file, data across different years are treated separately because there is no smooth transition of data
#across years (an instance would be looking at temperature transition). Most likely, data is from different countries

#Abbreviations: Data:General Dataset; Data1Daily: Dataset for daily aggregates in 2005; Data2Weekly:Dataset
#for weekly aggregates in 2006

#Importing the data
library(readxl)
Data <- read.csv('Energy_data.csv')
View(Data)
str(Data)
summary(Data)
Data[duplicated(Data[, 2]), 1]
#data appears to not be highly duplicated

#Replacing column names with shortcuts
colnames(Data) <- c('H','ECons1', 'ECons2', 'FTemp1', 
                    'FHum1', 'FTemp2', 'FHum2')

#Visualizing Hourly Data
library(ggplot2)
ggplot(data=Data, aes(x=H, y=FTemp1))+
  geom_line()

ggplot(data=Data, aes(x=H, y=FTemp2))+
  geom_line()

ggplot(data=Data, aes(x=H, y=FHum1))+
  geom_line()

ggplot(data=Data, aes(x=H, y=FHum2))+
  geom_line()

ggplot(data=Data, aes(x=H, y=ECons1))+
  geom_line()

ggplot(data=Data, aes(x=H, y=ECons2))+
  geom_line()

#Energy consumption has some abnormal observations

#####1.Normalization and filling missing values

#Normalizing extreme values
#abnormal values might be replaced with the mean of same 
#hour value a day before and a day after

which(Data$ECons1>= 1000 | Data$ECons1<=0)

ind01 <- which(Data$ECons1>= 1000 | Data$ECons1<=0)
Data$ECons1[ind01] <- sapply(ind01, function(i)
  with(Data, mean(c(ECons1[i-12], ECons1[i+12]))))

ind02 <- which(Data$ECons2>= 1000 | Data$ECons2<=0)
Data$ECons2[ind01] <- sapply(ind01, function(i)
  with(Data, mean(c(ECons2[i-12], ECons2[i+12]))))


#Dealing with missing values
#missing values might be replaced with the mean of same 
#hour value a day before and a day after

ind1 <- which(is.na(Data$ECons1))
Data$ECons1[ind1] <- sapply(ind1, function(i)
                      with(Data, mean(c(ECons1[i-12], ECons1[i+12]))))

#1NA remaining which will trake the mean between 3 data: 1h before, 1h after and 1 day before
ind10 <- which(is.na(Data$ECons1))
Data$ECons1[ind1] <- sapply(ind1, function(i)
  with(Data, mean(c(ECons1[i-12], ECons1[i+1]), ECons1[i-1])))


ind2 <- which(is.na(Data$ECons2))
Data$ECons2[ind2] <- sapply(ind2, function(i)
                      with(Data, mean(c(ECons2[i-12], ECons2[i+12]))))

ind20 <- which(is.na(Data$ECons2))
Data$ECons2[ind2] <- sapply(ind2, function(i)
  with(Data, mean(c(ECons2[i-12], ECons1[i+2]), ECons2[i-1])))


#####2.Correlation
#Correlation of variables across hours
library(corrplot)

#Correlation year 2005 (hourly)
M1 <- cor(Data[, c(2,4,5)])
corrplot(M1, method = "color",
         addCoef.col = "black",
         tl.col = "black")

#Correlation year 2006 (hourly)
M2 <- cor(Data[1:8760, c(3,6,7)])
corrplot(M2, method = "color",
         addCoef.col = "black",
         tl.col = "black")

#Humidity and Temperature appear to be negatively correlated

#Preparing daily data (2 dataframes are created for specific years)
#2 data columns are created in the main dataset 
#because on 29th of February there is a difference for year 2005 and year 2006

Data$Time<- (seq(as.POSIXct("2000-01-01 00:00:00"), 
                  as.POSIXct("2000-12-31 23:59:00"), by="hour"))
Data$DayMonth <- strftime(Data$Time, "%d%m")
Data$DayMonth <-as.numeric(Data$DayMonth)


Data1Daily <- (seq(as.POSIXct("2000-01-01"), 
                   as.POSIXct("2000-12-31"), by="days"))
Data1Daily <- as.data.frame(Data1Daily)

Data2Daily <- (seq(as.POSIXct("2006-01-01"), 
                   as.POSIXct("2006-12-31"), by="days"))
Data2Daily <- as.data.frame(Data2Daily)

#aggregating the values
#Data for 2005
#Daily mean is taking into consideration (SUM could also be taking into consideration 
#but would not yield different results as the number of observations is constant)
Data1Daily[,2]<- aggregate(Data$ECons1, by = list(Data$DayMonth), FUN=mean)[2]
Data1Daily[,3] <- aggregate(Data$FTemp1, by = list(Data$DayMonth), FUN=mean)[2]
Data1Daily[,4] <- aggregate(Data$FHum1, by = list(Data$DayMonth), FUN=mean)[2]
colnames(Data1Daily) <- c('Time','ECon1', 'FTemp1', 'FHum1')


#data for 2006
Data2Daily[,2]<- aggregate(Data[1:8760, 2], by = list(Data[1:8760, 9]), FUN=sum)[2]
Data2Daily[,3] <- aggregate(Data[1:8760, 6], by = list(Data[1:8760, 9]), FUN=mean)[2]
Data2Daily[,4] <- aggregate(Data[1:8760, 7], by = list(Data[1:8760, 9]), FUN=mean)[2]
colnames(Data2Daily) <- c('Time','ECon2', 'FTemp2', 'FHum2')

#Visualizing daily data
ggplot(data=Data1Daily, aes(x=Time, y=ECon1))+
  geom_line()+
  ggtitle('Energy consumption in 2005')

ggplot(data=Data2Daily, aes(x=Time, y=ECon2))+
  geom_line()+
  ggtitle('Energy consumption in 2006')


#Daily correlation  
#Correlation of year 2005
cor(Data1Daily[,2:4])
MD1 <- cor(Data1Daily[,2:4])
corrplot(MD1, method = "color",
         addCoef.col = "black",
         tl.col = "black")

#Correlation for year 2006
cor(Data2Daily[,2:4])
MD1 <- cor(Data2Daily[,2:4])
corrplot(MD1, method = "color",
         addCoef.col = "black",
         tl.col = "black")

#Preparing weekly data
#Weekly data for 2005

Data1Daily$Week <- strftime(Data1Daily$Time, "%W")

Data1Weekly <- (seq(as.POSIXct("2000-01-01"), 
                   as.POSIXct("2000-12-31"), by="week"))

Data1Weekly <- as.data.frame(Data1Weekly)
head(Data1Weekly)
Data1Weekly$Time <- 1:53


Data1Weekly[,2]<- aggregate(Data1Daily[1:366, 2], by = list(Data1Daily[1:366, 5]), FUN=sum)[2]
Data1Weekly[,3] <- aggregate(Data1Daily[1:366, 3], by = list(Data1Daily[1:366, 5]), FUN=mean)[2]
Data1Weekly[,4] <- aggregate(Data1Daily[1:366, 4], by = list(Data1Daily[1:366, 5]), FUN=mean)[2]
colnames(Data1Weekly) <- c('Time','ECon1', 'FTemp1', 'FHum1')

#Weekly data for 2006

Data2Weekly <- (seq(as.POSIXct("2006-01-01"), 
                    as.POSIXct("2006-12-31"), by="week"))

Data2Weekly <- as.data.frame(Data2Weekly)
Data2Daily$Week <- strftime(Data2Daily$Time, "%W")

Data2Weekly[,2]<- aggregate(Data2Daily[1:365, 2], by = list(Data2Daily[1:365, 5]), FUN=sum)[2]
Data2Weekly[,3] <- aggregate(Data2Daily[1:365, 3], by = list(Data2Daily[1:365, 5]), FUN=mean)[2]
Data2Weekly[,4] <- aggregate(Data2Daily[1:365, 4], by = list(Data2Daily[1:365, 5]), FUN=mean)[2]
colnames(Data1Weekly) <- c('Time','ECon1', 'FTemp1', 'FHum1')

#Weekly correlation  
#Correlation for year 2005

cor(Data1Weekly[,2:4])
MW1 <- cor(Data1Weekly[,2:4])
corrplot(MW1, method = "color",
         addCoef.col = "black",
         tl.col = "black")

#Correlation for year 2006

cor(Data2Weekly[,2:4])
MW2 <- cor(Data1Weekly[,2:4])
corrplot(MW2, method = "color",
         addCoef.col = "black",
         tl.col = "black")

#####3.Forecast
#2 models are going to be used ARIMA and ETS

#Autocorrelation
acf(Data1[,2], 50)
acf(diff(Data1[,2], 50))
plot(Data1[,1:2])

#Checking the stationarity
library(tseries)
adf.test(Data[, 3])
#Data appears to be stationary (energy consumption in 2005 moderate stationarity), 
#except temperature (non stationary)
Data$H <- as.numeric(Data$H)

# Forecast hourly year 2005
library(forecast)
#Energy 2005 daily
train.Data1EH <- Data[1:7027, 2]
test.Data1EH <- Data[7028:8784, 2]

#ets
ets.Data1EH <- ets(ts(train.Data1EH ,start = 1,frequency = 24))
summary(ets.Data1EH)
plot(forecast(ets.Data1EH),1757)
accuracy(forecast(ets.Data1EH, 1757), test.Data1EH)
checkresiduals(ets.Data1EH)
#Test set MAPE 13.64

#sarima
sarima.Data1EH<-auto.arima(train.Data1EH)
summary(sarima.Data1EH)
accuracy(forecast(sarima.Data1EH, 1757), test.Data1EH)
plot(sarima.Data1EH)
#MAPE 38.69


model1 <- ts(Data[1:7027 ,1:2], frequency = 24)
arima_fit1 = auto.arima(model1[,2])
arima_forecast1 = forecast(arima_fit1, h = 240)
plot(arima_forecast1)
accuracy(forecast(arima_fit1, 1757), test.Data1EH)
#MAPE 15.83%

#Energy 2006 daily

train.Data2EH <- Data[1:7008, 3]
test.Data2EH <- Data[7009:8760, 3]

#ets
ets.Data2EH <- ets(ts(train.Data2EH ,start = 1,frequency = 24))
summary(ets.Data2EH)
plot(forecast(ets.Data2EH),1752)
accuracy(forecast(ets.Data2EH, 1752), test.Data2EH)
checkresiduals(ets.Data2EH)
#Test set MAPE 13.05

#sarima
sarima.Data2EH<-auto.arima(train.Data2EH)
summary(sarima.Data2EH)
accuracy(forecast(sarima.Data2EH, 1752), test.Data2EH)
plot(sarima.Data2EH)
#MAPE 25.69



#Forecast daily energy 2005

train.Data1ED <- Data1Daily[1:292, 2]
test.Data1ED <- Data1Daily[293:366, 2]

#ets
ets.Data1ED <- ets(ts(train.Data1ED ,start = 1,frequency = 7))
summary(ets.Data1ED)
plot(forecast(ets.Data1ED),74)
accuracy(forecast(ets.Data1ED, 74), test.Data1ED)
checkresiduals(ets.Data1EH)
#Test set MAPE 9.71

#sarima
sarima.Data1ED<-auto.arima(train.Data1ED)
summary(sarima.Data1ED)
accuracy(forecast(sarima.Data1ED, 74), test.Data1ED)
plot(sarima.Data1EH)
#MAPE 9.02

#Forecast daily energy 2006

train.Data2ED <- Data2Daily[1:292, 2]
test.Data2ED <- Data2Daily[293:365, 2]

#ets
ets.Data2ED <- ets(ts(train.Data2ED ,start = 1,frequency = 7))
summary(ets.Data2ED)
plot(forecast(ets.Data2ED),73)
accuracy(forecast(ets.Data2ED, 73), test.Data2ED)
checkresiduals(ets.Data2EH)
#Test set MAPE 9.32

plot(forecast(ets.Data2ED,73))
lines(test.Data2ED,col="red")
lines(ets.Data2ED$fitted,col="violet") 


#sarima
sarima.Data2ED<-auto.arima(train.Data2ED)
summary(sarima.Data1ED)
accuracy(forecast(sarima.Data2ED, 73), test.Data2ED)
plot(sarima.Data2EH)
#MAPE 8.6
#For daily energy SARIMA is better


#Forecast weekly energy 2005

train.Data1EW <- Data1Weekly[1:42, 2]
test.Data1EW <- Data1Daily[43:53, 2]

#ets
ets.Data1EW <- ets(ts(train.Data1EW ,start = 1,frequency = 4))
summary(ets.Data1EW)
plot(forecast(ets.Data1EW),11)
accuracy(forecast(ets.Data1EW, 11), test.Data1EW)
checkresiduals(ets.Data1EW)
#Very high MAPE


#sarima
sarima.Data1EW<-auto.arima(train.Data1EW)
summary(sarima.Data1EW)
accuracy(forecast(sarima.Data1EW, 11), test.Data1EW)
plot(sarima.Data1EH)
#MAPE 581

#Very high MAPE for year 2005
#I assume more analysis is needed to find the best predictive model for this dataset


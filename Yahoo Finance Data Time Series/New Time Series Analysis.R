#Important Packages
library(dplyr)
library(lubridate)
library(forecast)
library(TTR)
library(ggplot2)
library(tseries)
library(gridExtra)

#Import dataset:
Dataset <- read.csv("covid_19_data.csv")
head(Dataset)
#subset the location only Jakarta, and column i..Date, and New.Cases and change data type
data1 <- Dataset %>% 
  subset(Location == "DKI Jakarta") %>% 
  select(ï..Date, New.Cases) %>% 
  mutate(ï..Date = mdy(ï..Date)) 

#Check null values
colSums(is.na(covid1))

#change the column name from i..Date to Date
colnames(data1)[1] = "Date"



data1 <- data1 %>% 
  subset(Date >= "2020-04-01")

#time series object
Covid19_data_time_series <- ts(data = data1$New.Cases,
               start = min(data1$Date),
               frequency = 7) 

#time series plot
plot(Covid19_data_time_series)
#decomposition
covid_dc <- decompose(Covid19_data_time_series, type = "multiplicative")
covid_dc %>% autoplot()
autoplot(covid_dc$x - covid_dc$seasonal)

#adf test
adf.test(Covid19_data_time_series)

diff(Covid19_data_time_series, lag = 1) %>% adf.test()
#plot
tsdisplay(diff(Covid19_data_time_series))
#modeling
covid_arima1 <- Arima(y = Covid19_data_time_series, order = c(1,1,1))
#Check accuracy
accuracy(covid_arima1)
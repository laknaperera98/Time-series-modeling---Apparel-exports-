
# Data from 2004 to 2019


library(fUnitRoots)
library(forecast)
library(tidyverse)
library(lmtest)
library(magrittr)
library(fpp2)
library(MLmetrics)

Original <- Dataset$Revenue
# This is the original data set.
# period : 2009 Jan - 2021 July
Original_data <- ts(Original, frequency = 12, start = c(2009, 1))
Original_data
#plot(Original_data)

# Define Training set and the Testing set

analysis_data <- window(ts(Original_data), 1, 132)
analysis_data <- ts(analysis_data, frequency = 12, start = c(2009, 1))

training_set <- window(ts(Original_data), 1, 108)
training_set
testing_set <- window(ts(Original_data), 109, 132)
testing_set


training_set <- ts(training_set, frequency = 12, start = c(2009, 1))
training_set
testing_set <- ts(testing_set, frequency = 12, start = c(2018, 1))
testing_set


# Plots 
# Time series plot for whole data set (2009 - 2021) 
# plot 1
autoplot(Original_data) +
  theme_classic() +
  labs(title = "Textile and Apparel exports in Sri Lanka", 
       x = "Year", 
       y = "US$ million",
       subtitle = "Time period : 2009-January - 2021-July",
       caption = "Figure 1") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 

# plot 2
ggseasonplot(Original_data, year.labels=TRUE, year.labels.left=TRUE) +
  theme_classic() +
  labs(title = "Textile and Apparel exports in Sri Lanka", 
       x = "Year", 
       y = "US$ million",
       subtitle = "Time period : 2009-January - 2021-July",
       caption = "Figure 1") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 

# plot 3
ggseasonplot(Original_data, polar = TRUE) +
  theme_classic() +
  labs(title = "Textile and Apparel exports in Sri Lanka", 
       x = "Year", 
       y = "US$ million",
       subtitle = "Time period : 2009-January - 2021-July",
       caption = "Figure 1") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 


# Time series plot for analysis (2009 - 2019)
# plot 4
autoplot(analysis_data, color = "#8da0cb") +
  theme_classic() +
  labs(title = "Textile and Apparel exports in Sri Lanka", 
       x = "Year", 
       y = "Revenue",
       subtitle = "Time period : 2009-January - 2019-December",
       caption = "Figure 2") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 


# plot 5
# Training set
autoplot(training_set, color = "#8da0cb") +
  theme_classic() +
  labs(title = "Textile and Apparel exports in Sri Lanka", 
       x = "Year", 
       y = "Revenue",
       subtitle = "Time period : 2009-January - 2017-December",
       caption = "Figure 3") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 

# plot 6
# Testing set
autoplot(testing_set, color = "#8da0cb") +
  theme_classic() +
  labs(title = "Textile and Apparel exports in Sri Lanka", 
       x = "Year", 
       y = "Revenue",
       subtitle = "Time period : 2018-January - 2019-December",
       caption = "Figure 4") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 



acf(training_set, 100, main = "ACF of original series")
pacf(training_set, 100, main = "PACF of original series")
# The series is not stationary. we can clearly see this in ACF.

# Also variance is increasing.
# First make the variance constant.
logTr = log(training_set)
autoplot(logTr, color = "#8da0cb") +
  theme_classic() +
  labs(title = "Log Transformed Series", 
       x = "Year", 
       y = "log (Revenue)",
       subtitle = "Time period : 2009-January - 2017-December",
       caption = "Figure 5") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 



acf(logTr, 100, main = "ACF of log(Revenue) Series")
pacf(logTr, 100, main = "PACF of log(Revenue) Series")
# By looking at the ACF we can say the series is not stationary.
# And it shows a seasonal pattern.
# Lets also check the ADF to test the stationarity of the series.
adfTest(logTr)
# High p value = 0.7594 , Do not reject H0
# Therefore the series is not stationary.




# Take the lag 12 difference
D12logTr <- diff(logTr, 12)
acf(D12logTr, 100, main = "ACF of D12 log(Revenue) Series")
pacf(D12logTr, 100, main = "PACF of D12 log(Revenue) Series") 


adfTest(D12logTr)


D12D1logTr <- diff(D12logTr, 1)
acf(D12D1logTr, 100, main = "ACF of D12 D1 log(Revenue) Series")
pacf(D12D1logTr, 100, main = "PACF of D12 D1 log(Revenue) Series")


adfTest(D12logTr)



# Fit the models

#ARIMA(1,1,0)(0,1,2) 
fit1 <- Arima(logTr, order = c(1,1,0), seasonal = c(0,1,2))
summary(fit1)
coeftest(fit1)
Box.test(residuals(fit1), lag = 24, type = "Ljung")
# p value = 0.5443, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.


#ARIMA(1,1,0)(3,1,0) 
fit2 <- Arima(logTr, order = c(1,1,0), seasonal = c(3,1,0))
summary(fit2)
coeftest(fit2)
Box.test(residuals(fit2), lag = 24, type = "Ljung")
# p value = 0.06797, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.



#ARIMA(1,1,0)(3,1,2) 
fit3 <- Arima(logTr, order = c(1,1,0), seasonal = c(3,1,2))
summary(fit3)
coeftest(fit3)
Box.test(residuals(fit3), lag = 24, type = "Ljung")
# p value = 0.1452, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.


#ARIMA(0,1,2)(0,1,2) 
fit4 <- Arima(logTr, order = c(0,1,2), seasonal = c(0,1,2))
summary(fit4)
coeftest(fit4)
Box.test(residuals(fit4), lag = 24, type = "Ljung")
# p value = 0.2489, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.


#ARIMA(0,1,2)(3,1,0) 
fit5 <- Arima(logTr, order = c(0,1,2), seasonal = c(3,1,0))
summary(fit5)
coeftest(fit5)
Box.test(residuals(fit5), lag = 24, type = "Ljung")
# p value = 0.2633, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.


#ARIMA(0,1,2)(3,1,2) 
fit6 <- Arima(logTr, order = c(0,1,2), seasonal = c(3,1,2))
summary(fit6)
coeftest(fit6)
Box.test(residuals(fit6), lag = 24, type = "Ljung")
# p value = 0.3771, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.


#ARIMA(1,1,2)(0,1,2) 
fit7 <- Arima(logTr, order = c(1,1,2), seasonal = c(0,1,2))
summary(fit7)
coeftest(fit7)
Box.test(residuals(fit7), lag = 24, type = "Ljung")
# p value = 0.2905, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.


#ARIMA(1,1,2)(3,1,2) 
fit8 <- Arima(logTr, order = c(1,1,2), seasonal = c(3,1,2))
summary(fit8)
coeftest(fit8)
Box.test(residuals(fit8), lag = 24, type = "Ljung")
# p value = 0.3916, do not reject Ho
# Therefore the residuals are independently distributed.
# residuals are white noise.






















# Plots

# forecast values for 2018 Jan - 2019 Dec using ARIMA 
plot(forecast(fit4, 24))

# Fitted values anti-log with original data
fitted_val_fit3 <- fitted(fit4) 
antilog_fitted <- exp(fitted_val_fit3)
# plot actual with fitted for ARIMA
df1 <- data.frame(actual = as.numeric(training_set), 
                  fitted = as.numeric(antilog_fitted),
                  date = seq(as.Date("2009/1/1"), by = "month", length.out = 108))

df1 %>% 
  pivot_longer(1:2, names_to = "type", values_to = "revenue") %>%
  ggplot() +
    geom_line(aes(x = date, y = revenue, color = type)) +
    theme_classic() +
    labs(title = "Actual and Fitted values for ARIMA(0,1,2)(0,1,2)12", 
       x = "Year", 
       y = "US$ million",
       subtitle = "Time period : 2009-January - 2017-December",
       caption = "Figure 6") +
    theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 
    








# Holt winter's 

fit_hw <- hw(training_set,seasonal="multiplicative")
summary(fit_hw)
Box.test(residuals(fit_hw), lag = 24, type = "Ljung")

# forecast values for 2018 Jan - 2019 Dec using Holt Winter's
plot(fit_hw)
  
# plot actual with fitted for Holt
df2 <- data.frame(actual = as.numeric(training_set), 
                  fitted = as.numeric(fit_hw$fitted),
                  date = seq(as.Date("2009/1/1"), by = "month", length.out = 108))

df2 %>% 
  pivot_longer(1:2, names_to = "type", values_to = "revenue") %>%
  ggplot() +
  geom_line(aes(x = date, y = revenue, color = type)) +
  theme_classic() +
  labs(title = "Actual and Fitted values for Holt Winter's Smoothing", 
       x = "Year", 
       y = "US$ million",
       subtitle = "Time period : 2009-January - 2017-December",
       caption = "Figure 7") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 



# plot three together
df3 <- data.frame(Actual = as.numeric(training_set), 
                  ARIMA = as.numeric(antilog_fitted),
                  HW = as.numeric(fit_hw$fitted),
                  date = seq(as.Date("2009/1/1"), by = "month", length.out = 108))
df3 %>% 
  pivot_longer(1:3, names_to = "type", values_to = "revenue") %>%
  ggplot() +
  geom_line(aes(x = date, y = revenue, color = type)) +
  theme_classic() +
  labs(title = "Actual and Fitted values", 
       x = "Year", 
       y = "US$ million",
       subtitle = "Time period : 2009-January - 2017-December",
       caption = "Figure 8") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 15)) 



# MAPE values
# ARIMA




forecasted_values = forecast(fit4, 60)

df1 <- c(6.225009, 6.210214, 6.204253, 6.221058, 6.275169, 
         6.311128, 6.271797, 6.331055, 6.083889, 6.156162,
         6.269679, 6.326081, 6.273724, 6.258929, 6.252969, 
         6.269773, 6.323885)

exp(df1)


MAPE()




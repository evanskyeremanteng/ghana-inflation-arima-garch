

#### ARCH MODEL - ECONOMISTICS.COM



rm(list = ls ())
library(FinTS)
library(rugarch)
library(tseries)
library(dynlm)
library(vars)
#library(nlwaldTest)
library(lmtest)
library(broom)
library(car)
library(sandwich)
library(knitr)
library(forecast)
library(ggplot2)
library(pdfetch)
library(tsbox)
library(stats)
library(zoo)
library(vrtest)
library(readxl)
#setting theme 
theme_set(theme_bw())

#collecting data 
During <- read_excel("C:/Users/Huzaif/Desktop/400/My_project_work/Methodology/analysis/DuringCovid/During.xlsx")

#convert to time series 
inflation_ts_during <- ts(During$INFLATION,start = c(2019, 1),end = c(2020, 12), frequency = 12)
data.frame(inflation_ts_during)

#Visuals 
autoplot(inflation_ts_during) +
  ggtitle("Monthly Inflation Rate: January 2019 – December 2020") +
  xlab("Year") +
  ylab("Inflation (%)") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

hist(inflation_ts_during, main = "Histogram to show variance before diferencing ", freq = FALSE, col= "grey")

#Diferencing data 
inflation_diff <- diff(inflation_ts_during)
plot(inflation_diff, main = "First Difference of Inflation Time Series",ylab = "Differenced Inflation", xlab = "Time")
hist(inflation_diff, main = "Histogram to show variance after diferencing ", freq = FALSE, col= "grey")


#Normality testing 
shapiro.test(inflation_ts_during)

#Determine AR and MA components 
acf.inflation_diff <- acf(inflation_diff, main = "Order of MA", lag.max = 50)
pacf.inflation_diff <- pacf(inflation_diff, main = "Order of AR", lag.max = 50)

#Estimating ARIMA model 

forecast::auto.arima(inflation_ts_during)


#arima011 <- arima(inflation_diff, order = c(0,1,1))
#summary(arima011)
#tsdiag(arima011)
#checkresiduals(arima011)
#autoplot(arima011)

#arimar <- arima011$residuals
#ggtsdisplay(arimar, main = "GSP ARIMA residulas")

#step 5: Estimating mean equation r = beta + error 

#gsp.mean <- dynlm(arimar ~ 1)
#summary(gsp.mean)

#step 6: Determination of ARCH effects 
ehatsq <- ts(resid(gsp.mean)^2)
gsp.archef <- dynlm(ehatsq ~ L(ehatsq ))
summary(gsp.archef)

gsp.archef1 <- ArchTest(arimar , lags=1, demean = TRUE)
gsp.archef1


#Forecasting
fit <- auto.arima(inflation_ts_during)

summary(fit)

forecast(fit, h = 12)

# Ljung-Box test
Box.test(residuals(arima011),
         lag = 12,
         type = "Ljung-Box")

# Residual plots
checkresiduals(arima011)
accuracy(fit)

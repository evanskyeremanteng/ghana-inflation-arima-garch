

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
Pre <- read_excel("C:/Users/Huzaif/Desktop/400/My_project_work/Methodology/Pre.xlsx")

#convert to time series 
inflation_ts <- ts(Pre$INFLATION,start = c(2017, 1),end = c(2018, 12), frequency = 12)
data.frame(inflation_ts)

#Visuals 
autoplot(inflation_ts) +
  ggtitle("Monthly Inflation Rate: January 2017 – December 2018") +
  xlab("Year") +
  ylab("Inflation (%)") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

hist(inflation_ts, main = "Histogram to show variance before diferencing ", freq = FALSE, col= "grey")


#checking for stationarity 
#adf.test(inflation_diff)
adf.test(inflation_ts)


#Diferencing data 
inflation_diff <- diff(inflation_ts)
autoplot(inflation_diff) +
  ggtitle("Differenced pre Covid data") +
  xlab("Year") +
  ylab("Inflation (%)") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )
hist(inflation_diff, main = "Histogram to show variance after diferencing ", freq = FALSE, col= "grey")

adf.test(inflation_diff)

#Second differencing 
inflation_diff2 <- diff(inflation_diff)
autoplot(inflation_diff2) +
  ggtitle("2nd Differenced pre Covid data") +
  xlab("Year") +
  ylab("Inflation (%)") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

adf.test(inflation_diff2)

#Normality testing 
shapiro.test(inflation_ts)

#Determine AR and MA components 
acf.inflation_diff <- acf(inflation_diff2, main = "Order of MA", lag.max = 50)
pacf.inflation_diff <- pacf(inflation_diff2, main = "Order of AR", lag.max = 50)


#Estimating ARIMA model 
fit <- auto.arima(inflation_ts)

summary(fit)
shapiro.test(residuals(fit))
forecast(fit, h = 12)

#Accuracy for forecasting
accuracy(fit)


ggtsdisplay(arimar, main = "GSP ARIMA residulas")

#step 5: Estimating mean equation r = beta + error 

gsp.mean <- dynlm(arimar ~ 1)
summary(gsp.mean)

#step 6: Determination of ARCH effects 
ehatsq <- ts(resid(gsp.mean)^2)
gsp.archef <- dynlm(ehatsq ~ L(ehatsq ))
summary(gsp.archef)

gsp.archef1 <- ArchTest(arimar , lags=1, demean = TRUE)
gsp.archef1


# Ljung-Box test
Box.test(residuals(arima011),
         lag = 12,
         type = "Ljung-Box")

# Residual plots
checkresiduals(arima011)
  


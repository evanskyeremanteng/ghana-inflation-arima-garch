

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
library(readxl)
Post <- read_excel("C:/Users/Huzaif/Desktop/400/My_project_work/Methodology/analysis/PostCovid/Post.xlsx")

#convert to time series 
inflation_ts_post <- ts(Post$INFLATION,start = c(2021, 1),end = c(2023, 12), frequency = 12)
data.frame(inflation_ts_post)

#Visuals 
autoplot(inflation_ts_post) +
  ggtitle("Monthly Inflation Rate: January 2021 – December 2023") +
  xlab("Year") +
  ylab("Inflation (%)") + 
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold")
  )

hist(inflation_ts_post, main = "Histogram to show variance before diferencing ", freq = FALSE, col= "grey")

#Differencing data 
inflation_diff <- diff(inflation_ts_post)
plot(inflation_diff, main = "First Difference of Inflation Time Series",ylab = "Differenced Inflation", xlab = "Time")
hist(inflation_diff, main = "Histogram to show variance after diferencing ", freq = FALSE, col= "grey")

#Second differencing 
inflation_diff2 <- diff(inflation_diff)
plot(inflation_diff2, main = "Second Difference of Inflation Time Series",ylab = "Differenced Inflation", xlab = "Time")
hist(inflation_diff2, main = "Histogram to show variance after second diferencing ", freq = FALSE, col= "grey")


#Normality testing 
shapiro.test(inflation_ts)

#Determine AR and MA components 
acf.inflation_diff <- acf(inflation_diff2, main = "Order of MA", lag.max = 50)
pacf.inflation_diff <- pacf(inflation_diff2, main = "Order of AR", lag.max = 50)


#Estimating ARIMA model 

forecast::auto.arima(inflation_ts_post)

#step 5: Estimating mean equation r = beta + error 

#gsp.mean <- dynlm(arimar ~ 1)
#summary(gsp.mean)

#step 6: Determination of ARCH effects 
ehatsq <- ts(resid(gsp.mean)^2)
gsp.archef <- dynlm(ehatsq ~ L(ehatsq ))
summary(gsp.archef)

gsp.archef1 <- ArchTest(arimar , lags=1, demean = TRUE)
gsp.archef1

#Fitting arima

fit <- auto.arima(inflation_ts)

summary(fit)








ggplot(inflation_ts, aes(x = Time, y = Inflation, linetype = Series)) +
  geom_line(linewidth = 1) +
  geom_vline(
    xintercept = 2024,
    linetype = "dotted"
  ) +
  labs(
    title = "Monthly Inflation Rate and 12-Month ARIMA Forecast",
    x = "Year",
    y = "Inflation (%)",
    linetype = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )










# Forecast the fitted ARIMA model
forecast_arima <- forecast(arima011, h = 12)

# Plot historical data and forecast
autoplot(forecast_arima) +
  autolayer(inflation_ts, series = "Historical Inflation") +
  ggtitle("12-Month Inflation Forecast Using ARIMA") +
  xlab("Year") +
  ylab("Inflation (%)") +
  guides(colour = guide_legend(title = "Series")) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


forecast(fit, h = 12)


#Forecast the fitted ARIMA model
forecast_arima <- forecast(arima011, h = 12)

# Plot forecast
autoplot(forecast_arima) +
  ggtitle("12-Month Inflation Forecast") +
  xlab("Time") +
  ylab("Inflation")






# Display forecast values
forecast_arima


# Ljung-Box test
Box.test(residuals(fit),
         lag = 12,
         type = "Ljung-Box")

# Residual plots
checkresiduals(fit)

#Accuracy for forecasting+
accuracy(fit)

residue<-fit$residuals
LM <- ArchTest(residue , lags=1, demean = TRUE)


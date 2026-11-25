#########################
# CHAPTER 4: ANALYSIS
# Project: Mathematical modelling and forecasting of montly inflation rate in Ghana Using ARIMA GARCH model
#########################



# ============================
# 1. Load Packages
# ============================
library(forecast)
library(tseries)
library(ggplot2)
library(FinTS)
library(dynlm)
library(lmtest)
library(knitr)
library(readxl)
library(e1071)

# Set theme
theme_set(theme_bw())

# ============================
# 2. Load and Prepare Data
# ============================
During <- read_excel("C:/Users/Huzaif/Desktop/400/My_project_work/Methodology/analysis/DuringCovid/During.xlsx")
inflation_ts_during <- ts(During$INFLATION, 
                   start = c(2019, 1), 
                   end = c(2020, 12), 
                   frequency = 12)

# ============================
# 3. Summary Statistics
# ============================
summary_stats_during <- data.frame(
  Statistic = c("Mean", "Median", "Std. Deviation", "Minimum", 
                "Maximum", "Skewness", "Kurtosis", "Observations"),
  Value = round(c(mean(inflation_ts_during), median(inflation_ts_during), sd(inflation_ts_during),
                  min(inflation_ts_during), max(inflation_ts_during), 
                  skewness(inflation_ts_during), kurtosis(inflation_ts_during),
                  length(inflation_ts_during)), 4)
)
print("Table 4.1: Summary Statistics")
print(summary_stats_during)

# ============================
# 4. Visualizations
# ============================
# Time series plot
autoplot(inflation_ts_during) +
  ggtitle(" Monthly Inflation Rate (2019 - 2020) ") +
  xlab("Year") +
  ylab("Inflation Rate (%)")

# ============================
# 5. Stationarity Tests
# ============================
adf.test(inflation_ts_during)
kpss.test(inflation_ts_during)

first_diff <- diff(inflation_ts_during)
adf.test(first_diff)
kpss.test(first_diff)

#Determine AR and MA components 
acf.inflation_diff_during <- acf(first_diff, main = "Order of MA", lag.max = 50)
pacf.inflation_diff_during <- pacf(first_diff, main = "Order of AR", lag.max = 50)


#==================Plot of differenced data==
autoplot(diff(inflation_ts_during)) +
  ggtitle("1st Differenced data") +
  xlab("Year") +
  ylab("Inflation Rate (%)")



# ============================
# 6. ARIMA Model Estimation
# ============================
fit_during <- auto.arima(inflation_ts_during, stepwise = FALSE, approximation = FALSE)
cat("\nSelected ARIMA Model:", fit_during$arma, "\n")
print(summary(fit_during))

# ============================
# 7. Diagnostic Tests
# ============================
shapiro_test_during <- shapiro.test(residuals(fit_during))
box_test_during <- Box.test(residuals(fit_during), type = "Ljung-Box", lag = 10)
arch_test_during <- ArchTest(residuals(fit_during), lags = 1, demean = TRUE)

cat("\n=== Diagnostic Tests ===\n")
cat("Shapiro-Wilk:", shapiro_test_during$p.value, "\n")
cat("Ljung-Box:", box_test_during$p.value, "\n")
cat("ARCH Test:", arch_test_during$p.value, "\n")

resid_fit_during<-fit_during$residuals
hist(resid_fit_during, breaks = 20, freq = FALSE, 
     main = "Histogram of Residuals", xlab = "Residuals")
curve(dnorm(x, mean = mean(resid_fit_during), sd = sd(resid_fit_during)), 
      add = TRUE, col = "red", lwd = 2)
# ============================
# 8. Model Comparison
# ============================
model_comp_during <- data.frame(
  Model = c("ARIMA(auto)", "ARIMA(1,1,1)", "ARIMA(0,1,1)"),
  AIC = c(AIC(fit_during), 
          AIC(Arima(inflation_ts_during, c(1,1,1))),
          AIC(Arima(inflation_ts_during, c(0,1,1)))),
  BIC = c(BIC(fit_during),
          BIC(Arima(inflation_ts_during, c(1,1,1))),
          BIC(Arima(inflation_ts_during, c(0,1,1))))
)
print("\n  Model Comparison")
print(model_comp_during)

#=============================
# Fit all three models 
fit1 <- fit_during                 
fit2 <- Arima(inflation_ts_during, order = c(1,1,1))
fit3 <- Arima(inflation_ts_during, order = c(0,1,1))

# Get in-sample accuracy for each
acc1 <- accuracy(fit1)
acc2 <- accuracy(fit2)
acc3 <- accuracy(fit3)

# Combine into a single comparison data frame
accuracy_comp <- data.frame(
  Model = c("ARIMA(0,1,0)", "ARIMA(1,1,1)", "ARIMA(0,1,1)"),
  RMSE  = c(acc1[, "RMSE"], acc2[, "RMSE"], acc3[, "RMSE"]),
  MAE   = c(acc1[, "MAE"],  acc2[, "MAE"],  acc3[, "MAE"]),
  MAPE  = c(acc1[, "MAPE"], acc2[, "MAPE"], acc3[, "MAPE"]),
  MASE  = c(acc1[, "MASE"], acc2[, "MASE"], acc3[, "MASE"])
)

print(accuracy_comp)

#=============================

# ============================
# 9. Forecasting
# ============================
forecast_result_during <- forecast(fit_during, h = 12)
print("\nTable 4.7: 12-Month Forecast")
print(forecast_result_during)


#=============================
df <- data.frame(
  time     = time(inflation_ts_during),
  actual   = as.numeric(inflation_ts_during),
  fitted   = as.numeric(fitted(fit3))
)

ggplot(df, aes(x = time)) +
  geom_line(aes(y = actual, color = "Actual"), linewidth = 1) +
  geom_line(aes(y = fitted, color = "Fitted/Predicted"), linewidth = 1) +
  scale_color_manual(values = c("Actual" = "red", "Fitted/Predicted" = "green")) +
  scale_x_continuous(breaks = seq(floor(min(df$time)), ceiling(max(df$time)), by = 1)) +
  labs(title = "Actual vs Fitted Inflation Values",
       x = "Time", y = "Inflation Rate", color = "Series") +
  theme_minimal()

#=============================
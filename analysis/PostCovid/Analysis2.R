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
Post <- read_excel("C:/Users/Huzaif/Desktop/400/My_project_work/Methodology/analysis/PostCovid/Post.xlsx")
inflation_ts_post <- ts(Post$INFLATION, 
                          start = c(2021, 1), 
                          end = c(2023, 12), 
                          frequency = 12)

# ============================
# 3. Summary Statistics
# ============================
summary_stats_post <- data.frame(
  Statistic = c("Mean", "Median", "Std. Deviation", "Minimum", 
                "Maximum", "Skewness", "Kurtosis", "Observations"),
  Value = round(c(mean(inflation_ts_post), median(inflation_ts_post), sd(inflation_ts_post),
                  min(inflation_ts_post), max(inflation_ts_post), 
                  skewness(inflation_ts_post), kurtosis(inflation_ts_post),
                  length(inflation_ts_post)), 4)
)
print("Table 4.1: Summary Statistics")
print(summary_stats_post)

# ============================
# 4. Visualizations
# ============================
# Time series plot
autoplot(inflation_ts_post) +
  ggtitle(" Monthly Inflation Rate (2021 - 2023) ") +
  xlab("Year") +
  ylab("Inflation Rate (%)")

# ============================
# 5. Stationarity Tests
# ============================
adf.test(inflation_ts_post)
kpss.test(inflation_ts_post)

first_diff_post <- diff(inflation_ts_post)
adf.test(first_diff_post)
kpss.test(first_diff_post)

second_diff_post <- diff(first_diff_post)
adf.test(second_diff_post)

#Determine AR and MA components 
acf.inflation_diff_post <- acf(first_diff, main = "Order of MA", lag.max = 50)
pacf.inflation_diff_post <- pacf(first_diff, main = "Order of AR", lag.max = 50)


#==================Plot of differenced data==
autoplot(first_diff_post) +
  ggtitle("1st Differenced data") +
  xlab("Year") +
  ylab("Inflation Rate (%)")

autoplot(second_diff_post) +
  ggtitle("2nd Differenced data") +
  xlab("Year") +
  ylab("Inflation Rate (%)")


# ============================
# 6. ARIMA Model Estimation
# ============================
fit_post <- auto.arima(inflation_ts_post, stepwise = FALSE, approximation = FALSE)
cat("\nSelected ARIMA Model:", fit_post$arma, "\n")
print(summary(fit_post))

# ============================
# 7. Diagnostic Tests
# ============================
shapiro_test_post <- shapiro.test(residuals(fit_post))
box_test_post <- Box.test(residuals(fit_post), type = "Ljung-Box", lag = 10)
arch_test_post <- ArchTest(residuals(fit_post), lags = 1, demean = TRUE)

cat("\n=== Diagnostic Tests ===\n")
cat("Shapiro-Wilk:", shapiro_test_post$p.value, "\n")
cat("Ljung-Box:", box_test_post$p.value, "\n")
cat("ARCH Test:", arch_test_post$p.value, "\n")

resid_fit_post<-fit_post$residuals
hist(resid_fit_post, breaks = 20, freq = FALSE, 
     main = "Histogram of Residuals", xlab = "Residuals")
curve(dnorm(x, mean = mean(resid_fit_post), sd = sd(resid_fit_post)), 
      add = TRUE, col = "red", lwd = 2)
# ============================
# 8. Model Comparison
# ============================
model_comp_post <- data.frame(
  Model = c("ARIMA(1,1,0)(0,0,1)[12])", "ARIMA(1,2,1)", "ARIMA(1,2,0)"),
  AIC = c(AIC(fit_post), 
          AIC(Arima(inflation_ts_post, order = c(1,2,1))),
          AIC(Arima(inflation_ts_post, order = c(1,2,0)))),
  BIC = c(BIC(fit_post),
          BIC(Arima(inflation_ts_post, order = c(1,2,1))),
          BIC(Arima(inflation_ts_post, order = c(1,2,0))))
)
print("Model Comparison")
print(model_comp_post)

#=============================
# Fit all three models
fit1 <- fit_post                     # ARIMA(auto): (1,1,0)(0,0,1)[12]
fit2 <- Arima(inflation_ts_post, order = c(1,2,1))
fit3 <- Arima(inflation_ts_post, order = c(1,2,0))

# Get in-sample accuracy for each
acc1 <- accuracy(fit1)
acc2 <- accuracy(fit2)
acc3 <- accuracy(fit3)

accuracy_comp_post <- data.frame(
  Model = c("ARIMA(auto)", "ARIMA(1,2,1)", "ARIMA(1,2,0)"),
  RMSE  = c(acc1[, "RMSE"], acc2[, "RMSE"], acc3[, "RMSE"]),
  MAE   = c(acc1[, "MAE"],  acc2[, "MAE"],  acc3[, "MAE"]),
  MAPE  = c(acc1[, "MAPE"], acc2[, "MAPE"], acc3[, "MAPE"]),
  MASE  = c(acc1[, "MASE"], acc2[, "MASE"], acc3[, "MASE"])
)

print(accuracy_comp_post)

#=============================
# Fit all three models 
fit1 <- fit_post                
fit2 <- Arima(inflation_ts_post, order = c(1,1,1))
fit3 <- Arima(inflation_ts_post, order = c(0,1,1))

# Get in-sample accuracy for each
acc1 <- accuracy(fit1)
acc2 <- accuracy(fit2)
acc3 <- accuracy(fit3)

# Combine into a single comparison data frame
accuracy_comp <- data.frame(
  Model = c("ARIMA(1,1,0)(0,0,1)_[12]", "ARIMA(1,2,1)", "ARIMA(1,2,0)"),
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
forecast_result_post <- forecast(fit_post, h = 12)
print("\nTable 4.7: 12-Month Forecast")
print(forecast_result_post)


#=============================
df <- data.frame(
  time     = time(inflation_ts_post),
  actual   = as.numeric(inflation_ts_post),
  fitted   = as.numeric(fitted(fit_post))
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




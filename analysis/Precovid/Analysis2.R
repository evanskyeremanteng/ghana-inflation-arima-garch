#########################
# CHAPTER 4: ANALYSIS
# Inflation Rate Forecasting using ARIMA Model
# Project: [Your Project Title]
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
Pre <- read_excel("C:/Users/Huzaif/Desktop/400/My_project_work/Methodology/Pre.xlsx")
inflation_ts <- ts(Pre$INFLATION, 
                   start = c(2017, 1), 
                   end = c(2018, 12), 
                   frequency = 12)

# ============================
# 3. Summary Statistics
# ============================
summary_stats <- data.frame(
  Statistic = c("Mean", "Median", "Std. Deviation", "Minimum", 
                "Maximum", "Skewness", "Kurtosis", "Observations"),
  Value = round(c(mean(inflation_ts), median(inflation_ts), sd(inflation_ts),
                  min(inflation_ts), max(inflation_ts), 
                  skewness(inflation_ts), kurtosis(inflation_ts),
                  length(inflation_ts)), 4)
)
print("Table 4.1: Summary Statistics")
print(summary_stats)

# ============================
# 4. Visualizations
# ============================
# Time series plot
autoplot(inflation_ts) +
  ggtitle("Monthly Inflation Rate") +
  xlab("Year") +
  ylab("Inflation Rate (%)")

# ============================
# 5. Stationarity Tests
# ============================
adf_original <- adf.test(inflation_ts)
cat("\nADF Test (Original):", adf_original$statistic, 
    "p-value:", adf_original$p.value, "\n")

adf_diff <- adf.test(diff(inflation_ts))
cat("ADF Test (First Diff):", adf_diff$statistic, 
    "p-value:", adf_diff$p.value, "\n")

# ============================
# 6. ARIMA Model Estimation
# ============================
fit <- auto.arima(inflation_ts, stepwise = FALSE, approximation = FALSE)
cat("\nSelected ARIMA Model:", fit$arma, "\n")
print(summary(fit))

# ============================
# 7. Diagnostic Tests
# ============================
shapiro_test <- shapiro.test(residuals(fit))
box_test <- Box.test(residuals(fit), type = "Ljung-Box", lag = 10)
arch_test <- ArchTest(residuals(fit), lags = 1, demean = TRUE)

cat("\n=== Diagnostic Tests ===\n")
cat("Shapiro-Wilk:", shapiro_test$p.value, "\n")
cat("Ljung-Box:", box_test$p.value, "\n")
cat("ARCH Test:", arch_test$p.value, "\n")

resid_fit<-fit$residuals
hist(resid_fit, breaks = 20, freq = FALSE, 
     main = "Histogram of Residuals", xlab = "Residuals")
curve(dnorm(x, mean = mean(resid_fit), sd = sd(resid_fit)), 
      add = TRUE, col = "red", lwd = 2)
# ============================
# 8. Model Comparison
# ============================
model_comp <- data.frame(
  Model = c("ARIMA(auto)", "ARIMA(1,1,1)", "ARIMA(2,1,2)"),
  AIC = c(AIC(fit), 
          AIC(Arima(inflation_ts, c(1,1,1))),
          AIC(Arima(inflation_ts, c(2,1,2)))),
  BIC = c(BIC(fit),
          BIC(Arima(inflation_ts, c(1,1,1))),
          BIC(Arima(inflation_ts, c(2,1,2))))
)
print("\nTable 4.5: Model Comparison")
print(model_comp)

#=============================
# Fit all three models (reuse if already fitted)
fit1 <- fit                                   # ARIMA(auto)
fit2 <- Arima(inflation_ts, order = c(1,1,1))
fit3 <- Arima(inflation_ts, order = c(2,1,2))

# Get in-sample accuracy for each
acc1 <- accuracy(fit1)
acc2 <- accuracy(fit2)
acc3 <- accuracy(fit3)

# Combine into a single comparison data frame
accuracy_comp <- data.frame(
  Model = c("ARIMA(auto)", "ARIMA(1,1,1)", "ARIMA(2,1,2)"),
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
forecast_result <- forecast(fit, h = 12)
print("\nTable 4.7: 12-Month Forecast")
print(forecast_result)


#=============================
df <- data.frame(
  time     = time(inflation_ts),
  actual   = as.numeric(inflation_ts),
  fitted   = as.numeric(fitted(fit))
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








# ============================
# 10. Save Results
# ============================
saveRDS(fit, "final_model.rds")
write.csv(as.data.frame(forecast_result), "forecast_results.csv")
cat("\nAnalysis complete. Results saved.\n")
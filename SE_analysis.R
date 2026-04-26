# =========================================================
# Forecasting Monthly Tourist Arrivals in Vienna (2022–2025)
# R script
# =========================================================

# -------------------------
# 0. Packages
# -------------------------
library(forecast)
library(tseries)
library(lmtest)

# -------------------------
# 1.prepare data
# -------------------------
setwd("/Users/susan/Desktop/SE/data")

df <- read.csv("vienna_tourism.csv", stringsAsFactors = FALSE)

# Check data
colnames(df)
head(df)
str(df)

# Convert variables
df$Time <- as.Date(df$Time)

df$Arrivals <- as.numeric(df$Arrivals)
df$vienna.austria <- as.numeric(df$vienna.austria)
df$hotel.vienna <- as.numeric(df$hotel.vienna)
df$airport.vienna <- as.numeric(df$airport.vienna)
df$Vienna.hostel <- as.numeric(df$Vienna.hostel)

# Time series objects
arrivals_ts <- ts(df$Arrivals, start = c(2022, 1), frequency = 12)
austria_ts  <- ts(df$vienna.austria, start = c(2022, 1), frequency = 12)
hotel_ts    <- ts(df$hotel.vienna, start = c(2022, 1), frequency = 12)
airport_ts  <- ts(df$airport.vienna, start = c(2022, 1), frequency = 12)
hostel_ts   <- ts(df$Vienna.hostel, start = c(2022, 1), frequency = 12)

# -------------------------
# 2. Table 1: Descriptive statistics
# -------------------------
period <- format(df$Time, "%Y-%m")

get_stats <- function(x, varname, period) {
  min_idx <- which.min(x)
  max_idx <- which.max(x)
  
  data.frame(
    Variable = varname,
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD = round(sd(x, na.rm = TRUE), 2),
    Min = round(min(x, na.rm = TRUE), 2),
    Min_Period = period[min_idx],
    Max = round(max(x, na.rm = TRUE), 2),
    Max_Period = period[max_idx]
  )
}

table1 <- rbind(
  get_stats(df$Arrivals, "Arrivals", period),
  get_stats(df$vienna.austria, "Vienna Austria", period),
  get_stats(df$hotel.vienna, "Hotel Vienna", period),
  get_stats(df$airport.vienna, "Airport Vienna", period),
  get_stats(df$Vienna.hostel, "Vienna Hostel", period)
)

print(table1)

# -------------------------
# 3. Figure 1: Time series plots
# -------------------------

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))

plot(arrivals_ts, type = "l", lwd = 2,
     main = "Monthly Tourist Arrivals in Vienna",
     ylab = "Arrivals", xlab = "Year")

plot(austria_ts, type = "l", lwd = 2,
     main = "Google Trends: Vienna Austria",
     ylab = "Index", xlab = "Year")

plot(hotel_ts, type = "l", lwd = 2,
     main = "Google Trends: Hotel Vienna",
     ylab = "Index", xlab = "Year")

plot(airport_ts, type = "l", lwd = 2,
     main = "Google Trends: Airport Vienna",
     ylab = "Index", xlab = "Year")

plot(hostel_ts, type = "l", lwd = 2,
     main = "Google Trends: Vienna Hostel",
     ylab = "Index", xlab = "Year")

par(mfrow = c(1, 1))

# -------------------------
# 4. ADF tests
# -------------------------
cat("--- ADF Test for Arrivals ---\n")
print(adf.test(arrivals_ts))

cat("--- ADF Test for Vienna Austria ---\n")
print(adf.test(austria_ts))

cat("--- ADF Test for Hotel Vienna ---\n")
print(adf.test(hotel_ts))

cat("--- ADF Test for Airport Vienna ---\n")
print(adf.test(airport_ts))

cat("--- ADF Test for Vienna Hostel ---\n")
print(adf.test(hostel_ts))

# First differences
arrivals_diff1 <- diff(arrivals_ts)
austria_diff1  <- diff(austria_ts)
hotel_diff1    <- diff(hotel_ts)
airport_diff1  <- diff(airport_ts)
hostel_diff1   <- diff(hostel_ts)

cat("--- ADF Test for First-Differenced Arrivals ---\n")
print(adf.test(arrivals_diff1))

cat("--- ADF Test for First-Differenced Vienna Austria ---\n")
print(adf.test(austria_diff1))

cat("--- ADF Test for First-Differenced Hotel Vienna ---\n")
print(adf.test(hotel_diff1))

cat("--- ADF Test for First-Differenced Airport Vienna ---\n")
print(adf.test(airport_diff1))

cat("--- ADF Test for First-Differenced Vienna Hostel ---\n")
print(adf.test(hostel_diff1))

# Seasonal / combined differencing for Arrivals
arrivals_seasdiff <- diff(arrivals_ts, lag = 12)
arrivals_bothdiff <- diff(diff(arrivals_ts), lag = 12)

cat("--- ADF Test for Seasonal-Differenced Arrivals ---\n")
print(adf.test(arrivals_seasdiff))

cat("--- ADF Test for First + Seasonal Differenced Arrivals ---\n")
print(adf.test(arrivals_bothdiff))

# Second differences for appendix variables
austria_diff2 <- diff(austria_ts, differences = 2)
airport_diff2 <- diff(airport_ts, differences = 2)

cat("--- ADF Test for Second-Differenced Vienna Austria ---\n")
print(adf.test(austria_diff2))

cat("--- ADF Test for Second-Differenced Airport Vienna ---\n")
print(adf.test(airport_diff2))

# -------------------------
# 5. CCF plots 
# -------------------------
arrivals_use <- diff(diff(arrivals_ts), lag = 12)
hotel_use    <- diff(hotel_ts)
hostel_use   <- diff(hostel_ts)

hotel_use <- window(
  hotel_use,
  start = time(arrivals_use)[1],
  end   = time(arrivals_use)[length(arrivals_use)]
)

hostel_use <- window(
  hostel_use,
  start = time(arrivals_use)[1],
  end   = time(arrivals_use)[length(arrivals_use)]
)


par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

ccf(hotel_use, arrivals_use,
    lag.max = 12,
    main = "CCF: Hotel Vienna and Arrivals")

ccf(hostel_use, arrivals_use,
    lag.max = 12,
    main = "CCF: Vienna Hostel and Arrivals")

par(mfrow = c(1, 1))

# -------------------------
# 6. Granger tests 
# -------------------------
g_h1 <- grangertest(arrivals_use ~ hotel_use, order = 1)
g_h2 <- grangertest(arrivals_use ~ hotel_use, order = 2)
g_h3 <- grangertest(arrivals_use ~ hotel_use, order = 3)

print(g_h1)
print(g_h2)
print(g_h3)

g_hs1 <- grangertest(arrivals_use ~ hostel_use, order = 1)
g_hs2 <- grangertest(arrivals_use ~ hostel_use, order = 2)
g_hs3 <- grangertest(arrivals_use ~ hostel_use, order = 3)

print(g_hs1)
print(g_hs2)
print(g_hs3)

table3 <- data.frame(
  Variable = c("Hotel Vienna", "Hotel Vienna", "Hotel Vienna",
               "Vienna Hostel", "Vienna Hostel", "Vienna Hostel"),
  Lag = c(1, 2, 3, 1, 2, 3),
  p_value = round(c(
    g_h1$`Pr(>F)`[2],
    g_h2$`Pr(>F)`[2],
    g_h3$`Pr(>F)`[2],
    g_hs1$`Pr(>F)`[2],
    g_hs2$`Pr(>F)`[2],
    g_hs3$`Pr(>F)`[2]
  ), 4)
)

table3$Significance <- ifelse(table3$p_value < 0.01, "**",
                              ifelse(table3$p_value < 0.05, "*", ""))

print(table3)

# -------------------------
# 7. Forecast comparison function
# -------------------------
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

calc_metrics <- function(actual, forecast_obj) {
  pred <- as.numeric(forecast_obj$mean)
  c(
    RMSE = rmse(actual, pred),
    MAPE = mape(actual, pred)
  )
}

run_forecast_comparison <- function(y, x, lag_k, label, test_len = 6) {
  
  n <- length(y)
  common_start <- lag_k + 1
  common_end <- n
  
  y_all <- y[common_start:common_end]
  x_lag <- x[(common_start - lag_k):(common_end - lag_k)]
  
  n_common <- length(y_all)
  train_idx <- 1:(n_common - test_len)
  test_idx  <- (n_common - test_len + 1):n_common
  
  y_train <- ts(y_all[train_idx], frequency = 12)
  y_test  <- y_all[test_idx]
  
  x_train <- matrix(x_lag[train_idx], ncol = 1)
  x_test  <- matrix(x_lag[test_idx], ncol = 1)
  
  fit_base <- auto.arima(
    y_train,
    d = 1,
    D = 1,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  fc_base <- forecast(fit_base, h = test_len)
  base_metrics <- calc_metrics(y_test, fc_base)
  
  fit_x <- auto.arima(
    y_train,
    xreg = x_train,
    d = 1,
    D = 1,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  fc_x <- forecast(fit_x, xreg = x_test, h = test_len)
  x_metrics <- calc_metrics(y_test, fc_x)
  
  out <- data.frame(
    Model = c("Baseline SARIMA", paste0("SARIMAX (", label, ", lag ", lag_k, ")")),
    RMSE = round(c(base_metrics["RMSE"], x_metrics["RMSE"]), 2),
    MAPE = round(c(base_metrics["MAPE"], x_metrics["MAPE"]), 2)
  )
  
  return(list(
    baseline_model = fit_base,
    sarimax_model = fit_x,
    comparison = out
  ))
}

# -------------------------
# 8. Forecast comparison
# -------------------------
y <- df$Arrivals
hotel <- df$hotel.vienna
hostel <- df$Vienna.hostel

res_hotel_l1  <- run_forecast_comparison(y, hotel, 1, "Hotel Vienna")
res_hotel_l2  <- run_forecast_comparison(y, hotel, 2, "Hotel Vienna")
res_hotel_l3  <- run_forecast_comparison(y, hotel, 3, "Hotel Vienna")

res_hostel_l1 <- run_forecast_comparison(y, hostel, 1, "Vienna Hostel")
res_hostel_l2 <- run_forecast_comparison(y, hostel, 2, "Vienna Hostel")
res_hostel_l3 <- run_forecast_comparison(y, hostel, 3, "Vienna Hostel")

print(res_hotel_l1$comparison)
print(res_hotel_l2$comparison)
print(res_hotel_l3$comparison)

print(res_hostel_l1$comparison)
print(res_hostel_l2$comparison)
print(res_hostel_l3$comparison)

table4 <- data.frame(
  Model = c(
    "Baseline SARIMA",
    "SARIMAX (Hotel Vienna, lag 1)",
    "SARIMAX (Hotel Vienna, lag 2)",
    "SARIMAX (Hotel Vienna, lag 3)",
    "SARIMAX (Vienna Hostel, lag 1)",
    "SARIMAX (Vienna Hostel, lag 2)",
    "SARIMAX (Vienna Hostel, lag 3)"
  ),
  RMSE = c(
    res_hostel_l1$comparison$RMSE[1],
    res_hotel_l1$comparison$RMSE[2],
    res_hotel_l2$comparison$RMSE[2],
    res_hotel_l3$comparison$RMSE[2],
    res_hostel_l1$comparison$RMSE[2],
    res_hostel_l2$comparison$RMSE[2],
    res_hostel_l3$comparison$RMSE[2]
  ),
  MAPE = c(
    res_hostel_l1$comparison$MAPE[1],
    res_hotel_l1$comparison$MAPE[2],
    res_hotel_l2$comparison$MAPE[2],
    res_hotel_l3$comparison$MAPE[2],
    res_hostel_l1$comparison$MAPE[2],
    res_hostel_l2$comparison$MAPE[2],
    res_hostel_l3$comparison$MAPE[2]
  )
)

table4$Better_than_Baseline <- c(
  "Baseline",
  ifelse(table4$RMSE[2] < table4$RMSE[1] & table4$MAPE[2] < table4$MAPE[1], "Yes", "No"),
  ifelse(table4$RMSE[3] < table4$RMSE[1] & table4$MAPE[3] < table4$MAPE[1], "Yes", "No"),
  ifelse(table4$RMSE[4] < table4$RMSE[1] & table4$MAPE[4] < table4$MAPE[1], "Yes", "No"),
  ifelse(table4$RMSE[5] < table4$RMSE[1] & table4$MAPE[5] < table4$MAPE[1], "Yes", "No"),
  ifelse(table4$RMSE[6] < table4$RMSE[1] & table4$MAPE[6] < table4$MAPE[1], "Yes", "No"),
  ifelse(table4$RMSE[7] < table4$RMSE[1] & table4$MAPE[7] < table4$MAPE[1], "Yes", "No")
)

print(table4)

# -------------------------
# 9. Forecast plot
# -------------------------
test_len <- 6
n <- nrow(df)

common_start <- 2
common_end <- n

y_all <- df$Arrivals[common_start:common_end]
x_all <- df$Vienna.hostel[(common_start - 1):(common_end - 1)]

n_common <- length(y_all)
train_idx <- 1:(n_common - test_len)
test_idx  <- (n_common - test_len + 1):n_common

y_train <- ts(y_all[train_idx], frequency = 12, start = c(2022, 2))
y_test  <- y_all[test_idx]

x_train <- matrix(x_all[train_idx], ncol = 1)
x_test  <- matrix(x_all[test_idx], ncol = 1)

fit_sarima <- auto.arima(
  y_train,
  d = 1,
  D = 1,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

fit_sarimax <- auto.arima(
  y_train,
  xreg = x_train,
  d = 1,
  D = 1,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)

fc_sarima  <- forecast(fit_sarima, h = test_len)
fc_sarimax <- forecast(fit_sarimax, xreg = x_test, h = test_len)

all_dates <- seq.Date(from = as.Date("2022-01-01"), by = "month", length.out = n)
aligned_dates <- all_dates[common_start:common_end]
test_dates <- aligned_dates[test_idx]


plot(
  test_dates, y_test,
  type = "o", pch = 16, lwd = 2, col = "black",
  xlab = "Month",
  ylab = "Tourist Arrivals",
  main = "Forecast Comparison over the Test Period",
  ylim = range(c(y_test, fc_sarima$mean, fc_sarimax$mean))
)

lines(test_dates, fc_sarima$mean, type = "o", pch = 17, lwd = 2, col = "blue")
lines(test_dates, fc_sarimax$mean, type = "o", pch = 15, lwd = 2, col = "red")

legend(
  "topleft",
  legend = c("Actual", "SARIMA", "SARIMAX (Vienna Hostel, lag 1)"),
  col = c("black", "blue", "red"),
  lty = 1,
  pch = c(16, 17, 15),
  lwd = 2,
  bty = "n"
)


# -------------------------
# 10. Residual diagnostics
# -------------------------
summary(fit_sarima)
summary(fit_sarimax)

checkresiduals(fit_sarima)
checkresiduals(fit_sarimax)

# -------------------------
# 11. Appendix: Granger tests for Vienna Austria / Airport Vienna
# -------------------------
austria_use <- diff(austria_ts, differences = 2)
airport_use <- diff(airport_ts, differences = 2)

austria_use <- window(
  austria_use,
  start = time(arrivals_use)[1],
  end   = time(arrivals_use)[length(arrivals_use)]
)

airport_use <- window(
  airport_use,
  start = time(arrivals_use)[1],
  end   = time(arrivals_use)[length(arrivals_use)]
)

g_a1 <- grangertest(arrivals_use ~ austria_use, order = 1)
g_a2 <- grangertest(arrivals_use ~ austria_use, order = 2)
g_a3 <- grangertest(arrivals_use ~ austria_use, order = 3)

g_ap1 <- grangertest(arrivals_use ~ airport_use, order = 1)
g_ap2 <- grangertest(arrivals_use ~ airport_use, order = 2)
g_ap3 <- grangertest(arrivals_use ~ airport_use, order = 3)

print(g_a1)
print(g_a2)
print(g_a3)

print(g_ap1)
print(g_ap2)
print(g_ap3)

appendix_granger <- data.frame(
  Variable = c("Vienna Austria", "Vienna Austria", "Vienna Austria",
               "Airport Vienna", "Airport Vienna", "Airport Vienna"),
  Lag = c(1, 2, 3, 1, 2, 3),
  p_value = round(c(
    g_a1$`Pr(>F)`[2],
    g_a2$`Pr(>F)`[2],
    g_a3$`Pr(>F)`[2],
    g_ap1$`Pr(>F)`[2],
    g_ap2$`Pr(>F)`[2],
    g_ap3$`Pr(>F)`[2]
  ), 4)
)

appendix_granger$Significance <- ifelse(appendix_granger$p_value < 0.01, "**",
                                        ifelse(appendix_granger$p_value < 0.05, "*", ""))

print(appendix_granger)

# -------------------------
# 12. Appendix: Forecast comparison for Vienna Austria / Airport Vienna
# -------------------------
austria <- df$vienna.austria
airport <- df$airport.vienna

res_austria_l1 <- run_forecast_comparison(y, austria, 1, "Vienna Austria")
res_austria_l2 <- run_forecast_comparison(y, austria, 2, "Vienna Austria")
res_austria_l3 <- run_forecast_comparison(y, austria, 3, "Vienna Austria")

res_airport_l1 <- run_forecast_comparison(y, airport, 1, "Airport Vienna")
res_airport_l2 <- run_forecast_comparison(y, airport, 2, "Airport Vienna")
res_airport_l3 <- run_forecast_comparison(y, airport, 3, "Airport Vienna")

print(res_austria_l1$comparison)
print(res_austria_l2$comparison)
print(res_austria_l3$comparison)

print(res_airport_l1$comparison)
print(res_airport_l2$comparison)
print(res_airport_l3$comparison)

appendix_forecast <- data.frame(
  Model = c(
    "Baseline SARIMA",
    "SARIMAX (Vienna Austria, lag 1)",
    "SARIMAX (Vienna Austria, lag 2)",
    "SARIMAX (Vienna Austria, lag 3)",
    "SARIMAX (Airport Vienna, lag 1)",
    "SARIMAX (Airport Vienna, lag 2)",
    "SARIMAX (Airport Vienna, lag 3)"
  ),
  RMSE = c(
    res_austria_l1$comparison$RMSE[1],
    res_austria_l1$comparison$RMSE[2],
    res_austria_l2$comparison$RMSE[2],
    res_austria_l3$comparison$RMSE[2],
    res_airport_l1$comparison$RMSE[2],
    res_airport_l2$comparison$RMSE[2],
    res_airport_l3$comparison$RMSE[2]
  ),
  MAPE = c(
    res_austria_l1$comparison$MAPE[1],
    res_austria_l1$comparison$MAPE[2],
    res_austria_l2$comparison$MAPE[2],
    res_austria_l3$comparison$MAPE[2],
    res_airport_l1$comparison$MAPE[2],
    res_airport_l2$comparison$MAPE[2],
    res_airport_l3$comparison$MAPE[2]
  )
)

appendix_forecast$Better_than_Baseline <- c(
  "Baseline",
  ifelse(appendix_forecast$RMSE[2] < appendix_forecast$RMSE[1] &
           appendix_forecast$MAPE[2] < appendix_forecast$MAPE[1], "Yes", "No"),
  ifelse(appendix_forecast$RMSE[3] < appendix_forecast$RMSE[1] &
           appendix_forecast$MAPE[3] < appendix_forecast$MAPE[1], "Yes", "No"),
  ifelse(appendix_forecast$RMSE[4] < appendix_forecast$RMSE[1] &
           appendix_forecast$MAPE[4] < appendix_forecast$MAPE[1], "Yes", "No"),
  ifelse(appendix_forecast$RMSE[5] < appendix_forecast$RMSE[1] &
           appendix_forecast$MAPE[5] < appendix_forecast$MAPE[1], "Yes", "No"),
  ifelse(appendix_forecast$RMSE[6] < appendix_forecast$RMSE[1] &
           appendix_forecast$MAPE[6] < appendix_forecast$MAPE[1], "Yes", "No"),
  ifelse(appendix_forecast$RMSE[7] < appendix_forecast$RMSE[1] &
           appendix_forecast$MAPE[7] < appendix_forecast$MAPE[1], "Yes", "No")
)

print(appendix_forecast)
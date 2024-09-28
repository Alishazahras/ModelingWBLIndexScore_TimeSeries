library(tseries)
library(forecast)
library(ggplot2)
library(TTR)
library(TSA)
library(Metrics)
library(car)
library(lmtest)
library(nortest)

data=data_wbl
datats=ts(data[2],start=1970, end=2023)

datats
ts.plot(datats)

# Split Train-Test
len = length(datats)
split_point <- floor(0.8 * len)
train = datats[1:split_point]
test = datats[(split_point+1):len]

print(length(train))

print(length(test))

train_ts = ts(train, frequency=1, start=1970, end=2012)
print(train_ts)

test_ts = ts(test, frequency=1, start=2013, end=2023)
print(test_ts)

eval_metrics = c("RMSE", "MSE", "MAPE", "MAE")

########################## Naive Model 1 #############################################
# Define the custom naive model with drift
naive_model1 <- function(train_ts, h) {
  forecast_values <- numeric(h)
  last_value <- train_ts[length(train_ts)]
  drift <- train_ts[length(train_ts)] - train_ts[length(train_ts) - 1]
  for (i in 1:h) {
    forecast_values[i] <- last_value + drift
    last_value <- forecast_values[i]
  }
  return(forecast_values)
}

# Calculate fitted values for the training data
train_fitted <- numeric(length(train_ts))
train_fitted[1] <- train_ts[1]
for (i in 2:length(train_ts)) {
  if (i == 2) {
    drift <- 0
  } else {
    drift <- train_ts[i-1] - train_ts[i-2]
  }
  train_fitted[i] <- train_ts[i-1] + drift
}

# Calculate RMSE, MSE, MAPE, and MAE for the training data
train_rmse_naive1 = rmse(train_ts, train_fitted)
train_mse_naive1 = mse(train_ts, train_fitted)
train_mape_naive1 = mape(train_ts, train_fitted)
train_mae_naive1 = mae(train_ts, train_fitted)

eval_model_naive1 = c(train_rmse_naive1, train_mse_naive1, train_mape_naive1, train_mae_naive1)

data.frame(eval_metrics, eval_model_naive1)


# Train the custom naive model and forecast on the test data
h <- length(test_ts)
predict_naive1 <- naive_model1(train_ts, h)


# Calculate RMSE, MSE, MAPE, and MAE for forecast
predict_rmse_naive1 = rmse(test_ts, predict_naive1)
predict_mse_naive1 = mse(test_ts, predict_naive1)
predict_mape_naive1 = mape(test_ts, predict_naive1)
predict_mae_naive1 = mae(test_ts, predict_naive1)

eval_predict_naive1 = c(predict_rmse_naive1, predict_mse_naive1, predict_mape_naive1, predict_mae_naive1)

data.frame(eval_metrics, eval_predict_naive1)

# Forecast 1 tahun
forecast_naive1 = tail(naive_model1(train_ts, length(test_ts)+1), 1)
forecast_naive1



########################## Naive Model 2 #############################################
# Function to perform geometric random walk forecast
naive_model2 <- function(train, h) {
  last_value <- train[length(train)]
  growth_rate <- train[length(train)] / train[length(train) - 1]
  forecast_values <- numeric(h)
  forecast_values[1] <- last_value * growth_rate
  for (i in 2:h) {
    forecast_values[i] <- forecast_values[i - 1] * growth_rate
  }
  return(forecast_values)
}

# Define the window size for the geometric random walk (last two observations)
window_size <- 2

# Calculate metrics for the train set
# Using the last 'window_size' observations to forecast the rest of the training set
train_fitted2 <- numeric(length(train_ts) - window_size)
for (i in (window_size + 1):length(train_ts)) {
  growth_rate <- train_ts[i - 1] / train_ts[i - window_size]
  train_fitted2[i - window_size] <- train_ts[i - 1] * growth_rate
}

# Align train_ts and train_fitted2 for error metric calculations
aligned_train_ts <- train_ts[(window_size + 1):length(train_ts)]

# Calculate RMSE, MSE, MAPE, and MAE for the training data
train_rmse_naive2 = rmse(aligned_train_ts, train_fitted2)
train_mse_naive2 = mse(aligned_train_ts, train_fitted2)
train_mape_naive2 = mape(aligned_train_ts, train_fitted2)
train_mae_naive2 = mae(aligned_train_ts, train_fitted2)

eval_model_naive2 = c(train_rmse_naive2, train_mse_naive2, train_mape_naive2, train_mae_naive2)

data.frame(eval_metrics, eval_model_naive2)


# Train the custom naive model and forecast on the test data
h <- length(test_ts)
predict_naive2 <- naive_model2(train_ts, h)


# Calculate RMSE, MSE, MAPE, and MAE for forecast
predict_rmse_naive2 = rmse(test_ts, predict_naive2)
predict_mse_naive2 = mse(test_ts, predict_naive2)
predict_mape_naive2 = mape(test_ts, predict_naive2)
predict_mae_naive2 = mae(test_ts, predict_naive2)

eval_predict_naive2 = c(predict_rmse_naive2, predict_mse_naive2, predict_mape_naive2, predict_mae_naive2)

data.frame(eval_metrics, eval_predict_naive2)

forecast_naive2 = tail(naive_model2(train_ts, length(test_ts)+1), 1)
forecast_naive2

################################### Double Moving average ##################################
DMA <- function(data, n, f){
  data_sma = SMA(data, n)
  dma = SMA(data_sma, n)
  At = 2*data_sma - dma
  Bt = 2/(n-1)*(data_sma - dma)
  data_dma = At + Bt
  data_predict = c(NA, data_dma)
  t = 1:12
  f = c()
  for (i in t) {
    f[i] = At[length(At)] + Bt[length(Bt)] * i
  }
  data_res <- data.frame(actual = c(data, rep(NA, 12)), 
                         SMA = c(data_sma, rep(NA, 12)), 
                         DMA = c(dma, rep(NA, 12)), 
                         At = c(At, rep(NA, 12)), 
                         Bt = c(Bt, rep(NA, 12)), 
                         prediction = c(data_predict,f[-1]))
  return(data_res)
}

# Example usage:
# Assuming 'training' contains your training data
# Replace 'training' with your actual training data

dma4 <- DMA(train_ts, 3)
dma4 <- DMA(train_ts, 4)
dma5 <- DMA(train_ts, 5)
dma6 <- DMA(train_ts, 6)

forecast_dma3 = dma3[length(datats)+1,6]
forecast_dma3
forecast_dma4 = dma4[length(datats)+1,6]
forecast_dma4
forecast_dma5 = dma5[length(datats)+1,6]
forecast_dma5
forecast_dma6 = dma6[length(datats)+1,6]
forecast_dma6



calc_metrics <- function(train_data, dma_fun, n,h) {
  jum_na = h*2
  fit_dma = dma_fun[jum_na:n, 6]
  rmse = rmse(train_data[-c(1:jum_na-1)], fit_dma)
  mse = mse(train_data[-c(1:jum_na-1)], fit_dma)
  mape = mape(train_data[-c(1:jum_na-1)], fit_dma)
  mae = mae(train_data[-c(1:jum_na-1)], fit_dma)
  return(c(rmse, mse, mape, mae))
}

eval_model_dma3 <- calc_metrics(train_ts, dma3, train,3)
eval_model_dma4 <- calc_metrics(train_ts, dma4, train,4)
eval_model_dma5 <- calc_metrics(train_ts, dma5, train,5)
eval_model_dma6 <- calc_metrics(train_ts, dma6, train,6)

model_results_3 <- data.frame(eval_metrics, eval_model_dma3)
model_results_4 <- data.frame(eval_metrics, eval_model_dma4)
model_results_5 <- data.frame(eval_metrics, eval_model_dma5)
model_results_6 <- data.frame(eval_metrics, eval_model_dma6)

print(model_results_3)
print(model_results_4)
print(model_results_5)
print(model_results_6)






# Calculate error metrics for the test data
calc_test_metrics <- function(test_ts, dma_forecast) {
  rmse_test <- rmse(test_ts, dma_forecast)
  mse_test <- mse(test_ts, dma_forecast)
  mape_test <- mape(test_ts, dma_forecast)
  mae_test <- mae(test_ts, dma_forecast)
  
  return(c(rmse_test, mse_test, mape_test, mae_test))
}
dma4[43:54 , 6]
# Calculate test metrics for each DMA model
eval_predict_dma4 <- calc_test_metrics(test_ts, dma4[44:54,6])
eval_predict_dma5 <- calc_test_metrics(test_ts, dma5[44:54,6])
eval_predict_dma6 <- calc_test_metrics(test_ts, dma6[44:54,6])

# Create data frames to display the evaluation metrics for the test set
predict_results_4 <- data.frame(eval_metrics, eval_predict_dma4)
predict_results_5 <- data.frame(eval_metrics, eval_predict_dma5)
predict_results_6 <- data.frame(eval_metrics, eval_predict_dma6)

# Print the test results for each DMA model
print(predict_results_4)
print(predict_results_5)
print(predict_results_6)



########################## Exponential Smoothing ################################### 
########################## AAN #########################################
# Train the ETS model on the training set
model_des_AAN <- ets(train_ts, model = "AAN")
summary(model_des_AAN)

fit_des_AAN = model_des_AAN$fitted

# Calculate RMSE, MSE, MAPE, and MAE for the training data
train_rmse_des_AAN = rmse(train_ts, fit_des_AAN)
train_mse_des_AAN = mse(train_ts, fit_des_AAN)
train_mape_des_AAN = mape(train_ts, fit_des_AAN)
train_mae_des_AAN = mae(train_ts, fit_des_AAN)

eval_model_des_AAN = c(train_rmse_des_AAN, train_mse_des_AAN, train_mape_des_AAN, train_mape_des_AAN)

data.frame(eval_metrics, eval_model_des_AAN)

# Forecast using the trained ETS model
predict_des_AAN <- forecast(model_des_AAN, h=length(test_ts))

# Print the forecasted values
print(predict_des_AAN)

predict_des_AAN = predict_des_AAN$mean

# Calculate RMSE, MSE, MAPE, and MAE for forecast
predict_rmse_des_AAN = rmse(test_ts, predict_des_AAN)
predict_mse_des_AAN = mse(test_ts, predict_des_AAN)
predict_mape_des_AAN = mape(test_ts, predict_des_AAN)
predict_mae_des_AAN = mae(test_ts, predict_des_AAN)

eval_predict_des_AAN = c(predict_rmse_des_AAN, predict_mse_des_AAN, predict_mape_des_AAN, predict_mae_des_AAN)

data.frame(eval_metrics, eval_predict_des_AAN)

forecast_des_AAN = forecast(model_des_AAN, h = length(test_ts)+1)$mean[length(test_ts)+1]
forecast_des_AAN

########################## MAN #########################################
# Train the ETS model on the training set
model_des_MAN <- ets(train_ts, model = "MAN")
summary(model_des_MAN)

fit_des_MAN = model_des_MAN$fitted

# Calculate RMSE, MSE, MAPE, and MAE for the training data
train_rmse_des_MAN = rmse(train_ts, fit_des_MAN)
train_mse_des_MAN = mse(train_ts, fit_des_MAN)
train_mape_des_MAN = mape(train_ts, fit_des_MAN)
train_mae_des_MAN = mae(train_ts, fit_des_MAN)

eval_model_des_MAN = c(train_rmse_des_MAN, train_mse_des_MAN, train_mape_des_MAN, train_mape_des_MAN)

data.frame(eval_metrics, eval_model_des_MAN)

# Forecast using the trained ETS model
forecast_des_MAN <- forecast(model_des_MAN, h=length(test_ts))

# Print the forecasted values
print(forecast_des_MAN)

predict_des_MAN = forecast_des_MAN$mean

# Calculate RMSE, MSE, MAPE, and MAE for forecast
predict_rmse_des_MAN = rmse(test_ts, predict_des_MAN)
predict_mse_des_MAN = mse(test_ts, predict_des_MAN)
predict_mape_des_MAN = mape(test_ts, predict_des_MAN)
predict_mae_des_MAN = mae(test_ts, predict_des_MAN)

eval_predict_des_MAN = c(predict_rmse_des_MAN, predict_mse_des_MAN, predict_mape_des_MAN, predict_mae_des_MAN)

data.frame(eval_metrics, eval_predict_des_MAN)

forecast_des_MAN = forecast(model_des_MAN, h = length(test_ts)+1)$mean[length(test_ts)+1]
forecast_des_MAN

################## MMN ################
# Train the ETS model on the training set
model_des_MMN <- ets(train_ts, model = "MMN")
summary(model_des_MMN)

fit_des_MMN = model_des_MMN$fitted

# Calculate RMSE, MSE, MAPE, and MAE for the training data
train_rmse_des_MMN = rmse(train_ts, fit_des_MMN)
train_mse_des_MMN = mse(train_ts, fit_des_MMN)
train_mape_des_MMN = mape(train_ts, fit_des_MMN)
train_mae_des_MMN = mae(train_ts, fit_des_MMN)

eval_model_des_MMN = c(train_rmse_des_MMN, train_mse_des_MMN, train_mape_des_MMN, train_mape_des_MMN)

data.frame(eval_metrics, eval_model_des_MMN)

# Forecast using the trained ETS model
forecast_des_MMN <- forecast(model_des_MMN, h=length(test_ts))

# Print the forecasted values
print(forecast_des_MMN)

predict_des_MMN = forecast_des_MMN$mean

# Calculate RMSE, MSE, MAPE, and MAE for forecast
predict_rmse_des_MMN = rmse(test_ts, predict_des_MMN)
predict_mse_des_MMN = mse(test_ts, predict_des_MMN)
predict_mape_des_MMN = mape(test_ts, predict_des_MMN)
predict_mae_des_MMN = mae(test_ts, predict_des_MMN)

eval_predict_des_MMN = c(predict_rmse_des_MMN, predict_mse_des_MMN, predict_mape_des_MMN, predict_mae_des_MMN)

data.frame(eval_metrics, eval_predict_des_MMN)

forecast_des_MMN = forecast(model_des_MMN, h = length(test_ts)+1)$mean[length(test_ts)+1]
forecast_des_MMN


########################## Regression ###################################  -> error
data
len_data = length(data$Skor)
len_data

train = floor(len_data*0.8)
data_train = data[1:train,]
head(data_train)

data_test = data[(train+1): len_data,]
head(data_test)

data_train$t = seq(1:train)
head(data_train)
data_train

data_test$t = seq((train+1),(train+length(test_ts)))
data_test

######### model 1 ########
model_regresi_1 = lm(Skor ~ t, data = data_train)
model_regresi_1
summary(model_regresi_1)

dwtest(model_regresi_1, alternative = "two.sided")

bptest(model_regresi_1)

lillie.test(model_regresi_1$residuals)

fitted_regresi_1 = model_regresi_1$fitted.values

# Calculate RMSE, MSE, MAPE, and MAE for the training data
model_rmse_regresi_1 = rmse(train_ts, fitted_regresi_1)
model_mse_regresi_1 = mse(train_ts, fitted_regresi_1)
model_mape_regresi_1 = mape(train_ts, fitted_regresi_1)
model_mae_regresi_1 = mae(train_ts, fitted_regresi_1)

eval_model_regresi_1 = c(model_rmse_regresi_1, model_mse_regresi_1, model_mape_regresi_1, model_mae_regresi_1)

data.frame(eval_metrics, eval_model_regresi_1)

### Predict ####
predict_regresi_1 = predict(model_regresi_1, newdata = data_test)

# Calculate RMSE, MSE, MAPE, and MAE for the training data
predict_rmse_regresi_1 = rmse(test_ts, predict_regresi_1)
predict_mse_regresi_1 = mse(test_ts, predict_regresi_1)
predict_mape_regresi_1 = mape(test_ts, predict_regresi_1)
predict_mae_regresi_1 = mae(test_ts, predict_regresi_1)

eval_predict_regresi_1 = c(predict_rmse_regresi_1, predict_mse_regresi_1, model_mape_regresi_1, model_mae_regresi_1)

data.frame(eval_metrics, eval_predict_regresi_1)

forecast_regresi_1 = tail(predict_regresi_1, 2)
forecast_regresi_1


####### Model 2 Exponential trend #########
data_train$lnY = log(data_train$Skor)
data_train

data_test$lnY = log(data_test$Skor)
data_test

model_regresi_2 = lm(lnY ~ t, data = data_train)
summary(model_regresi_2)

dwtest(model_regresi_2, alternative = "two.sided")

bptest(model_regresi_2)

lillie.test(model_regresi_2$residuals)

fitted_regresi_2 = model_regresi_2$fitted.values

# Calculate RMSE, MSE, MAPE, and MAE for the training data
model_rmse_regresi_2 = rmse(train_ts, fitted_regresi_2)
model_mse_regresi_2 = mse(train_ts, fitted_regresi_2)
model_mape_regresi_2 = mape(train_ts, fitted_regresi_2)
model_mae_regresi_2 = mae(train_ts, fitted_regresi_2)

eval_model_regresi_2 = c(model_rmse_regresi_2, model_mse_regresi_2, model_mape_regresi_2, model_mae_regresi_2)

data.frame(eval_metrics, eval_model_regresi_2)

### Predict ####
predict_regresi_2 = predict(model_regresi_2, newdata = data_test)

# Calculate RMSE, MSE, MAPE, and MAE for the training data
predict_rmse_regresi_2 = rmse(test_ts, predict_regresi_2)
predict_mse_regresi_2 = mse(test_ts, predict_regresi_2)
predict_mape_regresi_2 = mape(test_ts, predict_regresi_2)
predict_mae_regresi_2 = mae(test_ts, predict_regresi_2)

eval_predict_regresi_2 = c(predict_rmse_regresi_2, predict_mse_regresi_2, model_mape_regresi_2, model_mae_regresi_2)

data.frame(eval_metrics, eval_predict_regresi_2)

forecast_regresi_2 = tail(predict_regresi_2, 1)
forecast_regresi_2

####### Model 3 Quadratic trend #########
data_train$t2 = (data_train$Skor)^2
data_train

data_test$t2 = (data_test$Skor)^2
data_test

model_regresi_3 = lm(Skor ~ t +t2, data = data_train)
summary(model_regresi_3)

dwtest(model_regresi_3, alternative = "two.sided")

bptest(model_regresi_3)

lillie.test(model_regresi_3$residuals)

fitted_regresi_3 = model_regresi_3$fitted.values

# Calculate RMSE, MSE, MAPE, and MAE for the training data
model_rmse_regresi_3 = rmse(train_ts, fitted_regresi_3)
model_mse_regresi_3 = mse(train_ts, fitted_regresi_3)
model_mape_regresi_3 = mape(train_ts, fitted_regresi_3)
model_mae_regresi_3 = mae(train_ts, fitted_regresi_3)

eval_model_regresi_3 = c(model_rmse_regresi_3, model_mse_regresi_3, model_mape_regresi_3, model_mae_regresi_3)

data.frame(eval_metrics, eval_model_regresi_3)

### Predict ####
predict_regresi_3 = predict(model_regresi_3, newdata = data_test)

# Calculate RMSE, MSE, MAPE, and MAE for the training data
predict_rmse_regresi_3 = rmse(test_ts, predict_regresi_3)
predict_mse_regresi_3 = mse(test_ts, predict_regresi_3)
predict_mape_regresi_3 = mape(test_ts, predict_regresi_3)
predict_mae_regresi_3 = mae(test_ts, predict_regresi_3)

eval_predict_regresi_3 = c(predict_rmse_regresi_3, predict_mse_regresi_3, model_mape_regresi_3, model_mae_regresi_3)

data.frame(eval_metrics, eval_predict_regresi_3)

forecast_regresi_3 = tail(predict_regresi_3, 1)
forecast_regresi_3

######## Model 4 (lag 1) ##########
yt = data_train$Skor[2:train]
t = data_train$t[2:train]
yt_lag1 = data_train$Skor[1:(train-1)]

data_regresi_4 = data.frame(yt, t, yt_lag1)

model_regresi_4 = lm(yt ~ t + yt_lag1, data = data_regresi_4)
summary(model_regresi_4)

dwtest(model_regresi_4, alternative = "two.sided")

bptest(model_regresi_4)

lillie.test(model_regresi_4$residuals)

###### Model 5 (lag 2)
yt5 = data_train$Skor[3:train]
t5 = data_train$t[3:train]
yt5_lag1 = data_train$Skor[2:(train-1)]
yt5_lag2 = data_train$Skor[1:(train-2)]

data_regresi_5 = data.frame(yt5, t5, yt5_lag1, yt5_lag2)

model_regresi_5 = lm(yt5 ~ t5 + yt5_lag1 + yt5_lag2, data = data_regresi_5)
summary(model_regresi_5)

################################## ARIMA #####################################
# Cek Stationary 
# terhadap variance
powerTransform(train_ts)
summary(powerTransform(train_ts))

# H0 : lambda = 1 (stationer terhadap varians)
# H1 : lambda =/ 1 (tidak stationer terhadap lambda)
# Jika H0 ditolak, maka data harus ditransformasi berdasarkan nilai lambda
# Didapat bahwa p-value = 0.05189 artinya >0.05 maka gagal tolak H0 
# atau data sudah stationer terhadap varians

# terhadap mean
adf.test(train_ts)
# H0 : data tidak stationer terhadap mean
# H1 : data stationer terhadap mean
# Didapat bahwa p-value = 0.01 artinya < 0.05 maka gagal tolak H0 
# atau data tidak stationer terhadap mean

difference = diff(train_ts)
adf.test(difference)

difference2 = diff(difference)
adf.test(difference2)
# 0.01

difference3 = diff(difference2)
adf.test(difference3)

# Order
acf(difference3, lag.max = 20) 

pacf(difference3, lag.max = 20) 


# p = 1, 2, 3, 4
# d = 3
# q = 1

####### Model 1 (1, 3, 1) ######
model_arima_1 = arima(train_ts, order = c(1, 3, 1))
model_arima_1
coeftest(model_arima_1)
# signifikan

####### Model 2 (2, 3, 1) ######
model_arima_2 = arima(train_ts, order = c(2, 3, 1))
model_arima_2
coeftest(model_arima_2)
# AR(2) tidak signifikan

####### Model 3 (3, 3, 1) ######
model_arima_3 = arima(train_ts, order = c(3, 3, 1))
model_arima_3
coeftest(model_arima_3)
# signifikan

####### Model 4 (4, 3, 1) ######
model_arima_4 = arima(train_ts, order = c(4, 3, 1))
model_arima_4
coeftest(model_arima_4)
# AR(4) tidak signifikan

###### Uji Asumsi White Noise  #########
#H0: residual white noise
#H1: residual tidak white noise

#### Model 1 ####
Box.test(model_arima_1$residuals, type = "Ljung-Box")
# Karena > 0.05 maka gagal tolak H0, ada white noise

#### Model 3 ####
Box.test(model_arima_3$residuals, type = "Ljung-Box")
# Karena > 0.05 maka gagal tolak H0, ada white noise


#Asumsi dist. normal
lillie.test(model_arima_1$residuals) 
# H0 : Error berdistribusi normal
# H1 : Error tidak berdistribusi normal
# p-value < 2.2e-16
# Karena p-value < 0.05 maka tolak H0 yang artinya error tidak berdistribusi normal

lillie.test(model_arima_3$residuals)
# H0 : Error berdistribusi normal
# H1 : Error tidak berdistribusi normal
# p-value = 1.892e-06
# Karena p-value < 0.05 maka tolak H0 yang artinya error tidak berdistribusi normal



######### Model Evaluation #########

#### Model 1 #####
res_arima_1 = exp(model_arima_1$residuals)

rmse_model_arima_1 = sqrt(mean(res_arima_1^2))
mse_model_arima_1 = mean(res_arima_1^2)
mape_model_arima_1 = mean(abs(res_arima_1/train_ts)) * 100
mae_model_arima_1 = mean(abs(res_arima_1))

eval_model_arima_1 = c(rmse_model_arima_1, mse_model_arima_1, mape_model_arima_1, mae_model_arima_1)

data.frame(eval_metrics, eval_model_arima_1)

##### Model 3 ######
res_arima_3 = exp(model_arima_3$residuals)

rmse_model_arima_3 = sqrt(mean(res_arima_3^2))
mse_model_arima_3 = mean(res_arima_3^2)
mape_model_arima_3 = mean(abs(res_arima_3/train_ts)) * 100
mae_model_arima_3 = mean(abs(res_arima_3))


eval_model_arima_3 = c(rmse_model_arima_3, mse_model_arima_3, mape_model_arima_3, mae_model_arima_3)

data.frame(eval_metrics, eval_model_arima_3)



######### Prediction Model 1 #############
predict_arima_1 = predict(model_arima_1, n.ahead = length(test_ts))
predict_arima_1

predict_values_arima_1 = exp(predict_arima_1$pred)
predict_values_arima_1

rmse_arima_1 = rmse(test_ts, predict_values_arima_1)
mse_arima_1 = mse(test_ts, predict_values_arima_1)
mape_arima_1 = mape(test_ts, predict_values_arima_1)
mae_arima_1 = mae(test_ts, predict_values_arima_1)

eval_predict_arima_1 = c(rmse_arima_1, mse_arima_1, mape_arima_1, mae_arima_1)

data.frame(eval_metrics, eval_predict_arima_1)


######### Prediction Model 3 #############
predict_arima_3 = predict(model_arima_3, n.ahead = length(test_ts))
predict_arima_3

predict_values_arima_3 = exp(predict_arima_3$pred)
predict_values_arima_3

rmse_arima_3 = rmse(test_ts, predict_values_arima_3)
mse_arima_3 = mse(test_ts, predict_values_arima_3)
mape_arima_3 = mape(test_ts, predict_values_arima_3)
mae_arima_3 = mae(test_ts, predict_values_arima_3)

eval_predict_arima_3 = c(rmse_arima_3, mse_arima_3, mape_arima_3, mae_arima_3)

data.frame(eval_metrics, eval_predict_arima_3)


#### Forecast 2 tahun ####
forecast_arima_1 = exp(predict(model_arima_1, n.ahead = length(test_ts)+1)$pred[length(test_ts)+1])
forecast_arima_1

forecast_arima_3 = exp(predict(model_arima_3, n.ahead = length(test_ts)+1)$pred[length(test_ts)+1])
forecast_arima_3



########### Perbandingan model ###############
######### Train ###################
data.frame(eval_metrics, eval_model_naive1, eval_model_naive2, eval_model_dma4, eval_model_dma5, eval_model_dma6)
data.frame(eval_metrics, eval_model_des_AAN, eval_model_des_MAN, eval_model_des_MMN)


######### predict ###################
data.frame(eval_metrics, eval_predict_naive1, eval_predict_naive2, eval_predict_dma4, eval_predict_dma5, eval_predict_dma6)
data.frame(eval_metrics, eval_predict_des_AAN, eval_predict_des_MAN, eval_predict_des_MMN)

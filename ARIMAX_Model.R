####################ARIMAX MODEL########################  
library(forecast)
library(ggplot2)
#-------------1)BPCL.NS Data-------------------------------------------------

  

data = read.csv('BPCL.NS_updated.csv')
#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
train_data = na.omit(train_data)
test_data = data[(div+1):nrow(data),]

# Extract the names of selected covariates
model_stepwise <- lm(Close ~ ., data = data[,-1])
stepwise_model <- step(model_stepwise)
selected_covariate_names <- names(coef(stepwise_model))[-1]  # Exclude intercept
# Get the column numbers of selected covariates
selected_covariate_columns <- which(names(data) %in% selected_covariate_names)




# Fit an ARIMAX model with exogenous variables
arimax_model <- arima(train_data$Close, order = c(0, 1, 0), xreg = train_data[, selected_covariate_columns])
# Extract exogenous variables from the test data
test_covariates <- test_data[, selected_covariate_columns]
arimax_model$xreg  = test_covariates
# Make the forecast explicitly providing exogenous variables
y.fit <- forecast(arimax_model, xreg = as.matrix(test_covariates))$mean
head(y.fit)

#plots
ggplot() +
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6) +
  geom_line(data = data, aes(x = 1 : nrow(data), y = Close, col = 'Original')) +
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'fitted'), size = 0.75) +
  labs(x = 'Date',
       y = 'Closing Price',
       color = 'Index') +
  scale_x_continuous(breaks = seq(1, nrow(data), 500), labels = data$Date[seq(1, nrow(data), 500)]) +
  theme(axis.text.x = element_text(angle = 10)) +
  ggtitle("ARIMAX Model Fitted vs. Original Closing Prices")

#RMSE
library(Metrics)
rmse(test_data$Close, y.fit)


#-------------2)CIPLA Data-------------------------------------------------



data = read.csv('CIPLA.NS_updated.csv')
#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
train_data = na.omit(train_data)
test_data = data[(div+1):nrow(data),]

# Extract the names of selected covariates
model_stepwise <- lm(Close ~ ., data = data[,-1])
stepwise_model <- step(model_stepwise)
selected_covariate_names <- names(coef(stepwise_model))[-1]  # Exclude intercept
# Get the column numbers of selected covariates
selected_covariate_columns <- which(names(data) %in% selected_covariate_names)




# Fit an ARIMAX model with exogenous variables
arimax_model <- arima(train_data$Close, order = c(0, 1, 0), xreg = train_data[, selected_covariate_columns])
# Extract exogenous variables from the test data
test_covariates <- test_data[, selected_covariate_columns]
arimax_model$xreg  = test_covariates
# Make the forecast explicitly providing exogenous variables
y.fit <- forecast(arimax_model, xreg = as.matrix(test_covariates))$mean
head(y.fit)

#plots
ggplot() +
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6) +
  geom_line(data = data, aes(x = 1 : nrow(data), y = Close, col = 'Original')) +
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'fitted'), size = 0.75) +
  labs(x = 'Date',
       y = 'Closing Price',
       color = 'Index') +
  scale_x_continuous(breaks = seq(1, nrow(data), 500), labels = data$Date[seq(1, nrow(data), 500)]) +
  theme(axis.text.x = element_text(angle = 10)) +
  ggtitle("ARIMAX Model Fitted vs. Original Closing Prices")

#RMSE
library(Metrics)
rmse(test_data$Close, y.fit)



#-------------3)COROMANDEL Data-------------------------------------------------



data = read.csv('COROMANDEL.NS_updated.csv')
#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
train_data = na.omit(train_data)
test_data = data[(div+1):nrow(data),]

# Extract the names of selected covariates
model_stepwise <- lm(Close ~ ., data = data[,-1])
stepwise_model <- step(model_stepwise)
selected_covariate_names <- names(coef(stepwise_model))[-1]  # Exclude intercept
# Get the column numbers of selected covariates
selected_covariate_columns <- which(names(data) %in% selected_covariate_names)




# Fit an ARIMAX model with exogenous variables
arimax_model <- arima(train_data$Close, order = c(0, 1, 0), xreg = train_data[, selected_covariate_columns])
test_covariates <- test_data[, selected_covariate_columns]
arimax_model$xreg  = test_covariates
# Make the forecast explicitly providing exogenous variables
y.fit <- forecast(arimax_model, xreg = as.matrix(test_covariates))$mean
head(y.fit)

#plots
ggplot() +
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6) +
  geom_line(data = data, aes(x = 1 : nrow(data), y = Close, col = 'Original')) +
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'fitted'), size = 0.75) +
  labs(x = 'Date',
       y = 'Closing Price',
       color = 'Index') +
  scale_x_continuous(breaks = seq(1, nrow(data), 500), labels = data$Date[seq(1, nrow(data), 500)]) +
  theme(axis.text.x = element_text(angle = 10)) +
  ggtitle("ARIMAX Model Fitted vs. Original Closing Prices")

#RMSE
library(Metrics)
rmse(test_data$Close, y.fit)


#-------------4)HDFCBANK Data-------------------------------------------------



data = read.csv('HDFCBANK.NS_updated.csv')
#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
train_data = na.omit(train_data)
test_data = data[(div+1):nrow(data),]

# Extract the names of selected covariates
model_stepwise <- lm(Close ~ ., data = data[,-1])
stepwise_model <- step(model_stepwise)
selected_covariate_names <- names(coef(stepwise_model))[-1]  # Exclude intercept
# Get the column numbers of selected covariates
selected_covariate_columns <- which(names(data) %in% selected_covariate_names)




# Fit an ARIMAX model with exogenous variables
arimax_model <- arima(train_data$Close, order = c(0, 1, 0), xreg = train_data[, selected_covariate_columns])
test_covariates <- test_data[, selected_covariate_columns]
arimax_model$xreg  = test_covariates
# Make the forecast explicitly providing exogenous variables
y.fit <- forecast(arimax_model, xreg = as.matrix(test_covariates))$mean
head(y.fit)

#plots
ggplot() +
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6) +
  geom_line(data = data, aes(x = 1 : nrow(data), y = Close, col = 'Original')) +
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'fitted'), size = 0.75) +
  labs(x = 'Date',
       y = 'Closing Price',
       color = 'Index') +
  scale_x_continuous(breaks = seq(1, nrow(data), 500), labels = data$Date[seq(1, nrow(data), 500)]) +
  theme(axis.text.x = element_text(angle = 10)) +
  ggtitle("ARIMAX Model Fitted vs. Original Closing Prices")

#RMSE
library(Metrics)
rmse(test_data$Close, y.fit)


#-------------5)TCS Data-------------------------------------------------



data = read.csv('TCS.NS_updated.csv')
#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
train_data = na.omit(train_data)
test_data = data[(div+1):nrow(data),]

# Extract the names of selected covariates
model_stepwise <- lm(Close ~ ., data = data[,-1])
stepwise_model <- step(model_stepwise)
selected_covariate_names <- names(coef(stepwise_model))[-1]  # Exclude intercept
# Get the column numbers of selected covariates
selected_covariate_columns <- which(names(data) %in% selected_covariate_names)




# Fit an ARIMAX model with exogenous variables
arimax_model <- arima(train_data$Close, order = c(0, 1, 0), xreg = train_data[, selected_covariate_columns])
# Extract exogenous variables from the test data
test_covariates <- test_data[, selected_covariate_columns]
arimax_model$xreg  = test_covariates
# Make the forecast explicitly providing exogenous variables
y.fit <- forecast(arimax_model, xreg = as.matrix(test_covariates))$mean
head(y.fit)

#plots
ggplot() +
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6) +
  geom_line(data = data, aes(x = 1 : nrow(data), y = Close, col = 'Original')) +
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'fitted'), size = 0.75) +
  labs(x = 'Date',
       y = 'Closing Price',
       color = 'Index') +
  scale_x_continuous(breaks = seq(1, nrow(data), 500), labels = data$Date[seq(1, nrow(data), 500)]) +
  theme(axis.text.x = element_text(angle = 10)) +
  ggtitle("ARIMAX Model Fitted vs. Original Closing Prices")

#RMSE
library(Metrics)
rmse(test_data$Close, y.fit)


  

  
  
  
  
  
  

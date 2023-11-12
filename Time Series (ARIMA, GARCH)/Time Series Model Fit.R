library(forecast)
library(ggplot2)
library(tseries)
library(Metrics)
library(tseries)
library(rugarch)
library(FinTS)


#1. BPCL
#=======
#data
data = read.csv('BPCL.NS.csv')


#plotting the data against time
ggplot(data)+
  geom_line(aes(x = 1:nrow(data), y = Open))

#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
test_data = data[(div+1):nrow(data),]



#checking stationarity of the data
adf.test(train_data$Open)

#taking first order difference
y = diff(train_data$Open)

#again checking stationarity of the data
adf.test(y)

#plotting the stationary data
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#finding the order of AR & MA using the acf and pacf
acf(y)
pacf(y)

#checking order with auto.arima function
auto.arima(train_data$Open)

#fitting the arima model with p,d,q
model_train = arima(train_data$Open, order = c(0,1,0))
y.fit = forecast(model_train, length(test_data$Open))[[4]]

#RMSE
rmse(test_data$Open, y.fit)

#plots
ggplot()+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  geom_line(data = data, aes(x = 1 : nrow(data), y = Open, col = 'Original'))+
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'Fitted'), size = 0.75)+
  labs(x = 'Date',
       y = 'Opening Price',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')
  
#----------------------------------------------

#ARCH-GARCH

#checking volatility clustering
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#checking whether to fit ARCH model or not
ArchTest(y)

y1 = diff(data$Open)

#determining the order
garch(x = y, grad = 'numerical', trace = FALSE)

#fitting GARCH model
y_garch = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,0)))

y_garch_fit = ugarchfit(y_garch, data = y)

y_garch_fit


#forecasting
y2 = diff(test_data$Open)
y_forecast = (ugarchforecast(y_garch_fit, n.ahead = length(y2))@forecast)$series


#plot(y_garch_fit)

ggplot()+
  geom_line(aes(x = 1 : length(y1), y = y1, col = 'Original'))+
  geom_line(aes(x = (div+1):(nrow(data)-1), y = y_forecast, col = 'Fitted'), size = 0.75)+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  labs(x = 'Date',
       y = 'Opening Price (stationary)',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')
  
  


rmse(y2,y_forecast)
#====================================================================================

#2. CIPLA
#=========
#data
data = read.csv('CIPLA.NS.csv')


#plotting the data against time
ggplot(data)+
  geom_line(aes(x = 1:nrow(data), y = Open))

#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
test_data = data[(div+1):nrow(data),]



#checking stationarity of the data
adf.test(train_data$Open)

#taking first order difference
y = diff(train_data$Open)

#again checking stationarity of the data
adf.test(y)

#plotting the stationary data
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#finding the order of AR & MA using the acf and pacf
acf(y)
pacf(y)

#checking order with auto.arima function
auto.arima(train_data$Open)

#fitting the arima model with p,d,q
model_train = arima(train_data$Open, c(0,1,1))
y.fit = forecast(model_train, length(test_data$Open))[[4]]

#RMSE
rmse(test_data$Open, y.fit)

#plots
ggplot()+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  geom_line(data = data, aes(x = 1 : nrow(data), y = Open, col = 'Original'))+
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'Fitted'), size = 0.75)+
  labs(x = 'Date',
       y = 'Opening Price',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')
#-----------------------------------------------------------------------------
#ARCH-GARCH

#checking volatility clustering
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#checking whether to fit ARCH model or not
ArchTest(y)

y1 = diff(data$Open)

#determining the order
garch(x = y, grad = 'numerical', trace = FALSE)

#fitting GARCH model
y_garch = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,1)))

y_garch_fit = ugarchfit(y_garch, data = y)

y_garch_fit


#forecasting
y2 = diff(test_data$Open)
y_forecast = (ugarchforecast(y_garch_fit, n.ahead = length(y2))@forecast)$series


#plot(y_garch_fit)

ggplot()+
  geom_line(aes(x = 1 : length(y1), y = y1, col = 'Original'))+
  geom_line(aes(x = (div+1):(nrow(data)-1), y = y_forecast, col = 'Fitted'), size = 0.75)+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  labs(x = 'Date',
       y = 'Opening Price (stationary)',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')





rmse(y2,y_forecast)
#====================================================================================

#3. COROMANDEL
#==============


#data
data = read.csv('COROMANDEL.NS.csv')


#plotting the data against time
ggplot(data)+
  geom_line(aes(x = 1:nrow(data), y = Open))

#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
test_data = data[(div+1):nrow(data),]



#checking stationarity of the data
adf.test(train_data$Open)

#taking first order difference
y = diff(train_data$Open)

#again checking stationarity of the data
adf.test(y)

#plotting the stationary data
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#finding the order of AR & MA using the acf and pacf
acf(y)
pacf(y)

#checking order with auto.arima function
auto.arima(train_data$Open)

#fitting the arima model with p,d,q
model_train = arima(train_data$Open, c(0,1,0))
y.fit = forecast(model_train, length(test_data$Open))[[4]]

#RMSE
rmse(test_data$Open, y.fit)

#plots
ggplot()+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  geom_line(data = data, aes(x = 1 : nrow(data), y = Open, col = 'Original'))+
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'Fitted'), size = 0.75)+
  labs(x = 'Date',
       y = 'OPening Price',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')

#------------------------------------------------------------------------------------
#ARCH-GARCH

#checking volatility clustering
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#checking whether to fit ARCH model or not
ArchTest(y)

y1 = diff(data$Open)

#determining the order
garch(x = y, grad = 'numerical', trace = FALSE)

#fitting GARCH model
y_garch = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,0)))

y_garch_fit = ugarchfit(y_garch, data = y)

y_garch_fit


#forecasting
y2 = diff(test_data$Open)
y_forecast = (ugarchforecast(y_garch_fit, n.ahead = length(y2))@forecast)$series


#plot(y_garch_fit)

ggplot()+
  geom_line(aes(x = 1 : length(y1), y = y1, col = 'Original'))+
  geom_line(aes(x = (div+1):(nrow(data)-1), y = y_forecast, col = 'Fitted'), size = 0.75)+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  labs(x = 'Date',
       y = 'Opening Price (stationary)',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')





rmse(y2,y_forecast)
#====================================================================================

#4 HDFC
#=======

#data
data = read.csv('HDFCBANK.NS.csv')


#plotting the data against time
ggplot(data)+
  geom_line(aes(x = 1:nrow(data), y = Open))

#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
test_data = data[(div+1):nrow(data),]



#checking stationarity of the data
adf.test(train_data$Open)

#taking first order difference
y = diff(train_data$Open)

#again checking stationarity of the data
adf.test(y)

#plotting the stationary data
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#finding the order of AR & MA using the acf and pacf
acf(y)
pacf(y)

#checking order with auto.arima function
auto.arima(train_data$Open)

#fitting the arima model with p,d,q
model_train = arima(train_data$Open, c(1,1,1))
y.fit = forecast(model_train, length(test_data$Open))[[4]]

#RMSE
rmse(test_data$Open, y.fit)

#plots
ggplot()+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  geom_line(data = data, aes(x = 1 : nrow(data), y = Open, col = 'Original'))+
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'Fitted'), size = 0.75)+
  labs(x = 'Date',
       y = 'Opening Price',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')
#-----------------------------------------------------------------------------------
#ARCH-GARCH

#checking volatility clustering
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#checking whether to fit ARCH model or not
ArchTest(y)

y1 = diff(data$Open)

#determining the order
garch(x = y, grad = 'numerical', trace = FALSE)

#fitting GARCH model
y_garch = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model=list(armaOrder=c(1,1)))

y_garch_fit = ugarchfit(y_garch, data = y)

y_garch_fit


#forecasting
y2 = diff(test_data$Open)
y_forecast = (ugarchforecast(y_garch_fit, n.ahead = length(y2))@forecast)$series


#plot(y_garch_fit)

ggplot()+
  geom_line(aes(x = 1 : length(y1), y = y1, col = 'Original'))+
  geom_line(aes(x = (div+1):(nrow(data)-1), y = y_forecast, col = 'Fitted'), size = 0.75)+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  labs(x = 'Date',
       y = 'Opening Price (stationary)',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')





rmse(y2,y_forecast)
#===================================================================================

#5. TCS
#========

#data
data = read.csv('TCS.NS.csv')


#plotting the data against time
ggplot(data)+
  geom_line(aes(x = 1:nrow(data), y = Open))

#dividing into train and test
div = (floor(nrow(data)))*0.8
train_data = data[1:div,]
test_data = data[(div+1):nrow(data),]



#checking stationarity of the data
adf.test(train_data$Open)

#taking first order difference
y = diff(train_data$Open)

#again checking stationarity of the data
adf.test(y)

#plotting the stationary data
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#finding the order of AR & MA using the acf and pacf
acf(y)
pacf(y)

#checking order with auto.arima function
auto.arima(train_data$Open)

#fitting the arima model with p,d,q
model_train = arima(train_data$Open, c(0,1,0))
y.fit = forecast(model_train, length(test_data$Open))[[4]]

#RMSE
rmse(test_data$Open, y.fit)

#plots
ggplot()+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  geom_line(data = data, aes(x = 1 : nrow(data), y = Open, col = 'Original'))+
  geom_line(data = test_data, aes(x = (div+1):nrow(data), y = y.fit, col = 'Fitted'), size = 0.75)+
  labs(x = 'Date',
       y = 'Opening Price',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')
#----------------------------------------------------------------------------------
#ARCH-GARCH

#checking volatility clustering
ggplot()+
  geom_line(aes(x = 1:length(y), y = y))

#checking whether to fit ARCH model or not
ArchTest(y)

y1 = diff(data$Open)

#determining the order
garch(x = y, grad = 'numerical', trace = FALSE)

#fitting GARCH model
y_garch = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model=list(armaOrder=c(0,0)))

y_garch_fit = ugarchfit(y_garch, data = y)

y_garch_fit


#forecasting
y2 = diff(test_data$Open)
y_forecast = (ugarchforecast(y_garch_fit, n.ahead = length(y2))@forecast)$series


#plot(y_garch_fit)

ggplot()+
  geom_line(aes(x = 1 : length(y1), y = y1, col = 'Original'))+
  geom_line(aes(x = (div+1):(nrow(data)-1), y = y_forecast, col = 'Fitted'), size = 0.75)+
  geom_vline(xintercept = div, linetype = 'dashed', col = 'navyblue', size = 0.6)+
  labs(x = 'Date',
       y = 'Opening Price (stationary)',
       color = 'Index')+
  scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
  theme(axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text( size = 12),
        legend.title = element_text( size = 15, hjust = 0.5),
        legend.position = 'bottom')




rmse(y2,y_forecast)

# -*- coding: utf-8 -*-
"""
Created on Tue Nov  7 11:29:58 2023

@author: ASUS
"""


import pandas as pd 
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
import xgboost as xgb
import numpy as np

def split(dat):
    def test_train_split(dat):
        split_date = pd.to_datetime('2021-01-21')
        test = dat[dat.index >= split_date]
        train = dat[dat.index < split_date]
        
        return train, test

    ## X & y split
    def X_y_split(dat):
        X = dat.drop(['Close'], axis = 1)
        y = dat[['Close']]
        
        return X, y

    X, y = X_y_split(dat)
      
    ## test train split
    X_train, X_test = test_train_split(X)
    y_train, y_test = test_train_split(y)
    
    return X_train, X_test, y_train, y_test


## Linear Regression
def LR(X_train, X_test, y_train, y_test):
    model = LinearRegression()
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    return rmse
    
## Random Forest Regressor
def RFR(X_train, X_test, y_train, y_test):
    model = RandomForestRegressor(n_estimators=100, random_state=42)
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    return rmse

def XGB(X_train, X_test, y_train, y_test):
    model = xgb.XGBRegressor(objective='reg:squarederror', random_state=42, booster='gbtree')
    model.fit(X_train, y_train)
    
    y_pred = model.predict(X_test)
    
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    return rmse

   
updated_df = data['BPCL.NS']
df_ML = split(updated_df)
X_train, X_test, y_train, y_test = df_ML[0], df_ML[1], df_ML[2], df_ML[3]


LR(df_ML[0], df_ML[1], df_ML[2], df_ML[3])
RFR(df_ML[0], df_ML[1], df_ML[2], df_ML[3])
XGB(df_ML[0], df_ML[1], df_ML[2], df_ML[3])

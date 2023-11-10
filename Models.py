# -*- coding: utf-8 -*-
"""
Created on Tue Nov  7 11:29:58 2023

@author: Ringan Majumdar
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
    return rmse, y_pred
    
## Random Forest Regressor
def RFR(X_train, X_test, y_train, y_test):
    model = RandomForestRegressor(n_estimators=100, random_state=42)
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    return rmse, y_pred

def XGB(X_train, X_test, y_train, y_test):
    model = xgb.XGBRegressor(objective='reg:squarederror', random_state=42, booster='gbtree')
    model.fit(X_train, y_train)
    
    y_pred = model.predict(X_test)
    
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    return rmse, y_pred


def merged_y(X_train, X_test, y_train, y_test):
    df = {'y_test' : np.asarray(y_test).flatten(), 
          'LR_test' : np.asarray(LR(X_train, X_test, y_train, y_test)[1].flatten()),
          'RFR_test' : np.asarray(RFR(X_train, X_test, y_train, y_test)[1]),
          'XGB_test' : np.asarray(XGB(X_train, X_test, y_train, y_test)[1])}
    df = pd.DataFrame(df)
    df = df.set_index(y_test.index)
    return df

stocks = ["COROMANDEL.NS", "CIPLA.NS", "TCS.NS", "HDFCBANK.NS", "BPCL.NS"]
for i in stocks:
    X_train, X_test, y_train, y_test = split(data[i])
    foo = merged_y(X_train, X_test, y_train, y_test)
    
    foo.to_csv(f'{i}_pred_LR_RFR_XGB.csv')
    
    

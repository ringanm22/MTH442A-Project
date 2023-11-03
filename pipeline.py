import pandas as pd
import numpy as np
import ta
import talib
from datetime import datetime

def pipeline(df):
    dat = df.sort_index(ascending = True, axis = 0)
    
    #Moving Averages
    dat['MA_5'] = df.Close.rolling(window = 5).mean()
    dat['MA_10'] = df.Close.rolling(window = 10).mean()
    dat['MA_20'] = df.Close.rolling(window = 20).mean()
    dat['MA_50'] = df.Close.rolling(window = 50).mean()
    
    #Bollinger Bands
    dat['SD_20'] = df.Close.rolling(window = 20).std()
    dat['UB'] = dat['MA_20'] + dat['SD_20']*2
    dat['UB'] = dat['MA_20'] - dat['SD_20']*2
    
    #Lagged Data
    dat['Close(t-1)'] = dat.Close.shift(periods = 1)
    dat['Close(t-2)'] = dat.Close.shift(periods = 2)
    dat['Close(t-5)'] = dat.Close.shift(periods = 5)
    dat['Close(t-10)'] = dat.Close.shift(periods = 10)
    
    #Exponential Moving Averages
    dat['EMA_5'] = df.Close.ewm(span = 5, adjust = False).mean().fillna(0)
    dat['EMA_10'] = df.Close.ewm(span = 10, adjust = False).mean().fillna(0)
    dat['EMA_20'] = df.Close.ewm(span = 20, adjust = False).mean().fillna(0)
    dat['EMA_50'] = df.Close.ewm(span = 50, adjust = False).mean().fillna(0)
    dat['EMA_12'] = df.Close.ewm(span = 12, adjust = False).mean().fillna(0)
    dat['EMA_26'] = df.Close.ewm(span = 26, adjust = False).mean().fillna(0)
    
    #Moving Average Convergence Divergence
    dat['MACD'] = dat['EMA_12'] - dat['EMA_26']
    
    #Average True Range
    dat['ATR'] = df['High'].subtract(df['Low']).rolling(window = 14).mean()
    
    #Commodity Channel Index
    dat['Typ_price'] = (df['High'] + df['Low'] + df['Close'])/3
    dat['CCI_MA'] = dat['Typ_price'].rolling(window = 20).mean()
    dat['MD'] = (dat['Typ_price'] - dat['CCI_MA']).abs().rolling(window = 20).mean()
    dat["CCI"] = (dat['Typ_price'] - dat['CCI_MA'])/(0.015 * dat['MD'])
    
    #Average Directional Index
    dat['ADX'] = talib.ADX(df['High'], df['Low'], df['Close'], timeperiod=14)
    
    #Relative Strength Index
    dat['RSI'] = talib.RSI(df.Close.values, timeperiod = 14)
    
    #ROC
    dat['ROC'] = ((df['Close'] - df['Close'].shift(10)) / (df['Close'].shift(10)))*100

    #Williams %R
    dat['WL%R'] =  talib.WILLR(df.High.values, df.Low.values, df.Close.values, 14) 
    
    #Stochastic %K
    dat['STOC%K'] = ((df.Close - df.Low.rolling(window=14).min()) / (df.High.rolling(window=14).max() - df.Low.rolling(window=14).min())) * 100
    
    #SD over last 5 days of percentage change
    dat['pc'] = df.Close.pct_change()
    dat['STD5'] = dat.pc.rolling(window = 5).std()
    
    #Feature Extraction from date
    dat['Date'] = dat.index
    dat['Date'] = pd.to_datetime(dat['Date'])
    dat["dayofweek"] = dat["Date"].dt.dayofweek
    dat["dayofyear"] = dat["Date"].dt.dayofyear
    dat['year'] = dat['Date'].dt.year
    dat['month'] = dat['Date'].dt.month
    dat['week'] = dat['Date'].dt.week
    dat['Is_month_end'] = dat['Date'].dt.is_month_end.astype(int)
    dat['Is_month_start'] = dat['Date'].dt.is_month_start
    dat['Is_quarter_end'] = dat['Date'].dt.is_quarter_end
    dat['Is_quarter_start'] = dat['Date'].dt.is_quarter_start
    dat['Is_year_end'] = dat['Date'].dt.is_year_end
    dat['Is_year_start'] = dat['Date'].dt.is_year_start
    
    return dat
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

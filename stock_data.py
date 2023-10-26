# -*- coding: utf-8 -*-
"""
Created on Thu Oct 26 09:58:45 2023

@author: ASUS
"""


import yfinance as yf 
import pandas as pd 

stocks = ["COROMANDEL.NS", "CIPLA.NS", "TCS.NS", "HDFCBANK.NS", "BPCL.NS"]

data = {}

for i in stocks:
    df = yf.download(i, start = "2010-01-01", end = "2023-10-26")
    data[i] = df
    df.to_csv(f"{i}.csv")

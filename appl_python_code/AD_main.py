#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This is the main script for anomaly detection. It prepares data for computing AD status  
Created on Tue Jan  2 08:53:47 2018
@author: haroonr
"""
#%%
import pandas as pd
import AD_support as ads
#%%
dir = "path to dataset"
home = "House10.csv"
df = pd.read_csv(dir+home,index_col="Time")
df.index = pd.to_datetime(df.index)
df_sub = df["2014-03":] # since before march their are calibration issues
#%% Resampling data
print("*****RESAMPLING********")
df_samp = df_sub.resample('1T',label = 'right', closed ='right').mean()
data_sampling_time = 1 #in minutes
data_sampling_type = "minutes" # or seconds
#scn.rename_appliances(home, df_samp)
#%% select particular appliance for anomaly detection
myapp = "Refrigerator"
train_data =  df_samp[myapp]['2014-04-01' : '2014-04-30'] 
test_data =  df_samp[myapp]['2014-05-01':'2014-06-31']
train_results = ads.AD_refit_training(train_data,data_sampling_type,data_sampling_time)
test_results  = ads.AD_refit_testing(test_data,data_sampling_type,data_sampling_time)            
#%% Anomaly detection logic
num_std = 2
alpha = 2.5
res_df = ads.anomaly_detection_algorithm(test_results,train_results,alpha,num_std)
result_sub = res_df[res_df.status==1]

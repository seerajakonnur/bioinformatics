import array
import pandas as pd
import numpy as np
import csv
# import Normalizer from sklearn.preprocessing library
# import MinMaxScaler from sklearn.preprocessing library
from sklearn.preprocessing import Normalizer
from sklearn.preprocessing import MinMaxScaler
#
# read the csv file into data using pandas
data=pd.read_csv("final_values.csv")
# convert to array
arr=data.values
# Set the range and then normalize the values of the array between the range using min max formula
scaler=MinMaxScaler(feature_range=(0,1))
normalized=scaler.fit_transform(arr)
print(normalized)


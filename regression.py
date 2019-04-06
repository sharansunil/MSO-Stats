import pandas as pd 
import numpy as np 
import matplotlib.pyplot as plt 
import seaborn as sns 


df = pd.read_excel('data.xlsx')
df_sg=df[df.country=='Singapore']
df_sg.to_csv('sg_data.csv')

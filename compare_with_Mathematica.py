import random
import time
import numpy as np
import pandas as pd
import os
import math
import csv

BKV = 3.14159265359

def singleDrop(d=1.0, L=1.0):
    "dropping a single needle"
    y = np.random.uniform(0, d)
    angle = np.random.uniform(0, math.pi)
    height = L/2 * np.sin(angle)
    if (y + height) >= d or (y - height) <= 0:
        return True
    return False
    
def manyDrops(dropCnt=100):
    cnt=0
    hit=0
    pi=0
    for i in range(dropCnt):
        if(singleDrop()):
            hit+=1.0
        cnt+=1.0
        if hit:
            pi=2*cnt/hit
    return pi

pi_values=[]
def run(experimentCnt=100):
    for i in range(experimentCnt):
        pi_values.append(manyDrops())
    return pi_values

res = run()

with open('test.csv', "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    for val in res:
        writer.writerow([val]) 

df1 = pd.read_csv('singlegrid_100-1.csv', header=None)
df2 = pd.read_csv('singlegrid_100-2.csv', header=None)
df3 = pd.read_csv('singlegrid_100-3.csv', header=None)
df4 = pd.read_csv('singlegrid_100-4.csv', header=None)
df5 = pd.read_csv('singlegrid_100-5.csv', header=None)

def extractStatistics(df):
    mean = df.mean()
    error = abs(mean - BKV)
    count = df.count()
    minimum = df.min()
    maximum = df.max()
    return [mean, error, count, minimum, maximum]
print extractStatistics(df1)
import random
import time
import numpy as np
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

with open('testc.csv', "w") as output:
    writer = csv.writer(output, lineterminator='\n')
    for val in res:
        writer.writerow([val]) 



import random
import time
import numpy as np
import sys
import math
from random import *

BKV = 3.14159265359
sampleID = 1
def timedFunction(f, *a, **b):
    start = time.time()
    a = f(*a, **b)
    end = time.time()
    return end - start, a


def singleDrop(d=1.0, L=1.0):
    "dropping a single needle"
    y = np.random.uniform(0, d)
    angle = np.random.uniform(0, math.pi)
    height = L/2 * np.sin(angle)
  #  print y - height
    if (y + height) >= d or (y - height) <= 0:
        return True
    return False
    
def singleExperiment(pl, err, seed, sigfigs, first=True):
    "running a single experiment"
    cnt = 0
    hit = 0
    pi = 0
    BKV_upper = err + round(BKV, sigfigs)
    BKV_lower = round(BKV, sigfigs) - err
    for i in range(pl):
        if(singleDrop()):
            hit += 1.0
        cnt += 1.0
        if hit:
            pi = 2 * cnt / hit
        #print pi
        if pi >= BKV_lower and pi <= BKV_upper and first:
            return pi, cnt, False
    return pi, cnt, True


#@timedfunction
def run(probLmt=10, sigfigs=1, experimentCnt=1000, seed=None, first=True):
    "main method for parallel line"
    entry = []
    global sampleID
    seed = seed or np.random.randint(low=0, high=9999999)
    OFtol= 5.0/(10.0 ** (sigfigs + 1))
    np.random.seed(seed)
    for i in range(experimentCnt):
        isCensored = False
        t, result = timedFunction(singleExperiment, probLmt, OFtol, seed,sigfigs, first=first)
        pi, cnt, isCensored = result
        entry.append({
            "ID": sampleID,
            "Pi Hat": round(pi, 10), 
            "CntProbe": cnt,
            "CntProbeLmt": probLmt, 
            "IsCensored":isCensored, 
            "SeedInit":seed, 
            "Error": round(pi - BKV, 10),
            "OFTol": round(OFtol, 10),
            "Sig Figs": sigfigs,
            "RunTime": t,
            "Experiment": "Needles1"
            })
        
        seed = np.random.randint(low=0, high=9999999)
        np.random.seed(seed)
        yield entry[-1]
        sampleID += 1
    #return entry


if __name__ == "__main__":
    for i in range(1, 9):
        p = run(sigfigs=i, experimentCnt=100, first=True)
        if i == 1:
            for entry in p:
                for key, item in entry.items():
                    print(key, end='\t')
                print()
                break
        for i in p:
            for key, item in i.items():
                print(item, end='\t')
            print()
  #  print("Program running")


# A class to find the experimental value of Pi by throwing darts onto a unit square with a circle with unit diameter embedded inside. 
# Authors: Edward Chan and Annie Tang
# February 9, 2018

import numpy as np
import random
import time
BKV = 3.1415926

def timedfunction(f, *a, **b):
    start = time.time()
    a = f(*a, **b)
    end = time.time()
    return end - start, a


def singleThrow():
    "throwing a single dart"
    x = np.random.uniform(-.5, .5)
    y = np.random.uniform(-.5, .5)
    if np.sqrt(x**2 + y**2) <= 0.5:
        return True
    return False
    
def singleExperiment(pl, err, seed):
    "running a single experiment"
    cnt = 0
    hit = 0
    pi = 0
    BKV_upper = err + BKV
    BKV_lower = BKV - err
    for i in range(pl):
        if(singleThrow()):
            hit += 1.0
        cnt += 1.0
        pi = 4 * hit / cnt
        if hit:
            pi = 4 * hit / cnt
        #print pi
        if pi >= BKV_lower and pi <= BKV_upper:
            return pi, cnt, False
    return pi, cnt, True


    #@timedfunction
def run(probLmt=100000, tol=0.005, experimentCnt=5, seed=None):
    "main method for parallel line"
    entry = []
    seed = seed or np.random.randint(low=0, high=9999)
    np.random.seed(seed)
    for i in range(experimentCnt):
        isCensored = False
        t, result = timedfunction(singleExperiment, probLmt, tol, seed)
        pi, cnt, isCensored = result
        entry.append({
            "Pi": round(pi, 7), 
            "Count": cnt, 
            "IsCensored":isCensored, 
            "Seed":seed, 
            "Error": round(pi - BKV, 7)
            })
        seed = np.random.randint(low=0, high=9999)
        np.random.seed(seed)
    return entry

if __name__ == "__main__":
    p = run()
    for i in p:
        print i
  #  print("Program running")
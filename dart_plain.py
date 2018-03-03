# A class to find the experimental value of Pi by throwing darts onto a unit square with a circle with unit diameter embedded inside. 
# Authors: Edward Chan and Annie Tang
# February 9, 2018

import numpy as np
import random
import time
BKV = 3.1415926
sampleID = 1
def timedFunction(f, *a, **b):
    start = time.time()
    a = f(*a, **b)
    end = time.time()
    return end - start, a


def singleThrow():
    "throwing a single dart"
    x = np.random.uniform(-1, 1)
    y = np.random.uniform(-1, 1)
    if x**2 + y**2 <= 1:
        return True
    return False
    
def singleExperiment(pl, err, seed, sigfigs):
    "running a single experiment"
    cnt = 0
    hit = 0
    pi = 0
    BKV_upper = err + round(BKV, sigfigs)
    BKV_lower = round(BKV, sigfigs) - err
    for i in range(pl):
     #   print(pi)
        if(singleThrow()):
            hit += 1.0
        cnt += 1.0
        if hit:
            pi = 4 * hit / cnt
    return pi, cnt, True


    #@timedfunction
def run(sigfigs=1, probLmt=50 ** 6, tol=0.005, experimentCnt=10, seed=None, first=True):
    "main method for parallel line"
    entry = []
    global sampleID
    seed = seed or np.random.randint(low=0, high=9999999)
    OFtol= 5.0/(10.0 ** (sigfigs + 1))
    np.random.seed(seed)
    for i in range(experimentCnt):
        isCensored = False
        t, result = timedFunction(singleExperiment, probLmt, OFtol, seed,sigfigs)
        pi, cnt, isCensored = result
        entry.append({
            "ID": sampleID,
            "Pi": round(pi, 10), 
            "CntProbe": cnt,
            "CntProbeLmt": probLmt, 
            "IsCensored":isCensored, 
            "Seed":seed, 
            "Error": round(pi - BKV, 10),
            "OFTol": round(OFtol, 10),
            "Significant Figures:": sigfigs,
             "RunTime": t,
             "Experiment": "Dart"
            })
        sampleID += 1
        seed = np.random.randint(low=0, high=9999999)
        np.random.seed(seed)
        yield entry[-1]
    return entry

if __name__ == "__main__":
    for j in range(1, 7):
        total = 0
        lmt = 10 ** j
        p = run(probLmt=lmt)
        if j == 1:
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
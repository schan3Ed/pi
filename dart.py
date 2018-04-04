# A class to find the experimental value of Pi by throwing darts onto a unit square with a circle with unit diameter embedded inside. 
# Authors: Edward Chan and Annie Tang
# February 9, 2018

import numpy as np
import random
import time
import argparse
import sys

BKV = 3.1415926
sampleID = 0
lmt = [500, 1000, 5000, 20000, 50000, 100000, 1000000]
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
    BKV_upper = BKV + err
    BKV_lower = BKV - err
    for i in range(pl):
     #   print(pi)
        if(singleThrow()):
            hit += 1.0
        cnt += 1.0
        if hit:
            pi = 4 * hit / cnt
        if pi >= BKV_lower and pi <= BKV_upper:
            return pi, cnt, False
    return pi, cnt, True


    #@timedfunction
def run(sigfigs=1, probLmt=50 ** 6, tol=0.005, experimentCnt=5, seed=None, first=True):
    entry = []
    probLmt = lmt[sigfigs - 2]
    global sampleID

    OFtol= 5.0 / (10.0 ** sigfigs)
    np.random.seed(seed)
    expcnt = 0
    while expcnt < experimentCnt:
        isCensored = False
        t, result = timedFunction(singleExperiment, probLmt, OFtol, seed, sigfigs)
        pi, cnt, isCensored = result
        if isCensored == False:
            expcnt += 1
            entry.append({
                "sampleId": sampleID,
                "piHat": round(pi, sigfigs - 1), 
                "numThrows": cnt,
                "numThrowsLmt": probLmt, 
                "isCensored": "FALSE", 
                "seedInit":seed, 
                "OFerror": abs(pi - BKV),
       #         "OFtol": round(OFtol, 10),
                "signifDigits": sigfigs,
                "runtime": t,
                "solverName": "Dart"
                })
            
        seed = np.random.randint(low=0, high=9999999)
        np.random.seed(seed)
        if isCensored == False:
            sampleID += 1
            yield entry[-1]
        
    #return entry


def get_file_args():
  parser = argparse.ArgumentParser()
  parser.add_argument("-s", "--seedInit", type=int, default=None, help="Initial seed of experiment")
  parser.add_argument("-d", "--digits", type=int, default=3, help="Max significant digits")
  parser.add_argument("-p", "--samples", type=int, default=100, help="Number of samples in the experiment")

  return parser.parse_args()


if __name__ == "__main__":
    arg = get_file_args()
    seed = np.random.randint(low=0, high=9999999)
    print(seed, file=sys.stderr)
    for i in range(2, 8):
        p = run(sigfigs=i, experimentCnt=100, first=True, seed=seed)
        
        if i == 2:
            for entry in p:
                for key, item in entry.items():
                    print(key, end='\t')
                print()
                break
        for i in p:
            for key, item in i.items():
                print(item, end='\t')
            print()
        seed = np.random.randint(low=0, high=9999999)
  #  print("Program running")
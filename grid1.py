import random
import time
import numpy as np
import sys
import math
from random import *
import argparse

BKV = 3.14159265359
sampleID = 0
lmt = [400, 1000, 5000, 20000, 50000, 100000, 1000000]
def timedFunction(f, *a, **b):
    start = time.time()
    a = f(*a, **b)
    end = time.time()
    return end - start, a


def singleDrop(d=1.0, L=1.0):
    "dropping a single needle"
    y = np.random.uniform(0, d)
    x = np.random.uniform(0, d)
    while x ** 2 + y ** 2 > 1:
        y = np.random.uniform(0, d)
        x = np.random.uniform(0, d)
    angle = math.atan(y / x)
    d = np.random.uniform(0, 0.5)
    height = 0.5 * np.sin(angle)
  #  print y - height
    if d <= height:
        return True
    return False
    
def singleExperiment(pl, err, seed, sigfigs, first=True):
    "running a single experiment"
    cnt = 0
    hit = 0
    pi = 0
    BKV_upper = BKV + err
    BKV_lower = BKV - err
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
def run(probLmt=50 ** 6, sigfigs=1, experimentCnt=100, seed=None, first=True):
    "main method for parallel line"
    entry = []
    probLmt = lmt[sigfigs - 2]
    global sampleID

    OFtol= 0.5 / (10.0 ** sigfigs)
    np.random.seed(seed)
    expcnt = 0
    while expcnt < experimentCnt:
        isCensored = False
        t, result = timedFunction(singleExperiment, probLmt, OFtol, seed, sigfigs, first=first)
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
                "solverName": "Needles1"
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


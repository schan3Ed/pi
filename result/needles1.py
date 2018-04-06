import random
import time
import numpy as np
import sys
import math
from random import *
import argparse

BKV = 3.14159265359
sampleID = 0
lmt = [300, 2000, 8000, 40000, 160000, 500000, 1000000]
entry = []
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
    
def singleExperiment(pl, err, seed, sigfigs):
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
        if pi >= BKV_lower and pi <= BKV_upper:
            return pi, cnt, False
    return pi, cnt, True


#@timedfunction
def run(probLmt=50 ** 6, sigfigs=1, experimentCnt=100, seed=None):
    "main method for parallel line"
    global entry
    probLmt = lmt[sigfigs - 2]
    global sampleID

    OFtol= 0.5 / (10.0 ** sigfigs)
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
                "solverName": "Needles1"
                })
            
        seed = np.random.randint(low=0, high=9999999)
        np.random.seed(seed)
        if isCensored == False:
            sampleID += 1
            yield entry[-1]    

def get_file_args():
  parser = argparse.ArgumentParser()
  parser.add_argument("-s", "--seedInit", type=int, default=None, help="Initial seed of experiment")
  parser.add_argument("-d", "--digits", type=int, default=7, help="Max significant digits")
  parser.add_argument("-p", "--samples", type=int, default=100, help="Number of samples in the experiment")

  return parser.parse_args()

if __name__ == "__main__":
    arg = get_file_args()
    seed = arg.seedInit or np.random.randint(low=0, high=9999999)
    file = open("fg_asym_pi_signif_needles1_" + str(seed) + ".txt", "w")
    print(seed, file=sys.stderr)
    for i in range(2, arg.digits + 1):
        p = run(sigfigs=i, experimentCnt=arg.samples, seed=seed)
        print("Running signif: ", i)
        if i == 2:
            for row in p:
                for key, item in row.items():
                    print(key, end='\t', file=file)
                print(file=file) 
                break
        for i in p:
            for key, item in i.items():
                print(item, end='\t', file=file)
            print(file=file)
        seed = np.random.randint(low=0, high=9999999)
    file.close()
  #  print("Program running")


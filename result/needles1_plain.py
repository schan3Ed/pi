import random
import time
import numpy as np
import sys
import math
import argparse

BKV = 3.14159265359
sampleID = 0
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
    return pi, cnt, True


#@timedfunction
def run(probLmt=10, sigfigs=1, experimentCnt=100, seed=None, first=True):
    "main method for parallel line"
    entry = []
    global sampleID
    OFtol= 5.0/(10.0 ** (sigfigs + 1))
    np.random.seed(seed)
    for i in range(experimentCnt + 1):
        isCensored = False
        t, result = timedFunction(singleExperiment, probLmt, OFtol, seed,sigfigs, first=first)
        pi, cnt, isCensored = result
        entry.append({
            "ID": sampleID,
            "Pi Hat": round(pi, 10), 
            "CntProbe": cnt,
            "SeedInit":seed, 
            "Error": round(pi - BKV, 10),
            "RunTime": t,
            "Experiment": "Needles1"
            })
        
      #  print(t)
        seed = np.random.randint(low=0, high=9999999)
        np.random.seed(seed)
        yield entry[-1]
        sampleID += 1

    #return entry
def get_file_args():
  parser = argparse.ArgumentParser()
  parser.add_argument("-s", "--seedInit", type=int, default=None, help="Initial seed of experiment")
  parser.add_argument("-d", "--digits", type=int, default=7, help="Max significant digits")
  parser.add_argument("-p", "--samples", type=int, default=100, help="Number of samples in the experiment")

  return parser.parse_args()


if __name__ == "__main__":
    arg = get_file_args()
    seed = arg.seedInit or np.random.randint(low=0, high=9999999)
    file = open("fg_asym_pi_plain_needles_" + str(seed) + ".txt", "w")
    
    for j in range(1, 6):
        total = 0
        lmt = 10 ** j
        print("Running limit: ", lmt)
        p = run(probLmt=lmt, seed = seed)
        if j == 1:
            for entry in p:
                for key, item in entry.items():
                    print(key, end='\t', file=file)
                print(file=file)
                break
        for i in p:
            for key, item in i.items():
                print(item, end='\t', file=file)
            print(file=file)
        seed = np.random.randint(low=0, high=9999999)
  #  print("Program running")


import os
import time
import itertools
import subprocess

height = [i for i in "ST"]
hair = [i for i in "BRD"]
sex = [i for i in "MF"]
NUM = 2

def main():

    start_time = time.time()

    # all available pair combinations
    lst = [(h + c + g) for h in height for c in hair for g in sex]
    args = [" ".join(item) for item in itertools.combinations(lst, NUM)]

    results = []

    os.system('ghc -O2 --make Proj1Test')

    for arg in args:
        last = subprocess.getoutput(os.getcwd() + '/Proj1Test ' + arg)
        print(last)
        results.append(int(str(last).split("\n")[-1].split(' ')[-2]))

    print("Score: %.15f guesses" % (sum(results) / len(results)))
    print("CPU time: %s seconds" % (time.time() - start_time))


if __name__ == "__main__":
    main()

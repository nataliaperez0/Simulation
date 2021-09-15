from time import time
from math import sqrt, ceil
from multiprocessing import Pool

def primo(n):
    if n < 4: # 1 2 3
        return True
    if n % 2 == 0:
        return False
    for d in range(3, int(ceil(sqrt(n)))):
        if n % d == 0:
            return False
    return True

if __name__ == "__main__":
    antes = time()
    with Pool() as p:
        print(sum(p.map(primo, range(1, 1000001))))
    print(f'Tardamos {time() - antes}')

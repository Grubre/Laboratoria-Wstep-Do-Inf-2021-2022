import random 
import math
import numpy as np
import matplotlib.pyplot as plt
import statistics

def gen_n_rand_points(n: int, a: float, b: float, M: float):
    arr = []
    for _ in range(n):
        x = a + (b - a) * random.random()
        y = random.random() * M
        arr.append([x,y])
    return arr

def gen_points(a: float, b: float, M: float, n: int, f):
    points = gen_n_rand_points(n,a,b,M)
    C = 0
    for point in points:
        y = f(point[0])
        if y >= point[1]:
            C+=1
    return (C/n) * (b - a) * M

def gen_plot(name: str, f, I: float, a: float, b: float, M: float, n0: int, nmax: int, njump: int, k: int):
    indexes = []
    values = []
    means = []
    const = []
    for n in range(n0, nmax, njump):
        const.append(I)
        indexes.append(n)
        values_k = []
        for i in range(k):
            values_k.append(gen_points(a, b, M, n, f))
        values.append(values_k)
        means.append(statistics.mean(values_k))

    plt.clf()
    plt.plot(indexes, values, color='b', marker='o', markersize = 0.5, linewidth = 0)
    plt.plot(indexes, const, color='g')
    plt.plot(indexes, means, color='r', marker = 'o', markersize = 2, linewidth = 0)
    plt.savefig(name)

    pass

def f1(x):
    return np.cbrt(x);

def f2(x):
    return math.sin(x);

def f3(x):
    return 4 * x * (1-x)**3;

def main():
    integral_cbrtx_0_8 = 12
    integral_sinx_0_pi = 2
    integral_4x1x3_0_1 = 0.2
    max_cbrtx_0_8 = 2
    max_sinx_0_pi = 1
    max_4x1x3_0_1 = 27/64
    gen_plot('sqrt3x', f1, integral_cbrtx_0_8, 0, 8, max_cbrtx_0_8, 50, 5050, 50, 50)
    gen_plot('sinx', f2, integral_sinx_0_pi, 0, np.pi, max_sinx_0_pi, 50, 5050, 50, 50)
    gen_plot('4x(1-x)3', f3, integral_4x1x3_0_1, 0, 1, max_4x1x3_0_1, 50, 5050, 50, 50)
    pass

if __name__ == "__main__":
    main()
    pass


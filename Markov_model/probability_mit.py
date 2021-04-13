# MIT probability plot
import matplotlib.pyplot as plt
import numpy as np

DAY_IN_YEAR = 365
y = []

for n_guest in range(1, 200):
    try:
        prob = (np.math.factorial(DAY_IN_YEAR)/np.math.factorial(DAY_IN_YEAR - n_guest))/np.math.pow(DAY_IN_YEAR, n_guest)
    except:
        pass
    y.append(prob)

fig, ax = plt.subplots()
ax.plot(range(1, 200), y)
ax.set_xlim(0, 50)

from scipy.stats import norm
uda = norm(loc = 180, scale = 34)
uda.pdf(185.01) - uda.pdf(184.99)

from scipy.stats import binom
uda = binom(n = 60, p = 0.15)

import math
import matplotlib.pyplot as plt

data = [1, 2, 3, 4]
math.sqrt(sum([math.pow(i, 2) for i in data])/(len(data)-1))

import numpy as np
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt

fname = "force_comps.dat"

data = np.loadtxt(fname)

plt.figure()
plt.plot(data[:,0], data[:,1], '.')
plt.ylabel("error")
plt.xlabel("force")
plt.show()
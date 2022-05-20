# coding: utf-8
import numpy as np
import pylab
data1 = np.loadtxt("synspec_n12.dat")
data2 = np.loadtxt("synspec_n25.dat")
fig = pylab.figure()
ax = fig.add_subplot(111)
ax.plot(data1[:,0],data1[:,1],data2[:,0],data2[:,1],lw=2)
pylab.show()

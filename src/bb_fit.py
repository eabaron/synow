######################################################################
## Filename:      bb_fit.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Tue Feb 24 09:45:44 2015
## Modified at:   Wed Mar 17 17:11:13 2021
## Modified by:   Eddie Baron <baron@ou.edu>
## Description:   example for class
######################################################################

import numpy as np
import scipy.optimize as opt
import pylab
from astropy import constants as const
import my_funcs

pylab.interactive(True)


# Data errors can also easily be provided:



# The objective function is easily (but less general) defined as the model:

def planck_wl(wl,a,T):
  h = const.h.to('erg s').value
  c = const.c.to('cm/s').value
  k_b = const.k_B.to('erg/(K)').value
  wl_cm = 1.e-8*wl
  if hasattr(wl_cm,"__iter__"):
    planck = []
    for x in wl_cm:
      help = np.exp(-h*c/(x*k_b*T))
      if help >= 1.0:
        planck.append(0.)
      else:
        planck.append(a*1.e-8*(2.*h*c**2/x**5)*help/(1.-help)) # turn to per Angstrom
  else:
    help = np.exp(-h*c/(wl_cm*k_b*T)) 
    if help >= 1.0:
      planck = 0.
    else:
      planck = (2.*h*c**2/wl_cm**5)*help/(1.-help) 
      planck =  a*planck*1.e-8  # turn to per Angstrom
    
  return planck

def estimate_temp(wl_,fl_,sig_,x0=None):
  if x0 is None:
    x0 = np.array([1.0e-6,1e4])

  popt,pcov = opt.curve_fit(planck_wl, wl_, fl_, x0, sig_)
  return popt,pcov


if __name__ == "__main__":

  infile = input("Give File: ")
  try:
    wl_,fl_,sig_ = np.loadtxt(infile,unpack=True)
  except ValueError:
    wl_,fl_ = np.loadtxt(infile,unpack=True)
    sig_ = 0.05*fl_
  else:
      Exception("something is wrong")

  # Initial guess for (a,T), default is 1
  x0    = np.array([1.0e-6, 9600.0])  
  # Usage is very simple:


  popt,pcov = estimate_temp(wl_, fl_, sig_,x0=x0)

  print("a = {:.4e} T = {:.2f}".format(popt[0],popt[1]))
  print("covariance matrix = ")
  print(pcov)

  fig = pylab.figure(1)
  fig.clf()
  ax1 = fig.add_subplot(111)
  
  p1 = ax1.plot(wl_,fl_)
  
  yfit = planck_wl(wl_,*popt)
  
  p2, = ax1.plot(wl_,yfit,lw=2)
  fig.show()

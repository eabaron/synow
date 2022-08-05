######################################################################
## Filename:      run_plot_synow.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Thu Oct  4 10:08:42 2018
## Modified at:   Fri Aug  5 13:17:01 2022
## Modified by:   Eddie Baron <baron@ou.edu>
## Description:   
######################################################################
import numpy as np
import os
import pylab
import re
import scipy
import scipy.integrate
import struct
import tempfile
from astropy import constants as const
from astropy.io import fits 
from astropy import units as U
# from specutils import extinction
import importlib
import subprocess

import pylab
import my_funcs
from matplotlib.ticker import AutoMinorLocator
import fnmatch


def get_fit(runfile):
  process = subprocess.run([runfile], \
                         stdout=subprocess.PIPE, \
                         universal_newlines=True)
  return

def get_lists(synfile,obsfile,z=None):

  global wobs,fobs,wsyn,fsyn
  wobs,fobs = my_funcs.read_sp_data(obsfile,z=z)
  wsyn,fsyn = my_funcs.read_sp_data(synfile)

  return

if __name__ == "__main__":

  HOME = os.getenv('HOME')
  ylab = r'$F_\lambda$ (arbitrary units)'
  xlab = r'$\lambda$ ($\mathrm{\AA}$)'
  minorLocator   = AutoMinorLocator()





  synow_run_file = "new_synow_template.ksh"

  spectrum_file = "synow_spectrum.dat"

  obs_data_file = 'ptf11kly_20110828_MJD55801.12.dat'

               
      
  OBS_DIR = HOME + '/spec/sn2011fe/mark/'
  
  z = None # these data are already deredshifted
  get_fit(synow_run_file)

  obsfile = OBS_DIR + obs_data_file
  synfile = spectrum_file
  get_lists(synfile,obsfile,z=z)

  fig,ax = pylab.subplots(figsize=(8,6))


  maxobs = fobs.max()
  maxsyn = fsyn.max()
  p1, = ax.plot(wobs,fobs*maxsyn/maxobs,lw=2)
  p2, = ax.plot(wsyn,fsyn,lw=2)
  ax.legend([p1,p2],['Observed','SYNOW'])

  my_funcs.bold_labels(ax)
  ax.xaxis.set_minor_locator(minorLocator)
  ax.xaxis.grid(True,which='both')
  ax.set_xlim([3400,10000])

  fig.supylabel(ylab,fontsize=18)
  ax.set_xlabel(xlab,fontsize=18)
  
  
  pylab.tick_params(which='both', width=2)
  pylab.tick_params(which='major', length=7)
  pylab.tick_params(which='minor', length=4, color='r')
  


  fig.show()
  

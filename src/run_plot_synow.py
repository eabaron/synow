######################################################################
## Filename:      run_plot_synow.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Thu Oct  4 10:08:42 2018
## Modified at:   Fri Aug 19 10:24:21 2022
## Modified by:   Eddie Baron <baron@ou.edu>
## Description:   
######################################################################
import numpy as np
import os
import matplotlib.pyplot as plt
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
from matplotlib.ticker import AutoMinorLocator
import fnmatch

import imp

def have_extinction():
  global extinc_found
  try:
    imp.find_module('extinction')
    extinc_found = True
  except ImportError:
    extinc_found = False

    
def read_sp_data(filename,z=None,ebmv=None,rv=None):

  data=np.loadtxt(filename)

  sorted_data=data[data[:,0].argsort(),]

  wldat = sorted_data[:,0]
  fldat = sorted_data[:,1]
  index = fldat.ravel().nonzero()
  wldat = wldat[index]
  fldat = fldat[index]
  if rv is None: rv = 3.1
  if ebmv is not None:
    # fldatu = fldat*\
    #          extinction.reddening(wldat*U.AA,a_v=rv*ebmv,r_v=rv,model='f99')
    if extinc_found:
      fldatu = \
        extinction.apply(extinction.fitzpatrick99(wldat,-rv*ebmv,rv),fldat)
    else:
      raise ValueError("ebmv set, but no extinction routine")
    
  if z is not None: 
    wldat = wldat/(1.+z)

  if ebmv is not None:
    return wldat,fldat,fldatu
  else:
    return wldat,fldat

def read_sp_data_fits(filename,z=None,ebmv=None,rv=None):

#  hdulist = pyfits.open(filename)
  hdulist = fits.open(filename)
  cards = hdulist[ 0 ].header
#
# check if using IRAF style with 4 axes. Assume flux is in AXIS4
#
  if "CDELT1" in cards: # FITS Header
    wldat = cards[ "CRVAL1" ] + cards[ "CDELT1" ] * np.arange( cards[ "NAXIS1" ] )
    fldat = hdulist[ 0 ].data  
  elif "CD1_1" in cards: # IRAF Header
    wldat = cards[ "CRVAL1" ] + cards[ "CD1_1" ] * np.arange( cards[ "NAXIS1" ] )
    fldat = hdulist[ 0 ].data[0][:]
    help_ = fldat.shape
    if len(help_) != 1:
      fldat = fldat[0,:]
  else:
    raise ValueError("No wl scale")

  hdulist.close()
  index = fldat.ravel().nonzero()
  wldat = wldat[index]
  fldat = fldat[index]
  if rv is None: rv = 3.1

  if ebmv is not None:
    # fldatu = fldat*\
    #          extinction.reddening(wldat*U.AA,a_v=rv*ebmv,r_v=rv,model='f99')
    if extinc_found:
      fldatu = \
        extinction.apply(extinction.fitzpatrick99(wldat,-rv*ebmv,rv),fldat)
    else:
      raise ValueError("ebmv set, but no extinction routine")

  if z is not None: 
    wldat = wldat/(1.+z)
    
  if ebmv is not None:
    return wldat,fldat,fldatu
  else:
    return wldat,fldat

def read_new_sp_data_fits(filename,z=None,ebmv=None,rv=None):

#  hdulist = pyfits.open(filename)
  hdulist = fits.open(filename)
  cols = hdulist[1].columns
  #   cols.info
  data = hdulist[1].data
  wldat = data.WAVELENGTH[0]
  fldat = data.FLUX[0]
  hdulist.close()
  if rv is None: rv = 3.1
      
  if ebmv is not None:
    # fldatu = fldat*\
    #          extinction.reddening(wldat*U.AA,a_v=rv*ebmv,r_v=rv,model='f99')
    if extinc_found:
      fldatu = \
        extinction.apply(extinction.fitzpatrick99(wldat,-rv*ebmv,rv),fldat)
    else:
      raise ValueError("ebmv set, but no extinction routine")

  if z is not None: 
    wldat = wldat/(1.+z)

  if ebmv is not None:
    return wldat,fldat,fldatu
  else:
    return wldat,fldat

def read_calspec_sp_data_fits(filename,z=None,ebmv=None,rv=None):

#  hdulist = pyfits.open(filename)
  hdulist = fits.open(filename)
  cols = hdulist[1].columns
  #   cols.info
  data = hdulist[1].data
# before python 3
# changed to read CALSPEC files
# may need two routines
  # wldat = data.WAVELENGTH[0]
  # fldat = data.FLUX[0]
  wldat = data.WAVELENGTH
  fldat = data.FLUX
  hdulist.close()
  if rv is None: rv = 3.1
  if ebmv is not None:
    # fldatu = fldat*\
    #          extinction.reddening(wldat*U.AA,a_v=rv*ebmv,r_v=rv,model='f99')
    if extinc_found:
      fldatu = \
        extinction.apply(extinction.fitzpatrick99(wldat,-rv*ebmv,rv),fldat)
    else:
      raise ValueError("ebmv set, but no extinction routine")

  if z is not None: 
    wldat = wldat/(1.+z)


  if ebmv is not None:
    return wldat,fldat,fldatu
  else:
    return wldat,fldat


def read_sdss_data_fits(filename,z=None,ebmv=None,rv=None):

  hdulist = fits.open(filename)
  cards = hdulist[ 0 ].header
  if "CD1_1" in cards: # IRAF Header
    wldat = cards[ "CRVAL1" ] + cards[ "CD1_1" ] * np.arange( cards[ "NAXIS1" ] )
    fldat = hdulist[ 0 ].data[0]
  else:
    raise ValueError("No wl scale")

  hdulist.close()
  index = fldat.ravel().nonzero()
  wldat = 10.**wldat[index]
  fldat = fldat[index]
  if rv is None: rv = 3.1

  if ebmv is not None:
    # fldatu = fldat*\
    #          extinction.reddening(wldat*U.AA,a_v=rv*ebmv,r_v=rv,model='f99')
    if extinc_found:
      fldatu = \
        extinction.apply(extinction.fitzpatrick99(wldat,-rv*ebmv,rv),fldat)
    else:
      raise ValueError("ebmv set, but no extinction routine")

  if z is not None: 
    wldat = wldat/(1.+z)

  if ebmv is not None:
    return wldat,fldat,fldatu
  else:
    return wldat,fldat

def read_lcogt_fits(infile):
  """
  read very strangely stored wavelengths for LCOGT fits files
  Lluis helped me figure out how to get xmin
  """
  print(infile)
  hdulist = fits.open(infile)
  cards = hdulist[0].header
  data = hdulist[0].data
  n3 = cards['NAXIS1']
  xmin = cards['CRVAL1'] - cards['LTV1'] * cards['CD1_1']
  xmax = cards['XMAX']
  wldat = xmin + cards["CD1_1"]*np.arange(cards["NAXIS1"])
  data_ = dict()
  data_['flux_raw'] = data[0,0,:]
  data_['flux_corr'] = data[1,0,:]
  data_['errs'] = data[2,0,:]
  data_['unknown'] = data[3,0,:]
  data_['wl'] = wldat
  return data_


def get_fit(runfile):
  process = subprocess.run([runfile], \
                         stdout=subprocess.PIPE, \
                         universal_newlines=True)
  return

def get_lists(synfile,obsfile,z=None):

  global wobs,fobs,wsyn,fsyn
  if '.fits' in obsfile:
    wobs,fobs = read_sp_data_fits(obsfile,z=z)
  else:
    wobs,fobs = read_sp_data(obsfile,z=z)

  wsyn,fsyn = read_sp_data(synfile)

  return

def bold_labels(ax,fontsize=None):
  if fontsize is None:
    fontsize = 14
  for tick in ax.xaxis.get_major_ticks():
    tick.label1.set_fontsize(fontsize)
    tick.label1.set_fontweight('bold')
  for tick in ax.yaxis.get_major_ticks():
    tick.label1.set_fontsize(fontsize)
    tick.label1.set_fontweight('bold')

if __name__ == "__main__":

  have_extinction()
  if extinc_found:
    import extinction

  HOME = os.getenv('HOME')
  EBHOME = os.path.expanduser("~baron")
  ylab = r'$F_\lambda$ (arbitrary units)'
  xlab = r'$\lambda$ ($\mathrm{\AA}$)'
  minorLocator   = AutoMinorLocator()





  synow_run_file = "new_synow_template.ksh"

  spectrum_file = "synow_spectrum.dat"

  obs_data_file = 'ptf11kly_20110828_MJD55801.12.dat'

               
      
  OBS_DIR = EBHOME + '/spec/sn2011fe/mark/'
  
  z = None # these data are already deredshifted
  get_fit(synow_run_file)

  obsfile = OBS_DIR + obs_data_file
  synfile = spectrum_file
  get_lists(synfile,obsfile,z=z)

  fig,ax = plt.subplots(figsize=(8,6))


  maxobs = fobs.max()
  maxsyn = fsyn.max()
  p1, = ax.plot(wobs,fobs*maxsyn/maxobs,lw=2)
  p2, = ax.plot(wsyn,fsyn,lw=2)
  ax.legend([p1,p2],['Observed','SYNOW'])

  bold_labels(ax)
  ax.xaxis.set_minor_locator(minorLocator)
  ax.xaxis.grid(True,which='both')
  ax.set_xlim([3400,10000])

  ax.set_ylabel(ylab,fontsize=18)
  ax.set_xlabel(xlab,fontsize=18)
  
  
  plt.tick_params(which='both', width=2)
  plt.tick_params(which='major', length=7)
  plt.tick_params(which='minor', length=4, color='r')
  


  fig.show()
  

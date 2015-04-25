######################################################################
## Filename:      parse_ref.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Fri Mar  6 10:55:05 2015
## Modified at:   Sat Apr 25 14:56:05 2015
## Modified by:   Eddie Baron <baron@ou.edu>
## Description:   read ref.dat file and use for annotating plots
######################################################################
from __future__ import print_function, division
import numpy as N
import pylab
import my_funcs
pylab.interactive(True)
from matplotlib.ticker import AutoMinorLocator
import os
import os.path
import tempfile
import f90nml
def parse_runsynow(infile):
#  
  fout = tempfile.NamedTemporaryFile(delete=False)
  good = False
  with fout:
    with open(infile,'r') as f:
      for line in f:
        if(line.find("&parms") != -1):
          good = True
        if(line.find("/") != -1 and good):
          good = False
          print(line,file=fout)
        if(good): print(line,file=fout)
  tmpfile = fout.name
  mylist = f90nml.read(tmpfile)
  if os.path.isfile(tmpfile): os.unlink(tmpfile)
  return mylist

def parse_and_plot_ref(runfile,spectrum_file):
  fields = [('wl','f8'),('gf','f8'),('z','i'),('istg','i'),('chi','f8')]
  ref = N.loadtxt("ref.dat",dtype=fields)

  model = N.loadtxt(spectrum_file)

  mylist = parse_runsynow(runfile)
  numref = mylist['parms']['numref']
  an = []
  ai = []
  for x,y,z in zip(mylist['parms']['tau1'][:numref],\
                   mylist['parms']['an'][:numref],\
                   mylist['parms']['ai'][:numref]):
    if x > 0.:
      an.append(y)
      ai.append(z)
                   
  ions_used = [ z*100+istg for z,istg in zip(an,ai) ]
  ref_ions = []
  for i in xrange(N.size(ref['wl'])):
    ref_ions.append(ref['z'][i]*100 + ref['istg'][i])
  
  ref_index = []
  for ion in ions_used:
    ref_index.append(ref_ions.index(ion))


  pylab.interactive(True)

# One can supply an argument to AutoMinorLocator to
# specify a fixed number of minor intervals per major interval, e.g.:
# minorLocator = AutoMinorLocator(2)
# would lead to a single minor tick between major ticks.

  minorLocator   = AutoMinorLocator()

  golden = (pylab.sqrt(5)+1.)/2.


  figprops = dict(figsize=(8., 8./ golden ), dpi=128)    # Figure properties for single and stacked plots 
# figprops = dict(figsize=(16., 8./golden), dpi=128)    # Figure properties for side by sides
  adjustprops = dict(left=0.15, bottom=0.1, right=0.90, top=0.93, wspace=0.2, hspace=0.2)       # Subp

  fig = pylab.figure(1,**figprops)   # New figure
  fig.clf()
  fig.subplots_adjust(**adjustprops)  # Tunes the subplot layout

  ax1 = fig.add_subplot(1, 1, 1)

  my_funcs.bold_labels(ax1)

  p1, = ax1.plot(model[:,0],model[:,1],linewidth=2.0)
  ax1.set_ylabel(r'$F_\lambda$',fontsize=14)
  ax1.set_xlabel(r'$\lambda\ (\AA)$', fontsize=14)
# ax1.set_xlim([0.,60.])
# ax1.set_ylim([10.**41.4,10.**43.5])
# ax1.set_yscale('log')
# ax1.legend([p1,p2,p3,p4],['Day 10','Day 15','Day 25','Day 50'],frameon=False)
  ax1.xaxis.set_minor_locator(minorLocator)

  pylab.tick_params(which='both', width=2)
  pylab.tick_params(which='major', length=7)
  pylab.tick_params(which='minor', length=4, color='r')

#ax1.xaxis.grid(True,which='minor')
  ax1.xaxis.grid(True,which='both')


  wl_ref = []
  f_ref = []
  ymin,ymax = ax1.get_ybound()
  for i in ref_index:
    wl_ref.append([10.*ref['wl'][i],10.*ref['wl'][i]])
    ihelp = N.abs(model[:,0]-10.*ref['wl'][i]).argmin()
    yhelp = model[ihelp,1]
    f_ref.append([ymin,yhelp])

  for x,y in zip(wl_ref,f_ref):
    ax1.plot(x,y,lw=2)

  fields = [('Z','i'),('A','f8'),('Name','S13'),('sym','S4'),('MP','f8'),\
            ("BP",'f8'),('rho','f8'),('crust','f8'),('year','i'),\
            ('group','i'), ('config','S23'), ('chiion',"f8")]
  
# labels = N.loadtxt("periodic_table.dat",skiprows=1,delimiter=',',dtype=fields)  
  labels = N.genfromtxt("periodic_table.dat",skip_header=1,delimiter=',',dtype=None)  

  syms = []
  for x in labels['f3']:
    syms.append(x.replace(" ",""))

  ref_Zs = []
  for z in labels['f0']:
    ref_Zs.append(z)
  
  sym_indices = []
  for z in an:
    sym_indices.append(ref_Zs.index(z))

  spect_notation = ["I","II","III","IV","V","VI","VII","VIII","IX","X"]
  text_labels = []
  for i,j in enumerate(sym_indices):
    help = syms[j] + " " + spect_notation[ai[i]]
    text_labels.append(help)
  
  for x,y,l in zip(wl_ref,f_ref,text_labels):
    ax1.text(x[0],min(y[1]*1.08,ymax),l,fontsize=8)

# figname = "example"
# png_name=figname+".png"
# eps_name=figname+".eps"
# fig.savefig(png_name)
# fig.savefig(eps_name)
if __name__ == "__main__":

  runfile = input("Synow runfile:")
  specfile = input("Synow Spectrum file:")
  parse_and_plot_ref(runfile,specfile)

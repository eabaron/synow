import synowpy
import os
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator

if __name__ == "__main__":

  synowpy.have_extinction()
  if synowpy.extinc_found:
    import extinction

  HOME = os.getenv('HOME')
  EBHOME = os.path.expanduser("~baron")
  ylab = r'$F_\lambda$ (arbitrary units)'
  xlab = r'$\lambda$ ($\mathrm{\AA}$)'
  minorLocator   = AutoMinorLocator()





  synow_run_file = "check_density_bump.sh"

  spectrum_file = "synow_spectrum_wbump.dat"

  obs_data_file = "synow_spectrum_nobump.dat"

  synow_run_file = "detached_shell.sh"

  spectrum_file = "synow_spectrum_detached_shell.dat"

  obs_data_file = "synow_spectrum_nobump.dat"

  synow_run_file = "check_gaussian.sh"

  spectrum_file = "synow_spectrum_gaussian.dat"

  obs_data_file = "synow_spectrum_nobump.dat"

               
  CUR_DIR = os.getenv("PWD")
      
  OBS_DIR = f'{EBHOME}/spec/sn2011fe/mark/'
  OBS_DIR = CUR_DIR
  
  obsfile = f'{OBS_DIR}/{obs_data_file}'
  synfile = spectrum_file
  os.environ["SPECTRUM_FILE"] = f"{synfile}"

  z = None # these data are already deredshifted
  synowpy.get_fit(synow_run_file)

  synowpy.get_lists(synfile,obsfile,z=z,OBS=True)

  fig,ax = plt.subplots(figsize=(8,6))


  # maxobs = synowpy.fobs.max()
  # maxsyn = synowpy.fsyn.max()
  # p1, = ax.plot(synowpy.wobs,synowpy.fobs*maxsyn/maxobs,lw=2)
  p1, = ax.plot(synowpy.wobs,synowpy.fobs,lw=2)
  p2, = ax.plot(synowpy.wsyn,synowpy.fsyn,lw=2)
  ax.legend([p1,p2],['No Bump','Bump'])

  synowpy.bold_labels(ax)
  ax.xaxis.set_minor_locator(minorLocator)
  ax.xaxis.grid(True,which='both')
  ax.set_xlim([5000,8000])

  ax.set_ylabel(ylab,fontsize=18)
  ax.set_xlabel(xlab,fontsize=18)
  
  
  plt.tick_params(which='both', width=2)
  plt.tick_params(which='major', length=7)
  plt.tick_params(which='minor', length=4, color='r')
  


  fig.show()
  

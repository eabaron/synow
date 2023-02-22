from synPy import SynowModel
from synPy.io import read_data
from spextractor import Spextractor
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator

def bold_labels(ax,fontsize=None):
  if fontsize is None:
    fontsize = 14
  for tick in ax.xaxis.get_major_ticks():
    tick.label1.set_fontsize(fontsize)
    tick.label1.set_fontweight('bold')
  for tick in ax.yaxis.get_major_ticks():
    tick.label1.set_fontsize(fontsize)
    tick.label1.set_fontweight('bold')


global_params = {
    'synow_lines_path'     : '/Volumes/small_backup/baron/synow_lines/',
    'kurucz_linelist_path' : '/Volumes/small_backup/baron/lines/',
    'refdata_path'         : '$HOME/synow/src/',

    'spectrum_file'        : 'synthetic.dat',

    'vphot'                : 5000.0,
    'vmax'                 : 40000.0,
    'tbb'                  : 8000.0,

    'stspec'               :  3000.0,
    'ea'                   :  3200.0,
    'eb'                   :  9000.0,
}

# For fitting
fn = '/Users/baron/Downloads/SN2023bvj_gr4_NOT_AL_20230221.fits'
z = 0.00512



model = SynowModel()
model.set_params(**global_params)


# Manual Fitting and Model Setup

feature = { 'an' : 1, 'ai' : 0, 'tau1' : 20., 'vmine' : 5, 'vmaxe' : 40,
            've' : 3.0 }
model.add('H I', **feature)

feature = { 'an' : 6, 'ai' : 1, 'tau1' : 3, 'vmine' : 5, 'vmaxe' : 40,
            've' : 1.0 }
model.add('C II', **feature)

# feature = { 'an' : 14, 'ai' : 1, 'tau1' : 4.5, 'vmine' : 10.0, 'vmaxe' : 16,
#             've' : 1.9, 'temp' : 15 }
# model.add('Si II', **feature)

# feature = { 'an' : 14, 'ai' : 2, 'tau1' : 1.4, 'vmine' : 10, 'vmaxe' : 40,
#             've' : 1.0 }
# model.add('Si III', **feature)

# feature = { 'an' : 16, 'ai' : 1, 'tau1' : 1.6, 'vmine' : 10, 'vmaxe' : 40,
#             've' : 1.23 }
# model.add('S II', **feature)

# feature = { 'an' : 20, 'ai' : 1, 'tau1' : 70.0, 'vmine' : 12, 'vmaxe' : 18,
#             've' : 1.2 }
# model.add('Ca II', **feature)

# feature = { 'an' : 20, 'ai' : 1, 'tau1' : 1.5, 'vmine' : 20, 'vmaxe' : 40,
#             've' : 2.0 }
# model.add('Ca II HV', **feature)

# feature = { 'an' : 26, 'ai' : 1, 'tau1' : 0.3, 'vmine' : 10, 'vmaxe' : 40,
#             've' : 1.0 }
# model.add('Fe III', **feature)

# feature = { 'an' : 26, 'ai' : 1, 'tau1' : 0.4, 'vmine' : 16, 'vmaxe' : 40 }
# model.add('Fe II HV', **feature)

# feature = { 'an' : 27, 'ai' : 1, 'tau1' : 0.2, 'vmine' : 12, 'vmaxe' : 40 }
# model.add('Co II', **feature)

# # feature = { 'an' : 28, 'ai' : 1, 'tau1' : 0.0, 'vmine' : 12, 'vmaxe' : 18 }
# # model.add('Ni II', **feature)

# # feature = { 'an' : 26, 'ai' : 1, 'tau1' : 0.0, 'vmine' : 20, 'vmaxe' : 40 }
# # model.add('Ni II HV', **feature)

# # feature = { 'an' : 6, 'ai' : 1, 'tau1' : 0.0, 'vmine' : 12, 'vmaxe' : 40 }
# # model.add('C II', **feature)

# # feature = { 'an' : 11, 'ai' : 0, 'tau1' : 0.0, 'vmine' : 12, 'vmaxe' : 40 }
# # model.add('Na I', **feature)

# # feature = { 'an' : 12, 'ai' : 1, 'tau1' : 0.0, 'vmine' : 20, 'vmaxe' : 40 }
# # model.add('Mg II HV', **feature)

# # feature = { 'an' : 14, 'ai' : 1, 'tau1' : 1.0, 'vmine' : 22, 'vmaxe' : 40 }
# # model.add('Si II HV', **feature)

# # feature = { 'an' : 22, 'ai' : 1, 'tau1' : 0.0, 'vmine' : 12, 'vmaxe' : 40 }
# # model.add('Ti II', **feature)

# # feature = { 'an' : 26, 'ai' : 1, 'tau1' : 1.0, 'vmine' : 12, 'vmaxe' : 18 }
# # model.add('Fe II', **feature)


# Fitting
obs_data = read_data(fn, z=z)

# Can fit the params (feat_params) that allow synow to fit the data best.
# Bounds (feat_bounds) are for each different parameter. The feature is just
# the label from above (whatever you define it as; synow doesn't depend on it.)
# The parameters (feat_params) should be the exact synow feature parameter
# names.
# bounds = [(0., 20.), (0.5, 3.0)]
# model.fit(obs_data, wave_range=(6000., 6900.), feature='H I',
#           feat_params=['tau1', 've'], feat_bounds=bounds)

# Rerun synow
model_data = model._run_synow_and_retrieve()
wsyn = model_data[:,0]
fsyn = model_data[:, 1]

# Get final save
model.save()
spex = Spextractor(obs_data)
spex.create_model(downsampling=3)
wobs = obs_data[:,0]
fobs, _ = spex.predict(wsyn)
                     
fig,ax = plt.subplots(figsize=(8,6))


maxobs = fobs.max()
maxsyn = fsyn.max()
# p1, = ax.plot(wsyn,fobs*maxsyn/maxobs,lw=2)
p1, = ax.plot(wsyn,fobs,lw=2)
p2, = ax.plot(wsyn,fsyn,lw=2)
ax.legend([p1,p2],['Observed','SYNOW'])


ylab = r'$F_\lambda$ (arbitrary units)'
xlab = r'$\lambda$ ($\mathrm{\AA}$)'
minorLocator   = AutoMinorLocator()

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

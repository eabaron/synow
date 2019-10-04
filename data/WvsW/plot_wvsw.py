#!/opt/local/bin/python
######################################################################
## Filename:      plot_wvsw.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Wed Apr 25 11:21:02 2012
## Modified at:   Wed Jun 13 11:03:20 2012
## Modified by:   Eddie Baron <baron@ou.edu>
## $Id:$
## Description:   
######################################################################

import pylab
import numpy as N
from matplotlib.font_manager import FontProperties
from my_funcs import *

golden = (pylab.sqrt(5)+1.)/2.


figprops = dict(figsize=(8., 8./ golden ), dpi=128)    # Figure properties for single and stacked plots 
# figprops = dict(figsize=(16., 8./golden), dpi=128)    # Figure properties for side by sides
adjustprops = dict(left=0.15, bottom=0.1, right=0.90, top=0.93, wspace=0.2, hspace=0.2)       # Subp

fig = pylab.figure(**figprops)   # New figure
fig.subplots_adjust(**adjustprops)  # Tunes the subplot layout

ax1 = fig.add_subplot(1, 1, 1)

bold_labels(ax1)

data_ss = N.loadtxt("WvsWSS.dat",dtype=([('w1',float),('w2',float),\
                                             ('name',str,4)]))
data_cl = N.loadtxt("WvsWCL.dat",dtype=([('w1',float),('w2',float),\
                                             ('name',str,4),('l1',float),\
                                             ('l2',float),('l3',float),\
                                             ('l4',float) ]))
data_cn = N.loadtxt("WvsWCN.dat",dtype=([('w1',float),('w2',float),\
                                             ('name',str,4),('l1',float),\
                                             ('l2',float),('l3',float), \
                                             ('l4',float) ])) 
data_bl = N.loadtxt("WvsWBL.dat",dtype=([('w1',float),('w2',float),\
                                             ('name',str,4)]))



ax1.plot([50.],[8.4],'.',markersize=60.0,color="None")
ax1.arrow(35,20,8,-7,width=.4) # x,y,deltax,deltay,keyword
ax1.text(30,26,"99dq",fontsize=8)
ax1.text(30,24,"99aa",fontsize=8)
ax1.text(30,22,"98es",fontsize=8)
ax1.text(30,20,"99gp",fontsize=8)
for i in range(len(data_ss['w1'])):
    p1, = ax1.plot(data_ss['w1'][i],data_ss['w2'][i],ls="None",marker="^",color='blue')
    if data_ss['name'][i] not in ("99dq","99aa","98es","99gp","05hk","00cx","99ee","00E"):
        ax1.text(data_ss['w1'][i],data_ss['w2'][i]-2,\
                     data_ss['name'][i],fontsize=8)
    if data_ss['name'][i] in ("05hk","99ee"):
        ax1.text(data_ss['w1'][i]-1,data_ss['w2'][i]+1,\
                     data_ss['name'][i],fontsize=8)
    if data_ss['name'][i] in ("00cx","00E"):
        ax1.text(data_ss['w1'][i]-2,data_ss['w2'][i],\
                     data_ss['name'][i],fontsize=8)

#circ=pylab.Circle((50.,9.5),radius=1)
#ax1.add_patch(circ)
for i in range(len(data_cl['w1'])):
    p2, = ax1.plot(data_cl['w1'][i],data_cl['w2'][i],ls="None",marker="d",color='red')
    if data_cl['name'][i] not in ("91bg","89B"):
        ax1.text(data_cl['w1'][i],data_cl['w2'][i]-2,\
                     data_cl['name'][i],fontsize=8)
    elif data_cl['name'][i] in ("91bg"):
        ax1.text(data_cl['w1'][i],data_cl['w2'][i]+1,\
                     data_cl['name'][i],fontsize=8)
    elif data_cl['name'][i] in ("89B"):
        ax1.text(data_cl['w1'][i]+2,data_cl['w2'][i],\
                     data_cl['name'][i],fontsize=8)
for i in range(len(data_bl['w1'])):
    p3, = ax1.plot(data_bl['w1'][i],data_bl['w2'][i],ls="None",marker="s",color='green')
    if data_bl['name'][i] not in ("99ej","99cc","81B","02bf"):
        ax1.text(data_bl['w1'][i],data_bl['w2'][i]-2,\
                     data_bl['name'][i],fontsize=8)
    elif data_bl['name'][i] in ("81B"):
        ax1.text(data_bl['w1'][i]-10,data_bl['w2'][i],\
                     data_bl['name'][i],fontsize=8)
    else:
        ax1.text(data_bl['w1'][i],data_bl['w2'][i]+1,\
                     data_bl['name'][i],fontsize=8)


p4, = ax1.plot(data_cn['w1'],data_cn['w2'],ls="None",marker="o",\
                   fillstyle="full",color='k')

ax1.set_ylabel(r'W(5750)',fontsize=14)
ax1.set_xlabel(r'W(6100)', fontsize=14)
ax1.set_xlim([0.,215.])
# ax1.set_ylim([10.**41.4,10.**43.5])
# ax1.set_yscale('log')
# legend_font_props = FontProperties()
# legend_font_props.set_size('small')
# ax1.legend([p1,p2,p3,p4],['Shallow Silicons','Cools','Broad Lines','Core Normals'],frameon=True,loc = 'upper left',prop=legend_font_props)
#ax1.text(pi/2.-.1,.6,"a trough")
#pylab.figtext(.5,.5,"the middle")
ax1.plot(10.,56,ls="None",marker="^",color="blue")
ax1.text(15,55,"Shallow Silicons",fontsize=10)
ax1.plot(10.,51,ls="None",marker="d",color="red")
ax1.text(15,50,"Cools",fontsize=10)
ax1.plot(10.,46,ls="None",marker="s",color="green")
ax1.text(15,45,"Broad Lines",fontsize=10)
ax1.plot(10.,41,ls="None",marker="o",color="k")
ax1.text(15,40,"Core Normals",fontsize=10)
pylab.show()
figname = "wvsw_py"
pdf_name=figname+".pdf"
png_name=figname+".png"
eps_name=figname+".eps"
fig.savefig(pdf_name)
fig.savefig(png_name)
fig.savefig(eps_name)

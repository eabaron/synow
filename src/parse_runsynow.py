######################################################################
## Filename:      parse_runsynow.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Fri Apr 24 17:16:35 2015
## Modified at:   Fri Apr 24 17:42:39 2015
## Modified by:   Eddie Baron <baron@ou.edu>
## Description:   parse the namelist
######################################################################
from __future__ import print_function,division
import os
import os.path
tmpfile = "tmp_list.nml"
if os.path.isfile(tmpfile): os.unlink(tmpfile)
good = False
with open(tmpfile,'w') as fout:
  with open("runsynow.ksh",'r') as f:
    for line in f:
      if(line.find("&parms") != -1):
        good = True
      if(line.find("/") != -1 and good):
        good = False
        print(line,file=fout)
      if(good): print(line,file=fout)

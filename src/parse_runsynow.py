######################################################################
## Filename:      parse_runsynow.py
## Author:        Eddie Baron <baron@ou.edu>
## Created at:    Fri Apr 24 17:16:35 2015
## Modified at:   Fri Oct 18 16:24:51 2019
## Modified by:   Eddie Baron <baron@ou.edu>
## Description:   parse the namelist
######################################################################
from __future__ import print_function,division
import os
import os.path
import tempfile
import f90nml

def parse_runsynow(infile):
#  
#  fout = tempfile.NamedTemporaryFile(delete=False)
 
  good = False
  with open('/tmp/fout.tmp','w') as fout:
    with open(infile,'r') as f:
      for line in f:
        # print(line)
        if(line.find("&parms") != -1):
          good = True
        if(line.find("/") != -1 and good):
          good = False
          print(line,file=fout)
        if(good): print(line,file=fout)
  # tmpfile = fout.name
  tmpfile = '/tmp/fout.tmp'
  mylist = f90nml.read(tmpfile)
  if os.path.isfile(tmpfile): os.unlink(tmpfile)
  # import json
  # json.dumps(mylist,indent=1)
  # import pprint
  # pprint.pprint(mylist)
  return mylist

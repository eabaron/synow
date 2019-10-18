#- Mac
#- ----
#-
   FCOMP = gfortran
   FCOPT = -O -ffree-form -m64 -fconvert=big-endian
#   FCOPT = -g -Wall -fno-automatic  -ffree-form -fbounds-check -fconvert=big-endian
   FLOPT =  -m64 -L/usr/lib
#-
#- Linux with gfortran
#- ----
#-
#  FCOMP = gfortran 
#  FCOPT = -O -ffree-form -fconvert=big-endian -m64
#  FLOPT = -m64
#  export FORT_CONVERT100000=big-endian
#- 
#- ifort
#-----------------------------
# FCOMP = ifort
# FCOPT=-O3 -free -m64 -convert big_endian
# FLOPT = -m64
#-
#- hp :
#- ----
#-
#  FCOMP =
#  FCOPT =
#  FLOPT = 
#-
#- oscar/wes:
#- Sun Ultra 5, on SunOS 5.7
#- -------------------------

#  FCOMP = f95
#  FCOPT = -O4 -e -free
#  FLOPT = 
#-
#- sputnick:
#- Intel Pentium MMX, lf95 5.5 on Linux Debian 2.2
#- -----------------------------------------------
#-
#  FCOMP = /usr/bin/lf95
#  FCOPT = --fix --tp --nchk --ntrace --nsav --nstchk -O -nw --info 
#  FLOPT =
#-
#- monk/blakey:
#- IBM RS/6000, xlf95 6.1 on AIX 4.3
#- ---------------------------------
#-
#-  FCOMP = /usr/bin/xlf95
#  FCOPT = -O3 -qinit=f90ptr -qstrict -qtune=auto -qarch=auto -qlanglvl=95pure -qfixed
#-  FCOPT = -qautodbl=dbl8
#  FLOPT = -lmass -bmaxdata:0x80000000
#  FLOPT =
#-

all: synow

TMP_DIR = Synow-0.1-0

OBJ_DRIVER = \
Global.o \
Inform.o \
Const.o \
Sort.o \
KuruczCtrl.o \
Kurucz.o \
locnorm.o \
LteLineList.o \
alt.o \
getbin.o \
initial.o \
source.o \
spectrum.o \
synnew.o

OBJ = $(OBJ_DRIVER)

synow: $(OBJ)
	$(FCOMP) $(OBJ) $(FLOPT) -o synow

.f.o:
	$(FCOMP) -c $(FCOPT) $<

clean:
	rm -f *.o *.mod

realclean:
	rm -f *.o  *.mod spectrum.out synow

srcdist:
	mkdir $(TMP_DIR).SRC
	cp `xargs < MANIFEST.SRC` $(TMP_DIR).SRC
	tar cvf $(TMP_DIR).SRC.tar $(TMP_DIR).SRC
	gzip $(TMP_DIR).SRC.tar
	rm -rf $(TMP_DIR).SRC

bindist:
	mkdir $(TMP_DIR).BIN
	cp `xargs < MANIFEST.BIN` $(TMP_DIR).BIN
	tar cvf $(TMP_DIR).BIN.tar $(TMP_DIR).BIN
	gzip $(TMP_DIR).BIN.tar
	rm -rf $(TMP_DIR).BIN

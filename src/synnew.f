      program synnew
      
      use Global 
      
      implicit none
      
      integer, parameter :: nradstep=10000, nwavestep = 1024
      integer blueline, thisline, ifile, isto, isto1, isto2, isto3, &
      		  isto4, used, usedold, numref, is, grid, jrlim, nlam, &
      		  ffilenum, sfilenum, last, i, j, an(50), ai(50)
      real(wp) elamx(92, 0:5), gfx(92, 0:5), chix(92, 0:5), &
      		   taux(92, 0:5, nradstep), vphot, vmax, tbb, ea, eb, &
      		   taumin, zeta, stspec, onepvdc, delta_v, &
      		   elam(nwavestep), t(nwavestep, nradstep), &
      		   s(nwavestep, nradstep), td(nwavestep, nradstep), &
      		   sd(nwavestep, nradstep), filparm(50, 5), black(10000), &
      		   xplot(10000), CTHETA(nradstep, 21)
      logical flambda, notcalled, new
 
      include "param.inc"
      include "radial.inc"

      notcalled = .true.
      new = .true.

!     READ IN INITIAL PHYSICAL PARAMETERS
      call INITIALIZE(onepvdc)
      write (0,*) "Binning is ", delta_v, "km/s"
      
      blueline = 1
      usedold = 0
      used = 0

!     SETUP TO READ PROPER LINE,FILE
      isto = int(log(ea/900.0)/log(onepvdc)/256.0)
      ea = 900.*(onepvdc**256)**isto
      write (0,*) 'Calling continuum function'
      call BB(ea, eb, nlam, black, xplot, tbb)
      call THETA(ctheta, grid, jrlim)
 
      sfilenum = isto + 1
      ffilenum = int(log(eb/900.0)/log(onepvdc)/256.0)+1

      last = 0
      do i = 1, nlam
      	if (xplot(i) .lt. stspec) last=i
      enddo

!     READ IN NEXT BIN AND INITIALIZED OPTICAL DEPTHS
      do ifile = sfilenum, ffilenum
        call FILE(taux, gfx, chix, an, ai, ifile, filparm, numref)
        call GETBIN(elam, t, usedold, used, ifile, filparm, new, &
        		    onepvdc, notcalled)
        if (ifile .eq. sfilenum) then
        	do i = 1, used - 1
          		if (elam(i) .lt. ea) then
            		blueline = i + 1
            		thisline = i + 1
            		do j = 1, jrlim
             			t(i, j) = 0.0
             			td(i, j) = 0.0
            		enddo
          		endif
         	enddo
        endif
        
        do thisline = usedold + 1, used
           if (elam(thisline) .gt. eb) goto 555
!	       CALCULATE SOURCE FUNCTIONS FOR THIS LINE
           call SOURCE(elam, thisline, blueline, ctheta, black)
!          CALCULATE SPECTRUM
           if (elam(thisline) .gt. stspec) then
           		call SPECTRUM(elam, thisline, blueline, xplot, black, &
             				  jrlim, grid, last, vmax, vphot, flambda, &
             				  nlam, zeta, ea, eb)
           endif
 
        enddo
        if (used .gt. 767) then
            print *,'SHUFFLE ', thisline, blueline, used, usedold
            do is = blueline, used
            	do j = grid + 1, jrlim
                	t(is - blueline + 1, j) = t(is, j)
                	t(is, j) = 0.0
                	td(is - blueline + 1, j) = td(is, j)
                	s(is - blueline + 1, j) = s(is, j)
                	sd(is - blueline + 1, j) = sd(is, j)
              	enddo
              	elam(is - blueline + 1) = elam(is)
              	elam(is) = 1.1*eb
              	t(is, 4) = 0.0
            enddo
            do is = used - blueline + 2, blueline
            	do j = grid + 1, jrlim
                	t(is, j) = 0.0
              	enddo
            enddo
            used = used - blueline + 1
            thisline = thisline - blueline + 1
            blueline = 1 
        endif
        usedold = used
      enddo

!     CALCULATE LAST BIT OF SPECTRUM.
555   print *,'ALMOST DONE'
      do i = thisline, nwavestep
      	elam(i) = eb*10.0
      enddo
      call SPECTRUM(elam, thisline, blueline, xplot, black, jrlim, &
      				grid, last, vmax, vphot, flambda, nlam, zeta, ea, &
      				eb)

!                             END OF MAIN PROGRAM
      end

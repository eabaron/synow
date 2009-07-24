      subroutine INITIALIZE(onepvdc)
      
      use Global
      
      implicit none
      
      integer, parameter :: nradstep=350
      real(wp) onepvdc, hc, k, elamx(92, 0:5), gfx(92, 0:5), &
      		   chix(92, 0:5), taux(92, 0:5, nradstep), vphot, vmax, &
      		   tauxold, tbb, onex, ea, eb, wk, pwrlawin(50), xsto, &
      		   taumin, zeta, rmax, wzone, alam, agf, aelow, stspec, &
      		   tau1(50), vmine(50), vmaxe(50), ve(50), temp(50), &
      		   delta_v,sigma_v(50), vmaxg(50)
      character dprof(50)
      character*132 :: spectrum_file="fort.11"
      integer grid, jrlim, nlam, numref, i, j, anum, aion, ii, ij, ik, &
      		  ai(50), an(50), begin, stop
      logical :: flambda, debug_out=.true.
      
      include "param.inc"
      namelist /parms/ vphot, vmax, tbb, ea, eb, nlam, flambda, &
      				   taumin, grid, zeta, stspec, numref, delta_v, &
      				   an, ai, tau1, pwrlawin, vmine, vmaxe, ve, &
      				   vmaxg, sigma_v, temp, dprof, spectrum_file, debug_out

      hc = 12400._wp
      k = 8.6167d-5
      onex = 1.0_wp
      print *,'INITIALIZING'
      
!     READ IN REFERENCE LINE PARAMETERS
      open(unit = 1, file = 'ref.dat', status = 'old')
1     read(1, 3, end = 2) alam, agf, anum, aion, aelow
3     format(F12.4, 1X, F6.3, 1X, I2, 2X, I1, 1X, F12.8)
      elamx(anum, aion) =  alam*10.0_wp
      gfx(anum, aion)   =  agf*log(10._wp)
      chix(anum, aion)  =  aelow
      goto 1
2	continue
!     READ IN PHYSICAL PARAMETERS
!2     open(unit = 5, file = 'in.dat', status = 'old')
 
      read(*, parms)
      onepvdc = 1.0_wp + delta_v/3d5
      close (5)
      
	open(11, file=trim(spectrum_file), status='unknown')
        IF(.not. debug_out) then
           DO j=1,numref
              OPEN (j+60, status='scratch')
           ENDDO
        ELSE
           DO j=1,numref
              OPEN (j+60, status='unknown')
           ENDDO
        ENDIF

      do anum =1, 92
      	do aion = 0, 5
        	taux(anum, aion, 1) = 0.0_wp
        enddo
      enddo
       
      rmax = vmax/vphot
      tbb = tbb*k/hc
      jrlim = int(rmax*real(grid) + 0.5_wp)
      
!     NOW WE SET UP RADIAL PROFILES 
      do j = 1, numref
      	vmine(j) = 1000._wp*vmine(j)
        vmaxe(j) = 1000._wp*vmaxe(j)
        ve(j) = 1000._wp*ve(j)
        vmaxg(j) = 1000._wp*vmaxg(j)
        sigma_v(j) = 1000._wp*sigma_v(j)
        if (vmine(j) .le. vphot*onepvdc) then
        	begin = grid + 1
            wzone = 0.5_wp
            vmine(j) = vphot
        else
        	begin = int(1.5_wp + real(grid)*(vmine(j)/vphot))
            wzone = 0.5_wp*(1.0_wp - (1.0_wp - &
            		(vmine(j)/vphot)**(-2))**.5)
            begin = min(begin, jrlim)
        endif
        	stop = int(1.5_wp + real(grid)*(vmaxe(j)/vphot))
      		stop = min(stop, jrlim)
      		
!     Now we set up optical depth profiles.
!     REMEMBER that we have to anticorrect for stimulated emission.
        if (elamx(an(j), ai(j)) .lt. 1000._wp) then
        	print *,'NEED ref.dat line for ',an(j),ai(j)
        	stop
        endif
        wk = (exp(1._wp/tbb/elamx(an(j), ai(j))) - &
        	 1._wp)/wzone/zeta/zeta+1.
        wk = 1._wp - 1._wp/wk
        tauxold = taux(an(j), ai(j), 1)
        taux(an(j), ai(j), 1) = tau1(j)/wk
        taux(an(j), ai(j), 2) = temp(j)*1000._wp*k
        do i = begin, stop
        	if (dprof(j) .eq. 'p') then
            	xsto = max(onex, (vphot/vmine(j)))
            	taux(an(j), ai(j), i) = taux(an(j), ai(j), 1)&
             						  *(xsto*real(i - 1)/REAL(grid))&
             						  **(-pwrlawin(j))*(xsto*real(begin&
             						  - 1)/REAL(grid))**(pwrlawin(j))
            	write (j + 60, *) vphot*real(i - 1)/REAL(grid), &
            					  taux(an(j),ai(j),i), vmaxg(j)
            else if (dprof(j) .eq. 'g') then
            	taux(an(j), ai(j), i) = taux(an(j), ai(j), 1)&
             							*exp(-(vmaxg(j) - vphot&
             							*real(i - 1)/REAL(grid))**2&
             							/sigma_v(j)**2/2.0) 
            	write (j + 60, *) vphot*real(i - 1)/REAL(grid), &
            					  taux(an(j), ai(j), i), vmaxg(j)
           else if (dprof(j) .eq. 'e') then
           		taux(an(j), ai(j), i) = taux(an(j), ai(j), 1)&
             						    *exp((vmine(j) - vphot&
             						    *real(i - 1)/REAL(grid))/ve(j))
             	write (j + 60, *) vphot*real(i - 1)/REAL(grid), &
             					  taux(an(j), ai(j), i), vmaxg(j)
           else
           		write (0, *) 'invalid tau profile'
           		stop
           endif
        enddo
        	taux(an(j), ai(j), 1) = tauxold + taux(an(j), ai(j), 1)
      enddo
      
!     this part close packs the species in use
      ii = 1
      do ij = 1, numref
      	if (tau1(ij) .gt. 0.0001_wp*taumin) then
        	do ik = 1,ii - 1
            	if (ai(ik) .eq. ai(ij) .and. an(ik) .eq. an(ij)) then
            	goto 200
            	endif
            enddo
            ai(ii) = ai(ij)
            an(ii) = an(ij)
            tau1(ij) = 0.0
            ii = ii + 1
        endif
 
 
!     put this next line here to avoid compile warnings about jumping to
!     the end of the construct, instead of just using "200 enddo"
200  	continue
      enddo

	  numref = ii - 1
   
      print *, 'INITIALIZATION COMPLETE FOR', numref, 'SPECIES'  
      return
      end

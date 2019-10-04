      subroutine GETBIN(wavelength, t, usedold, used, i, filparm, new, &
      				    onepvdc, notcalled)
      use LteLineListClass
      use KuruczCtrl, only: synow_lines_path,kurucz_linelist_path
      use Global
      
      implicit none
      
      integer, parameter :: nradstep=10000,nwavestep=1024
      integer, parameter :: kUnit = 100000
      integer, parameter :: kcUnit = 100001
      real(wp), parameter :: logGfMin  =   -3.0_wp      
      real(wp) wavelength(nwavestep), t(nwavestep, nradstep), wk, &
      		   elamx(92, 0:5), gfx(92, 0:5), chix(92, 0:5), &
      		   taux(92, 0:5, nradstep), vphot, vmax, tbb, ea, eb, &
      		   taumin, zeta, chi, gfl, WL, WLN, dw, smelly, smelly2, &
      		   onepvdc, delta_v, filparm(50,5), stspec, beginw, endw, &
      		   lambdaMin, lambdaMax
      integer ik, ij, k, ii, used, usedold, i, icbyt, icbit, ipos1, &
      		  ipos2, an(50), ai(50), numref, isto1, isto2, isto3, &
      		  isto4, isto5, num, ion, grid, jrlim, nlam, rad, j
      logical flambda, notcalled, new
      character*80 filename
      integer*4 IWL, IEL, IGF, IIII, IBUF, IIIII

      type(LteLineListType) :: tLte

      include "param.inc"

!     First let's calculate the beginning and ending wavelengths
      lambdaMin = 900._wp*onepvdc**real(256*(i - 1))
      lambdaMax = 900._wp*onepvdc**real(256*(i))

      do j = 1, numref

!	  	Call destructor first just to be safe.
	  	call LteLineList(tLte)

!	  	Now this returns the line list!
!--
!-- this for Eddie running on his Macs
!--
!      	call initLteLineList('/net/linmer/myhome2/synow_lines/', &
!      						 '/net/linmer/myhome2/lines/', .false.)
!--
!-- this for general use on the login machines
!--
      	call initLteLineList(synow_lines_path,kurucz_linelist_path, &
         .false.)
!                             Rollin's cd1_f90.x.bin files cd1_f90.bin
	  	call LteLineList(tLte, kUnit, kcUnit, lambdamin, lambdamax)
      	call ionCodeInsert(tLte, an(j)*100 + ai(j))

      	write (0, *) "Number of lines considered for element", an(j), &
      				ai(j), "is", tLte%numline
      	write (0, *) "for begin wave", lambdamin, "end wave", lambdamax

!     	Loop over the number of lines.
      	do k = 1, tLte%numline

!     		First get wavelength of line 
      		WL = tLte%line(k)%lambda

!     		Now put it into the proper bin
      		IWL = int(log10(real(WL)/900.0)/log10(onepvdc) - &
      			  real(i - 1)*256.0) 
      		DW = 900.*onepvdc**real(256*(i - 1) + IWL + 1) - 900.&
      	   		 *onepvdc**real(256*(i - 1) + IWL)
      		WLN = WL + DW/2.0
      		IWL = int(log10(real(WLN)/900.0)/log10(onepvdc) - &
      			  real(i - 1)*256.0) 

!     		Calculate gfl using the reference line gf in filparm(j, 1)
      		gfl = log(tLte%line(k)%gf) + filparm(j, 1)

!     		Do the same with the chi
      		chi = -tLte%line(k)%chi*filparm(j, 4) + filparm(j, 3)

      		wavelength(IWL + usedold + 1) = 900.*onepvdc**real(256&
      									    *(i - 1) + IWL)
      		wk = exp(gfl + chi)*wavelength(IWL + usedold + 1)&
      	   		 /elamx(an(j), ai(j))
      		smelly = (gfl + gfx(an(j), ai(j)))/log(10.0)
      		smelly2 = gfx(an(j), ai(j))/log(10.0)

      		if (wk .gt. 1.0e-10) then
      			if ((wk .gt. 1e-1) .and. &
      				(wavelength(IWL + usedold + 1) .gt. 5160.0) .and. &
      				(wavelength(IWL + usedold + 1) .lt. 5190.0)) then
        				write (0, *) wavelength(IWL + usedold + 1), &
        							 smelly, smelly2, wk
        		endif
      		endif

      		if (wk .gt. 1.0e-10) then
      			do rad = grid + 1, jrlim
        			t(IWL + usedold + 1, rad) = t(IWL + usedold + 1, &
        			rad) + wk*taux(an(j), ai(j), rad)
        		enddo
      		endif

      	enddo
2     enddo

      call LteLineList(tLte)

      ii = usedold

      do ik = usedold + 1, usedold + 256
      	if (wavelength(ik) .lt. ea) goto 4
        	do j = grid + 1, jrlim     
          		if (t(ik,j) .gt. taumin) then
               		t(ik, 4) = 1.0
               		goto 3
        		endif
      		enddo
         
3     		if (t(ik, 4) .gt. 0.5) then
      			ii = ii + 1
          		if (ii .ne. ik) then
          			wavelength(ii) = wavelength(ik)
            		wavelength(ik) = 0.0
            		t(ii, 4) = 1.0
            		t(ik, 4) = 0.0
            		do ij = grid + 1, jrlim
            			t(ii, ij) = t(ik, ij)
            			t(ik, ij) = 0.0
            		enddo  
          		endif
        endif
!	  put this next line here to avoid compile warnings about jumping to
!     the end of the construct, instead of just using "4 enddo"
4     	continue
      enddo
      
      used = ii
      
      return
      end

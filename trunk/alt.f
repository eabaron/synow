      subroutine BB(ea, eb, nlam, black, xplot, tbb)
      
      use Global
      
      implicit none
      
      real(wp) ea, eb, black(10000), xplot(10000), tbb, xsto
      integer i, nlam
      
      xsto = ea + eb
      
      do i = 1, nlam
      	xplot(i) = ea*(eb/ea)**(real(i - 1)/real(nlam - 1))
        black(i) = (xsto/xplot(i))**3* &
                   (exp(1./tbb/xsto) - 1.)/(exp(1./tbb/xplot(i)) - 1.)
      enddo
      
      return
      end

      subroutine PL(ea, eb, nlam, black, xplot, tbb)

	  use Global
      
      implicit none
      
      real(wp) ea, eb, black(10000), xplot(10000), tbb, xsto
      integer i, nlam
      
      xsto = ea + eb
      
      do i = 1, nlam
      	xplot(i) = ea*(eb/ea)**(real(i - 1)/real(nlam - 1))
        black(i) = (xsto/xplot(i))**(-tbb)*(xsto/xplot(i))**(2)
        write (50, *) xplot(i), black(i)
      enddo
      
      return
      end

      subroutine theta(ctheta, grid, jrlim)
      
      use Global
      
      implicit none
      
      integer, parameter :: nradstep=500
      real(wp) ctheta(nradstep, 21), crit, pi
      integer jrlim, grid, rad, step
      
      pi = acos(-1.)
      rad = grid + 1
      
      do step = 1, 10
      	crit = 0.5*pi
        ctheta(rad, 21) = 0.5
        ctheta(rad, step) = 1.
        ctheta(rad, step + 10) = 0.05 - 0.1*real(step) 
      enddo
      
      do rad = grid + 2, jrlim
      	crit = sqrt(1. - (real(grid)/real(rad - 1))**2)
        ctheta(rad, 21) = 0.5*(1. - sqrt(1. - &
        				  (real(grid)/real(rad-1))**2))
        do step = 1, 10
        	ctheta(rad, step) = crit + (1. - crit)&
        						*(2*REAL(step) - 1.)/20.
            ctheta(rad, step + 10) = -1. + (1. + crit)&
            						 *(2*REAL(step) - 1.)/20.
        enddo
      enddo
      
      return
      end
 
      subroutine FILE(taux, gfx, chix, an, ai, i, filparm, numref)
      
      use Global
      
      implicit none
      
      integer, parameter :: nradstep=500  
      real(wp) taux(92, 0:5, nradstep), gfx(92, 0:5), chix(92, 0:5), &
      		   tex, filparm(50, 5)
      integer an(50), ai(50), i, isto1, isto2, isto3, isto4, isto5, j, &
      		  numref

      isto4 = 0
      
      do j = 1, numref
      	filparm(j, 5) = 0.0
      enddo

      do j = 1, numref
      	filparm(j, 1) = 0.0
        filparm(j, 2) = 0.0
        filparm(j, 3) = 0.0
        filparm(j, 4) = 0.0
        filparm(j, 5) = 0.0
        filparm(j, 1) = -gfx(an(j), ai(j)) 
        tex = taux(an(j), ai(j), 2)
        filparm(j, 3) = (chix(an(j), ai(j)))/tex
        filparm(j, 4) = 1.0/tex
      enddo
      
      return
      end
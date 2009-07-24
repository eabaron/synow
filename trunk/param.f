!*********************************************************************
! Filename:      param.f
! Author:        Eddie Baron <baron@ou.edu>
! Created at:    Wed Jun 10 10:47:13 2009
! Modified at:   Wed Jun 10 10:57:59 2009
! Modified by:   Eddie Baron <baron@ou.edu>
! Version:       
! Description:   module replacement for old common /param/
!*********************************************************************
      module param_module
      use Global
      implicit none
      logical :: flambda
      integer, parameter :: nradstep=500, nwavestep = 1024
      integer :: grid, jrlim, nlam, numref, an(50), ai(50)
      real(wp) :: elamx(92, 0:5), gfx(92, 0:5), chix(92, 0:5), &
      taux(92, 0:5, nradstep), vphot, vmax, tbb, ea, eb, &
      taumin, zeta, stspec, delta_v
      end module param_module

                      


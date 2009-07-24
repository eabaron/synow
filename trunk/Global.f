!     File    : Global.f
!     ------------------
!     Created : Mon May 19 15:37:00 CDT 2003
!     Authors : Rollin C. Thomas (RCT) - rcthomas@lbl.gov
!     Purpose : Global options set at compile time.

      module Global

      implicit none
      private

!     Type kind definitions.  Make sure compiler settings do not obviate
!     this.
      
      integer, parameter, public :: sp = kind(1.0)
      integer, parameter, public :: dp = kind(1.0d0)
      integer, parameter, public :: wp = dp

!     Floating point comparison.  Checking against equality is frowned
!     upon. Check for them to be in the neighborhood.
      
      real(sp), parameter, public :: SPEPS = epsilon(1.0)
      real(dp), parameter, public :: DPEPS = epsilon(1.0d0)
      real(wp), parameter, public :: WPEPS = epsilon(1.0_wp)

!     Standard file units.  Depends on the machine!  You might not want
!     to use them at all.
      
      integer, parameter, public :: STDERR = 0
      integer, parameter, public :: STDIN  = 5
      integer, parameter, public :: STDOUT = 6

      end module Global

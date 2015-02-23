!     File    : Const.f
!     -----------------
!     Created : Mon May 19 15:36:44 CDT 2003
!     Authors : Rollin C. Thomas (RCT) - rcthomas@lbl.gov
!     Purpose : General constants, physical, mathematical and
!               astronomical

      module Const

      use Global

      implicit none
      private

!     Basic physics, astromomy, and math constants.  Default system is
!     the CGS system.

!     Pi
      real(wp), parameter, public :: piConst   = 3.141592653589793_wp 
!     Planck's constant
      real(wp), parameter, public :: hConst    = 6.62619e-27_wp       
!     Speed of light
      real(wp), parameter, public :: cConst    = 2.997924562e+10_wp 
!     Boltzmann's constant
      real(wp), parameter, public :: kConst    = 1.380658e-16_wp      
!     Avogadro's number
      real(wp), parameter, public :: naConst   = 6.022045e23_wp       
!     Thompson cross section 
      real(wp), parameter, public :: thConst   = 0.665e-24_wp         
!     Solar mass in grams
      real(wp), parameter, public :: mSunConst = 1.9891e33_wp         

!     Conversion factors.  To apply a conversion, you multiply the
!     quantity of interest by the factor listed.  To reverse it, use
!     division.

      real(wp), parameter, public :: ev2ergConst  = 1.602e-12_wp
      real(wp), parameter, public :: km2cmConst   = 1.0e5_wp
      real(wp), parameter, public :: day2secConst = 86400.0_wp
      real(wp), parameter, public :: pc2cmConst   = 3.08568025e18_wp

!     Compound constants.  These constants are combinations of the above
!     ones or of each other.

      real(wp), parameter, public :: hcConstEV  = hConst * cConst &
                                                  / ev2ergConst
      real(wp), parameter, public :: kConstEV   = kConst / ev2ergConst
      real(wp), parameter, public :: cConstKMPS = cConst / km2cmConst
      real(wp), parameter, public :: ksiConst   = 1.0e08_wp &
                                                  * ( hConst * cConst &
                                                  / kConst )
      real(wp), parameter, public :: twoPiConst = 2.0_wp * piConst
      real(wp), parameter, public :: optConst   = day2secConst &
                                                  * 1.0e-8_wp &
                                                  * 2.653e-2_wp

      end module Const
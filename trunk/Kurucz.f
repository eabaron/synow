!	  File    : Kurucz.f
!	  ------------------
!	  Created : Sun Jul  4 21:08:46 2004
!	  Authors : Rollin C. Thomas (RCT) - rcthomas@lbl.gov
!	  Purpose : Interface to Kurucz linelist files.

      module Kurucz

      use Global
      use Inform
      use Const

      implicit none
      private

      public :: initKurucz
      public :: kuruczRecord
      public :: kuruczList

!	  Mapping from Kurucz's element code to ionization states.
      integer, parameter :: kIonState( 924 ) = (/ &
      01, 02, 03, 01, 02, 03, 04, 01, 02, 03, 04, 05, 01, 02, 05, 01, & 
      02, 03, 01, 02, 03, 01, 02, 03, 01, 02, 03, 05, 01, 02, 03, 04, & 
      01, 02, 01, 02, 01, 02, 03, 01, 02, 03, 04, 01, 02, 03, 04, 01, & 
      02, 03, 04, 01, 02, 03, 04, 01, 02, 01, 02, 03, 01, 02, 01, 00, & 
      00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, & 
      00, 00, 00, 00, 00, 01, 00, 01, 02, 00, 01, 02, 03, 00, 01, 02, & 
      03, 04, 00, 01, 02, 03, 04, 05, 00, 01, 02, 03, 04, 05, 06, 00, & 
      01, 02, 03, 04, 05, 06, 07, 00, 01, 02, 03, 04, 05, 06, 07, 08, & 
      00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 00, 01, 02, 03, 04, 05, & 
      06, 07, 08, 09, 10, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, & 
      11, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 00, 01, & 
      02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 00, 01, 02, 03, & 
      04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 00, 01, 02, 03, 04, & 
      05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 00, 01, 02, 03, 04, & 
      05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 00, 01, 02, 03, & 
      04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 00, 01, & 
      02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, & 
      18, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, & 
      15, 16, 17, 18, 19, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, & 
      11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 00, 01, 02, 03, 04, 05, & 
      06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, & 
      00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, & 
      16, 17, 18, 19, 20, 21, 22, 00, 01, 02, 03, 04, 05, 06, 07, 08, & 
      09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 00, & 
      01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, & 
      17, 18, 19, 20, 21, 22, 23, 24, 00, 01, 02, 03, 04, 05, 06, 07, & 
      08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, & 
      24, 25, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, & 
      14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 00, 01, 02, & 
      03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, & 
      19, 20, 21, 22, 23, 24, 25, 26, 27, 00, 01, 02, 03, 04, 05, 06, & 
      07, 08, 09, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, & 
      23, 24, 25, 26, 27, 28, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, & 
      10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, & 
      26, 27, 28, 29, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, & 
      12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, & 
      28, 29, 30, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, & 
      03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, & 
      04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, & 
      00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, & 
      01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, & 
      02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, & 
      03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, & 
      04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, & 
      00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, & 
      01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, & 
      02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, & 
      03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, & 
      04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, & 
      00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, & 
      01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, & 
      02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, & 
      03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, & 
      04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, & 
      00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, & 
      01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, & 
      02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04, 00, 01, 02, & 
      03, 04, 00, 01, 02, 03, 04, 00, 01, 02, 03, 04 /)

!	  Mapping from Kurucz's element code to atomic number.
      integer, parameter :: kAtomNum( 924 ) = (/                                                                                  &
      06, 06, 06, 07, 07, 07, 07, 08, 08, 08, 08, 08, 11, 11, 11, 12, & 
      12, 12, 13, 13, 13, 14, 14, 14, 16, 16, 16, 16, 19, 19, 19, 19, & 
      20, 20, 21, 21, 22, 22, 22, 23, 23, 23, 23, 24, 24, 24, 24, 25, & 
      25, 25, 25, 26, 26, 26, 26, 27, 27, 28, 28, 28, 29, 29, 56, 01, & 
      06, 07, 08, 11, 12, 13, 14, 16, 19, 20, 21, 22, 23, 24, 25, 26, & 
      27, 28, 29, 56, 01, 01, 02, 02, 02, 03, 03, 03, 03, 04, 04, 04, & 
      04, 04, 05, 05, 05, 05, 05, 05, 06, 06, 06, 06, 06, 06, 06, 07, & 
      07, 07, 07, 07, 07, 07, 07, 08, 08, 08, 08, 08, 08, 08, 08, 08, & 
      09, 09, 09, 09, 09, 09, 09, 09, 09, 09, 10, 10, 10, 10, 10, 10, & 
      10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, & 
      11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, & 
      13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, & 
      14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, & 
      15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, & 
      16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, & 
      17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, & 
      18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, & 
      18, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, & 
      19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, & 
      20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, & 
      21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, & 
      22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, & 
      22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, & 
      23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, & 
      24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, & 
      24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, & 
      25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, & 
      25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, & 
      26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, & 
      27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, & 
      27, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, & 
      28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, & 
      28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, & 
      29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, & 
      29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, & 
      30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, & 
      30, 30, 30, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 33, 33, 33, & 
      33, 33, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 36, 36, 36, 36, & 
      36, 37, 37, 37, 37, 37, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, & 
      40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42, 43, & 
      43, 43, 43, 43, 44, 44, 44, 44, 44, 45, 45, 45, 45, 45, 46, 46, & 
      46, 46, 46, 47, 47, 47, 47, 47, 48, 48, 48, 48, 48, 49, 49, 49, & 
      49, 49, 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, 52, 52, 52, 52, & 
      52, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, 55, 55, 55, 55, 55, & 
      56, 56, 56, 56, 56, 57, 57, 57, 57, 57, 58, 58, 58, 58, 58, 59, & 
      59, 59, 59, 59, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61, 62, 62, & 
      62, 62, 62, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64, 65, 65, 65, & 
      65, 65, 66, 66, 66, 66, 66, 67, 67, 67, 67, 67, 68, 68, 68, 68, & 
      68, 69, 69, 69, 69, 69, 70, 70, 70, 70, 70, 71, 71, 71, 71, 71, & 
      72, 72, 72, 72, 72, 73, 73, 73, 73, 73, 74, 74, 74, 74, 74, 75, & 
      75, 75, 75, 75, 76, 76, 76, 76, 76, 77, 77, 77, 77, 77, 78, 78, & 
      78, 78, 78, 79, 79, 79, 79, 79, 80, 80, 80, 80, 80, 81, 81, 81, & 
      81, 81, 82, 82, 82, 82, 82, 83, 83, 83, 83, 83, 84, 84, 84, 84, & 
      84, 85, 85, 85, 85, 85, 86, 86, 86, 86, 86, 87, 87, 87, 87, 87, & 
      88, 88, 88, 88, 88, 89, 89, 89, 89, 89, 90, 90, 90, 90, 90, 91, & 
      91, 91, 91, 91, 92, 92, 92, 92, 92, 93, 93, 93, 93, 93, 94, 94, & 
      94, 94, 94, 95, 95, 95, 95, 95, 96, 96, 96, 96, 96, 97, 97, 97, & 
      97, 97, 98, 98, 98, 98, 98, 99, 99, 99, 99, 99 /)
      
!	  Maximum and minimum records available in the file.
      integer, parameter :: firstRecord = 2
      integer, parameter :: finalRecord = 41975808

!	  Local variables.
      character(len = 256), save :: path = './'
      logical             , save :: nuxi = .false.
      logical             , save :: init = .false.
      real(wp)            , save :: rlog = 0.0_wp
      real(wp)            , save :: ltth = 0.0_wp

      contains

!	  initKurucz :
      subroutine initKurucz(kPath, kNuxi)
      character(len = *), intent(in) :: kPath
      logical           , intent(in) :: kNuxi

!	  FIX  Make sure the path is not too long.
      path = adjustl(kPath)
      nuxi = kNuxi
        
!	  FIX  Make sure the file exists.
      init = .true.
      rlog = log(1.0_wp + 1.0_wp / 2000000.0_wp)
      ltth = 0.001_wp * log(10.0_wp)

      end subroutine initKurucz

!     kuruczFileExists : Makes sure the Kurucz linelist file exists.
      logical function kuruczFileExists()
      inquire(file = trim(path) // 'cd1_f90.bin', &
      		  exist = kuruczFileExists) 
      end function kuruczFileExists
      
!     kuruczRecord : Given a wavelength, this method binary searches 
!     through the Kurucz file cd1_f90.bin for the last record with 
!     wavelength less than or equal to the wavelength given.  The 
!     wavelength passed in is assumed to be in Angstroms.
      integer function kuruczRecord(kUnit, lambda)

      integer , intent(in) :: kUnit
      real(wp), intent(in) :: lambda
      
      integer  :: jl, jm, ju, ibuf(2), jbuf(2)
      real(wp) :: kLambda, firstLambda, finalLambda
      real(wp) :: lbuf(2), gbuf(2), cbuf(2)

      integer(selected_int_kind(1)) :: buff(16)
      integer(selected_int_kind(3)) :: ielm, ielo, ilgf, igra, igsa, &
      								   igwa
      integer(selected_int_kind(5)) :: iwln

      ibuf = (/ firstRecord, finalRecord /)
      call kuruczList(kUnit, ibuf, jbuf, lbuf, gbuf, cbuf)
      firstLambda = lbuf(1)
      finalLambda = lbuf(2)

!      write(*,*)"opening cd1_f90.bin",trim(path)
      open(unit = kUnit, file = trim(path) // "cd1_f90.bin", &
           form = 'unformatted', access = 'direct', recl = 16, &
           status = 'old')

      jl = 0
      ju = finalRecord - firstRecord + 2
      do while(ju - jl .gt. 1)
      	jm = (ju + jl) / 2
        if (nuxi) then
        	read (kUnit, rec = jm + firstRecord - 1) buff
          	iwln = transfer(buff(4 : 1 : - 1), iwln) 
        else
        	read (kUnit, rec = jm + firstRecord - 1) iwln, ielm, ielo, &
        		  ilgf, igra, igsa, igwa
        end if
        kLambda = 10.0_wp * exp(iwln * rlog)
        if(lambda .ge. kLambda) then
        	jl = jm
        else
        	ju = jm
        end if
      end do
      
      if (lambda .eq. firstLambda) then
      	kuruczRecord = firstRecord
      else if (lambda .eq. finalLambda) then
      	kuruczRecord = finalRecord - 1
      else
      	kuruczRecord = jl + firstRecord - 1
      end if

      close(unit = kUnit)

      end function kuruczRecord

!	  kuruczList : Returns Kurucz linelist parameters.
      subroutine kuruczList(kUnit, ident, ionCode, lambda, gf, chi)

      integer, intent(in) :: kUnit
      integer, intent(in out) :: ident(:), ionCode(:)
      real(wp), intent(in out) :: lambda(:), gf(:), chi(:)

      integer :: nr, ir

      integer(selected_int_kind(1)) :: buff(16)
      integer(selected_int_kind(3)) :: ielm, ielo, ilgf, igra, igsa, & 
      								   igwa
      integer(selected_int_kind(5)) :: ilam

      logical :: first = .true.
      character*80 :: cd1

      nr = size(ident)
      
!     Check to make sure all the arrays are the same size.
!     Something about checking if the numbers are legal.
      if (first) then
	  	first = .false.
      	cd1 = trim(path) // "cd1_f90.bin"
        write (*, *) "kurucz file: ", kuruczfileexists(), trim(cd1), &
        			 kUnit
        endif
      
	  open(unit = kUnit, file = trim(path) // "cd1_f90.bin", &
           form = 'unformatted', access = 'direct', recl = 16, &
           status = 'old')

      do ir = 1, nr
	  	if (nuxi) then
        	read(kUnit, rec = ident(ir)) buff
        	ilam = transfer(buff(4 : 1 : - 1), ilam) 
        	ielm = transfer(buff(6 : 5 : - 1), ielm)
        	ielo = transfer(buff(8 : 7 : - 1), ielo)
        	ilgf = transfer(buff(10 : 9 : - 1), ilgf)
        else
        	read (kUnit, rec = ident(ir)) ilam, ielm, ielo, ilgf, &
        		  igra, igsa, igwa
      	end if
      
      	ielm = abs(ielm)
      	ionCode(ir) = kIonState(ielm) + 100 * kAtomNum(ielm)
      	lambda(ir) = 10.0_wp * exp(ilam * rlog)            !- Angstroms.
      	gf(ir) = exp(ltth * real(ilgf - 16384, wp))      !- Log10( gf ).
      	chi(ir) = exp(ltth * real(ielo - 16384, wp)) * hcConstEV  !- EV.
      end do

      close(kUnit)

      end subroutine kuruczList

      end module Kurucz

!	  $Log: Kurucz.f,v $
!	  Revision 1.3  2004/07/22 23:51:42  rthomas
!	  Added headers and logs to everything and corrected the stupid
!	  "Changed" comment thing to "Created."

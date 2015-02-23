!	  File    : Inform.f
!	  ------------------
!	  Created : Mon May 19 15:37:14 CDT 2003
!	  Authors : Rollin C. Thomas (RCT) - rcthomas@lbl.gov
!	  Purpose : Informational messages sent as warnings or croak
!	  messages.

      module Inform

      use Global

      implicit none
      private

      public :: info
      public :: warn
      public :: die

      logical, public, save :: suppressInformation = .false.
      logical, public, save :: suppressWarnings    = .false.

      contains

!     info : Send an informational message to standard error.
      subroutine info(from, mesg)

      character(len = *), intent(in) :: from
      character(len = *), intent(in) :: mesg

      if (suppressInformation) return

      write(STDERR, '( " --"         )')
      write(STDERR, '( " -- INFORMATION ", A19 )') dateTimeStr()
      write(STDERR, '( " -- ", A     )') trim(adjustl(from))
      write(STDERR, '( " -- ", A     )') trim(adjustl(mesg))
      write(STDERR, '( " --"         )')
        
      end subroutine info

!	  warn : Send a message to standard error but continue execution.
      subroutine warn(from, mesg)

      character(len = *), intent(in) :: from
      character(len = *), intent(in) :: mesg

      if(suppressWarnings) return

      write(STDERR, '( " --"         )')
      write(STDERR, '( " -- WARNING ", A19 )') dateTimeStr()
      write(STDERR, '( " -- ", A     )') trim(adjustl(from))
      write(STDERR, '( " -- ", A     )') trim(adjustl(mesg))
      write(STDERR, '( " --"         )')

      end subroutine warn

!	  die : Send a message to standard error and halt execution.
      subroutine die(from, mesg)

      character(len = *), intent(in) :: from
      character(len = *), intent(in) :: mesg

      write(STDERR, '( " --"       )')
      write(STDERR, '( " -- FATAL ", A19 )') dateTimeStr()
      write(STDERR, '( " -- ", A   )') trim(adjustl(from))
      write(STDERR, '( " -- ", A   )') trim(adjustl(mesg))
      write(STDERR, '( " --"       )')
      stop

      end subroutine die

!	  dateTimeStr : Returns an RFC 8601 date and time string.

      character(len = 19) function dateTimeStr()

      character(len = 8)  :: tdat
      character(len = 10) :: ttim
      character(len = 5)  :: tzon
      character(len = 10) :: dstr
      character(len = 8)  :: tstr

      call date_and_time(tdat, ttim, tzon)
      dstr = tdat(1:4) // '-' // tdat(5:6) // '-' // tdat(7:8)
      tstr = ttim(1:2) // ':' // ttim(3:4) // ':' // ttim(5:6)
      dateTimeStr = dstr // 'T' // tstr

      end function dateTimeStr

      end module Inform

!	  $Log: Inform.f,v $
!	  Revision 1.2  2004/07/22 23:51:42  rthomas
!	  Added headers and logs to everything and corrected the stupid
!	  "Changed" comment thing to "Created."
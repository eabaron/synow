!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! Copyright D.J. Jeffery, 2006jan01.
!
!  locnorm.f is a stand-alone subroutine (and package) for doing local normalization of a spectrum file.
!
!  Note that spectra created with this subroutine will not be quite the same as with speclist.f's difmaker, 
!  because that does an additional small scale smoothing.
!
!   References:
!
! \bibitem[Jeffery et al.(2006)]{jeffery2006} Jeffery, D. J., Ketchum, W., 
!           Branch, D., Baron, E., 
!           Elmhamdi, A., \&~Danziger, I. J.,2006, ApJS, submitted, 
!           astro-ph/0607084
!           % Local normalization and DIFF1/2.
!
!     \bibitem[Metcalf et al.(2004)]{metcalf2004} Metcalf, M., Reid, J., 
!          \&~Cohen, M. 2004, Fortran 95/2003 Explained
!          (Oxford:  Oxford University Press) (MRC)
!          % Pretty good reference book.
!
!     \bibitem[Press et al.(1992)]{press1992} Press, W. H., Teukolsky, S. A.,
!          Vetterling, W. T., \& Flannery, B. P. 1992,
!            Numerical Recipes in Fortran
!            (Cambridge:  Cambridge University Press)
!            % Bill should have given me a complimentary copy when
!            % I co-taught with him in 1993!
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! See MRC-153,192 and the example /aalib/testnamelist.f.
!
      module numerical2_mod
      implicit none
!
      integer, parameter :: nsngl=kind(0.0)   ! Determines the index number for single precision.
      integer, parameter :: ndble=kind(0.d0)  ! Determines the index number for double precision.
      integer, parameter :: nprecision=ndble  ! Assigns the common precision.  
!                                             ! Since nothing in the locnorm package
!                                             !   here interacts directly with the rest of synow, 
!                                             !   we can use double precision. 
!                                             !   The interpolation subroutine really 
!                                             !   needs double precision. 
!
      end module numerical2_mod
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! See MRC-153,192 and the example /aalib/testnamelist.f.
!
      module locnorm_namelist_mod
      use numerical2_mod
      implicit none
!
      real (kind=nprecision) :: aafiction                 ! An unused variable that just holds the first place in the namelist. 
!
      character :: afile1*180='fort.11'                   ! Name of the input spectrum file which must be wavelength 
!                                                         !  and flux in 2-column format.
!                                                         !  There can be a header ending with 'END:'.
      character :: afile2*180='fort.12'                   ! Name of the output locally-normalized spectrum file 
!                                                         ! which will retain the header.
!
      integer :: icopy=0                                  ! icopy=1 copes the header from afile1 to afile2.  
!                                                         !   icopy=0 means don't copy the header. 
!                                                         ! A header allways ends in `END:', but no header is required.
      integer :: igrid=0                                  ! Interpolate to a logarithmic wavelength grid.
      real(kind=nprecision) :: grid=1.d0/3000.d0          ! The standard natural logarithmic grid interpolation.  
!                                                         !   Must convert to base 10 grid.
      integer :: imode=-4                                 ! imode=+/- 1 gives ordinary smoothing.
!                                                         ! imode=+/- 2 gives smoothing with logarithmic x. 
!                                                         ! imode=+/-3 gives smoothing with logarithmic y. 
!                                                         ! imode=+/-4 gives smoothing with logarithmic x and y. 
!                                                         ! imode > 0 gives box-car smoothing with trapezoid integration.
!                                                         ! imode < 0 gives box-car smoothing with a least-squares fit to a line. 
!                                                         ! I like imode=-4 for logarithmic plotting, 
!                                                         !   but imode=-1 would be almost the same I guess. 
!
      real (kind=nprecision) :: x_relative=.15d0          ! The relative half-smoothing length for the local normalization.
!                                                         !   For example, .15d0, locally-normalizes by 15 % 
!                                                         !   in wavelength redward and blueward of
!                                                         !   any wavelength point. 
!
      namelist/locnorm_namelist/aafiction                                          &
     & , afile1                                                                    &
     & , afile2                                                                    &
     & , icopy                                                                     &
     & , igrid                                                                     &
     & , grid                                                                      &
     & , imode                                                                     &
     & , x_relative                                                                &
     &                                                    ! End of the namelist
!
      end module locnorm_namelist_mod
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
!  Subroutine locnorm.
!
!  This is where the real work is done
!
      subroutine locnorm(ifile,afile) 
      use numerical2_mod
      use locnorm_namelist_mod
      implicit none
!
      integer, parameter :: nx=10000
!
      character, intent(in) :: afile*(*)   ! File with the parameters.
      integer, intent(in) :: ifile    ! 0 for the default values;  1 for use the input file.
!                                     !   -1 for use in SYNOW's in.dat file.
!
      integer :: i,j,k,l,m,n
      integer :: iend
      integer :: ismooth=1
      integer :: istart
      integer :: itmp
      integer :: ix
      integer :: iunit_in = 11, iunit_out=12
      logical :: logical_open
      real (kind=nprecision) :: smooth_xrel(1)
      real (kind=nprecision) :: smooth_zone(1)=1.d30      !  This should extend well beyond any wavelength range.
      real (kind=nprecision), allocatable :: x(:)
      real (kind=nprecision) :: xx
      real (kind=nprecision), allocatable :: y(:)
      real (kind=nprecision) :: yy
      real (kind=nprecision), allocatable :: y2(:)
!
!-----------------------------------------------------------------------
      write(*,*)
      write(*,*) 'In locnorm.f where the locally-normalized spectrum '
      write(*,*) 'is made and placed in fort.12/output file.'
      write(*,*) 'Before reading fort.11/input file.'
!-----------------------------------------------------------------------
!
!      if(ifile .eq. 1) then
!          open(unit=31,file=afile,status='old',action='read') ! 31 seems unlikely to cause
!            read(31,locnorm_namelist)                          !   any interference.
!            write(*,*)
!            write(*,locnorm_namelist)
!          close(unit=31)
!        else if(ifile .eq. -1) then
!!          write(*,*) 'What the heck.'
!          inquire(file='in.dat',opened=logical_open)
!          if(.not.logical_open) open(unit=5,file='in.dat',                  &
!     &                               status='old',action='read')
!          rewind 5
!          read(5,locnorm_namelist,err=110)   ! SYNOW opens `in.dat' as unit=5 and
!  110     continue                           !   never closes it it seems.
!          write(*,locnorm_namelist)
!          if(.not.logical_open) close(unit=5)
!      end if
!
      smooth_xrel(1)=x_relative
!
!      open(unit=1,file=afile1,status='old',action='read')
!      open(unit=2,file=afile2,status='unknown',action='write')
      call readcopy(iunit_in,iunit_out,icopy,' ',ix) 
!
      if(.not.allocated(x)) allocate(x(ix))
      if(.not.allocated(y)) allocate(y(ix))
      if(.not.allocated(y2)) allocate(y2(ix))
!
      do420 : do i=1,ix
        read(iunit_in,*) x(i),y(i)
!        write(*,*) x(i),y(i)
      end do do420 
!
!-----------------------------------------------------------------------
      write(*,*)
      write(*,*) '1,x(1),y(1)'
      write(*,*) 1,x(1),y(1)
      write(*,*) 'ix,x(ix),y(ix)'
      write(*,*) ix,x(ix),y(ix)
!-----------------------------------------------------------------------
!
      if(abs(imode) .ge. 3) y(:)=max(abs(y(:)),1.d-50)   ! I assume negative noise is the same size as postive noise and
!                                                        ! 1.d-50 is far below the flux size in any units. 
!
      y2(:)=y(:)
      call smooth(imode,ix,x,y2,ismooth,smooth_xrel,smooth_zone)  
      y(:)=y(:)/y2(:)
!
!-----------------------------------------------------------------------
      write(*,*)
      write(*,*) 'Before putting the locally-normalized spectrum ',       &
     &           'standard grid.'
!-----------------------------------------------------------------------
!
      if(igrid .eq. 0) then
          do430 : do i=1,ix
            write(iunit_out,'(2f25.15)') x(i),y(i)
          end do do430
        else
          grid=grid*log10(exp(real(1.d0,nprecision)))    ! Converting the grid spacing to base 10 logarithm.
          istart=ceiling(log10(x(1))/grid)
          iend=int(log10(x(ix))/grid)
!          write(*,*) 'ix,grid,istart,iend'
!          write(*,*) ix,grid,istart,iend
          do440 : do i=istart,iend
            xx=10.d0**( real(i,nprecision)*grid )
!            write(*,'(i5,f15.3,1p,2e25.15)') itmp,xx,xx
            call interpolation(ix,x,y,1,1,xx,itmp,yy)   ! Subroutines scew up when the variable kinds mismatch. 
!            write(*,'(i5,f15.3,1p,2e25.15)') itmp,xx,yy
            write(iunit_out,'(2f25.15)') xx,yy
          end do do440
      end if
!
      if(allocated(x)) deallocate(x)
      if(allocated(y)) deallocate(y)
      if(allocated(y2)) deallocate(y2)
!
      close(unit=iunit_in)
      close(unit=iunit_out)
!
      end subroutine locnorm
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! Copyright D.J. Jeffery, 2006jan01.
!
! interpolation.f does interpolations and extrapolations.
!
      subroutine interpolation(n,x,y,iimode,idx,xx,ixx,yy)
      use numerical2_mod
      implicit none
!
      integer, intent(in) :: n      ! Number of points in the array of data.
      integer, intent(in) :: iimode  ! 1 for linear, 2 for semi-log, 3 for log-log
      integer, intent(in) :: idx    ! 0 for equally spaced points, 1 for unequally spaced points
      integer, intent(out) :: ixx   ! Index of array space that contains xx
      integer :: i
      real (kind=nprecision), intent(in) :: x(n),y(n),xx
      real (kind=nprecision), intent(out) :: yy       ! The output interpolated value.
      real (kind=nprecision) :: a,b
!
      if(idx .eq. 0) then
          i=max( int( (xx-x(1))/(x(2)-x(1)) ) + 2, 2)  ! 2= 1 (for rounding up)
!                                                        !   +1 (for no zero element).
        else
          do i=2,n
           if(x(i) .ge. xx) exit
          end do
      end if
      ixx=min(i,n)
!
      if(iimode .eq. 1) then
          a=(y(ixx)-y(ixx-1))/(x(ixx)-x(ixx-1))    ! linear
          b=y(ixx)-a*x(ixx)
          yy=a*xx+b
!          yy=a*(xx-x(ixx-1))+y(ixx-1)
!          write(*,*) ixx-1,x(ixx-1),y(ixx-1)
!          write(*,*) ixx,x(ixx),y(ixx)
!          write(*,*) a,b,xx,xx-x(ixx-1),yy
!          stop
        else if(iimode .eq. 2) then
          a=log(y(ixx)/y(ixx-1))/(x(ixx)-x(ixx-1)) ! semi-logarithmic
          b=log(y(ixx))-a*x(ixx)
          yy=exp( a*xx+b )
        else
          a=log(y(ixx)/y(ixx-1))/log(x(ixx)/x(ixx-1)) ! logarithmic
          b=log(y(ixx))-a*log(x(ixx))
          yy=exp( a*log(xx)+b )
      end if
!
      end subroutine interpolation
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! Copyright D.J. Jeffery, 2006jan01.
!
!   References:
!
!     \bibitem[Bevington(1969)]{bevington1969} Bevington, P. R. 1969,
!         Data Reduction and Error Analysis for the Physical Sciences
!         (New York:  McGraw-Hill Book Company) (Bev)
!        % The old, but still very good Bevington
!
!     \bibitem[Metcalf et al.(2004)]{metcalf} Metcalf, M., Reid, J., \&~Cohen, M. 2004,
!          Fortran 95/2003 Explained
!          (Oxford:  Oxford University Press) (MRC)
!          % Pretty good reference book.
!
!     \bibitem[Press et al.(1992)]{press1992} Press, W. H., Teukolsky, S. A.,
!          Vetterling, W. T., \& Flannery, B. P. 1992,
!            Numerical Recipes in Fortran
!            (Cambridge:  Cambridge University Press) (Pr)
!            % Bill should have given me a complimentary copy when
!            % I co-taught with him in 1993!
!            % p. 269--273.
!
!  linreg1 is degree 1 least-squares subroutine.  Thus its fits a line
!  to a set of data.
!
!  See Bev-103--104.
!
!-----------------------------------------------------------------------
!
      subroutine linreg01(ix,w,x,y,c0,c1)
      use numerical2_mod
      implicit none
!
      real (kind=nprecision) :: a00,a01,a11 
      real (kind=nprecision) :: b0,b1 
      real (kind=nprecision), intent(out) :: c0,c1 
      integer, intent(in) :: ix
      real (kind=nprecision), intent(in) :: w(ix)
      real (kind=nprecision), intent(in) :: x(ix)
      real (kind=nprecision), intent(in) :: y(ix)
!
      a00=sum(w(:))                     ! See MRC-173 for sum.
      a01=sum(w(:)*x(:))
      a11=sum(w(:)*x(:)**2)
      b0=sum(w(:)*y(:))
      b1=sum(w(:)*x(:)*y(:))
!
!      write(*,*)
!      write(*,*) 'In linreg'
!      write(*,*) 'a00,a01,a11,b0,b1'
!      write(*,*) a00,a01,a11,b0,b1
!
      c0=(b0*a11-b1*a01)/(a00*a11-a01**2)
      c1=(b0-a00*c0)/a01
!
      end subroutine linreg01
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! Copyright D.J. Jeffery, 2007jan01.
!
! A subroutine to handle my own idiosyncratic read-ins.
!
!-----------------------------------------------------------------------
!
      subroutine readcopy(iunit1,iunit2,icopy,headend,idata)
      implicit none
!
      character, intent(in) :: headend*(*)   ! A ' ' must be input for the default header 'END:'.
      integer, intent(in) :: icopy           ! If icopy=1, then copy the header to a second file iunit2.
      integer, intent(in) :: iunit1          ! Unit to be read from.
      integer, intent(in) :: iunit2          ! Unit to be written to if icopy=1.
!
      integer, intent(out) :: idata          ! Counts how many lines beyond the header.
!
      integer :: i,j,k,l,m,n
      integer :: iheader                     ! Counts the number of header lines.
      character :: record*180
!
!      write(*,*) 'Before header readin.'
      rewind iunit1                          ! Necessary since sometimes, one
!                                            !  will read some of the file before
!                                            !  one calls readcopy.
!
      iheader=0
      do
        read(iunit1,'(a180)',end=100) record
        iheader=iheader+1
        if(headend .eq. ' ') then                      ! Some fortrans don't allow zero length strings.
            if(index(record,'END:') .ne. 0) exit       ! Check for the default header end.
          else
            if(index(record,headend) .ne. 0) exit      ! Check the given header end.
        end if
      end do
!
      go to 110   ! Goto's are honored in programming history.
  100 continue
      idata=iheader   ! There was no header, only data.
      iheader=0       !   Or maybe there was neither. 
      go to 120
  110 continue
!
      idata=0
      do
        read(iunit1,*,end=120)
        idata=idata+1
      end do
  120 continue
!
      rewind iunit1
!
!      write(*,*) 'iunit1=',iunit1
!      write(*,*) 'Before repeat header readin.  iheader=',iheader
!
      do i=1,iheader                           ! Read past the header in preparation for reading the data.
        read(iunit1,'(a180)',end=100) record
        if(icopy .eq. 1) write(iunit2,'(a)') trim(record)
!         read(iunit1,'(a)') record
!         write(*,*) i,trim(record)
      end do
!
      end subroutine readcopy
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! Copyright D.J. Jeffery, 2006jan01.
!
! smooth.f is a smoothing routine for spectra using a a relative smoothing
! interval.
!
!   References:
!
!     \bibitem[Metcalf et al.(2004)]{metcalf} Metcalf, M., Reid, J., \&~Cohen, M. 2004,
!          Fortran 95/2003 Explained
!          (Oxford:  Oxford University Press) (MRC)
!          % Pretty good reference book.
!
!     \bibitem[Press et al.(1992)]{press1992} Press, W. H., Teukolsky, S. A.,
!          Vetterling, W. T., \& Flannery, B. P. 1992,
!            Numerical Recipes in Fortran
!            (Cambridge:  Cambridge University Press)
!            % Bill should have given me a complimentary copy when
!            % I co-taught with him in 1993!
!            % p. 269--273.
!
!
!-----------------------------------------------------------------------
!
      subroutine smooth(iimode,ix,x,y,ismooth,smooth_xrel,smooth_zone) 
      use numerical2_mod
      implicit none
!
      real (kind=nprecision) :: aa 
      real (kind=nprecision) :: bb 
      integer :: i,j,k,l,m,n
      integer :: ia
      integer :: ib
      integer, intent(in) :: iimode
      integer, intent(in) :: ismooth
      integer, intent(in) :: ix
      integer :: jmode
      integer :: jsmooth
      real (kind=nprecision), intent(in) :: smooth_xrel(ismooth)
      real (kind=nprecision), intent(in) :: smooth_zone(ismooth)
      real (kind=nprecision) :: ww(ix) 
      real (kind=nprecision), intent(in) :: x(ix)
      real (kind=nprecision) :: xx(ix)
      real (kind=nprecision), intent(inout) :: y(ix)
      real (kind=nprecision) :: yy(ix)
      real (kind=nprecision) :: y2(ix)
      real (kind=nprecision) :: yout
!
      if(ismooth .eq. 0) return     ! This is the no-smoothing case which usually should never be called.
!
      jmode=abs(iimode)
!
      if(jmode .eq. 1) then
          xx(:)=x(:)
          yy(:)=y(:)
        else if(jmode .eq. 2) then
          xx(:)=log10(x(:))
          yy(:)=y(:)
        else if(jmode .eq. 3) then
          xx(:)=x(:)
          yy(:)=log10(y(:))          ! You'd better make sure y(i) is never less than or equal to zero.
        else if(jmode .eq. 4) then
          xx(:)=log10(x(:))
          yy(:)=log10(y(:))          ! You'd better make sure y(i) is never less than or equal to zero.
      end if
!
      y2(:)=yy(:)
!
      jsmooth=1
      do410 : do i=1,ix
 ! 
        if(x(i) .gt. smooth_zone(jsmooth)) then
            jsmooth=jsmooth+1
            if(jsmooth .gt. ismooth) exit do410
        end if
!
        ia=i
        do420 : do j=i-1,1,-1
          if((x(i)-x(j))/x(i) .le. smooth_xrel(jsmooth)) then
              ia=j
            else 
              exit do420
          end if
        end do do420
        ib=i
        do430: do j=i+1,ix
          if((x(j)-x(i))/x(i) .le. smooth_xrel(jsmooth)) then
              ib=j
           else
              exit do430
          end if
        end do do430
!
!        write(*,*) 'In smooth'
!        write(*,*) 'ia,ib'
!        write(*,*) 'ismooth,i,jsmooth,smooth_xrel,smooth_zone,ia,ib'
!        write(*,*) ismooth,i,jsmooth,smooth_xrel,smooth_zone,ia,ib
!
        if(ia .eq. ib) cycle do410
!
        if(iimode .gt. 0) then                                             ! Integration smoothing.
            call trapezoid(ib-ia+1,xx(ia:ib),yy(ia:ib),1,yout)
            y2(i)=yout/(xx(ib)-xx(ia))
          else                                                            ! Least squares smoothing.
            ww(:)=1.d0   ! For some dumb reason, this can't be done as an initiation.
!                        !  I suppose, the compiler needs the length of the array
!                        !  in order to assign values.
            call linreg01(ib-ia+1,ww(ia:ib),xx(ia:ib),yy(ia:ib),aa,bb)
            y2(i)=aa+bb*xx(i)
!            write(*,'(i5,7e15.4)') i,x(i),xx(i),yy(i),y2(i),                      &
!     &        abs( (yy(i)-y2(i))/y2(i) ),aa,bb
        end if
!
!        write(*,*) i,x(i),y(i),y2(i)
!
      end do do410
!
      if(jmode .le. 2) then 
         y(:)=y2(:)
        else
         y(:)=10.d0**y2(:)
      end if
!
      end subroutine smooth 
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12
!
! Copyright D.J. Jeffery, 2006jan01.
!
! trapezoid.f does the general trapezoid rule integration.
!
      subroutine trapezoid(n,x,y,idx,yintegral)
      use numerical2_mod
      implicit none
      integer, intent(in) :: n,idx
      integer :: i
      real (kind=nprecision), intent(in) :: x(n),y(n)
      real (kind=nprecision), intent(out) :: yintegral
!
      if(idx .eq. 0) then
          yintegral=(sum(y(:))-.5*(y(1)+y(n)) )*(x(2)-x(1))
        else
          yintegral=y(1)*(x(2)-x(1))+y(n)*(x(n)-x(n-1))
          do i=2,n-1
           yintegral=yintegral+y(i)*(x(i+1)-x(i-1))
          end do
          yintegral=.5*yintegral
      end if
!
      end subroutine trapezoid
!
!23456789a123456789b123456789c123456789d123456789e123456789f123456789g12

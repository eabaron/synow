!-
!- File    : LteLineList.f
!- -----------------------
!- Created : Thu Jul  1 17:04:56 2004
!- Authors : Rollin C. Thomas (RCT) - rcthomas@lbl.gov
!- Purpose : Multipurpose access to Kurucz line list.
!-
!- $Header: /home/users/rthomas/cvsroot/Brute/LteLineList.f,v 1.12 2004/08/07 17:50:04 rthomas Exp $
!-
!- TODO :: CHECK tauSobolev method for LTE runs.
!- TODO :: Add tauSobolev method for parameterized runs.
!-
      module LteLineListClass
!-    -----------------------
       use Global
       use Inform
       use Const
       use Sort

       use KuruczCtrl
       use Kurucz

       implicit none
       private

       public :: initLteLineList
       public :: LteLineList
       public :: dumpLineList
       public :: identInsert
       public :: identDelete
       public :: ionCodeInsert
       public :: ionCodeDelete
       public :: tauSobolev

       integer, parameter :: GROW_FACTOR   = 2
       integer, parameter :: INIT_CAPACITY = GROW_FACTOR ** 10

       type, public :: LteLineType
!-     ---------------------------
        
        !- Information for identification.
        integer :: ident   = 0             !- Identifier; index in the Kurucz line list.
        integer :: ionCode = 0             !- Ion code; 100 * atomNum + ionState.

        !- Physical line parameters.
        real( wp ) :: lambda = 0.0_wp      !- Line wavelength in Angstroms.
        real( wp ) :: gf     = 0.0_wp      !- Line oscillator strength.
        real( wp ) :: chi    = 0.0_wp      !- Line lower level energy above ground state.

       end type LteLineType

       type, public :: LteLineListType
!-     -------------------------------

        !- Line list management.
        integer :: kUnit    = 0                             !- File unit for talking to Kurucz linelist file.
        integer :: kcUnit   = 0                             !- File unit for talkint to Kurucz control files.
        integer :: capacity = 0                             !- Current line list capacity.
        integer :: numLine  = 0                             !- Number of lines in line list.

        !- Line list parameter cuts.
        real( wp ) :: lambdaMin = 0.0_wp                    !- Minimum wavelength to load.
        real( wp ) :: lambdaMax = + huge( 1.0_wp )          !- Maximum wavelength to load.
        real( wp ) :: logGfMin  = - huge( 1.0_wp )          !- Minimum log( gf ) to load.
        integer    :: recordMin = + huge( 1 )               !- Lowest record corresponding to minimum wavelength.
        integer    :: recordMax = - huge( 1 )               !- Highest record corresponding to maximum wavelength.

        !- Actual line list.
        integer            , pointer :: key( : )  => null() !- Mapping from ident to line list entry (goes from recordMin to Max).
        type( LteLineType ), pointer :: line( : ) => null() !- Line list.

       end type LteLineListType

       interface LteLineList
!-     ---------------------
        module procedure LteLineList_new  !- Constructor.
        module procedure LteLineList_copy !- Copy constructor.
        module procedure LteLineList_die  !- Destructor.
       end interface LteLineList

       interface identInsert
!-     ---------------------
        module procedure identInsert_one
        module procedure identInsert_many
       end interface identInsert

       interface identDelete
!-     ---------------------
        module procedure identDelete_one
        module procedure identDelete_many
       end interface identDelete

       interface ionCodeInsert
!-     -----------------------
        module procedure ionCodeInsert_one
        module procedure ionCodeInsert_many
       end interface ionCodeInsert

       interface ionCodeDelete
!-     -----------------------
        module procedure ionCodeDelete_one
        module procedure ionCodeDelete_many
       end interface ionCodeDelete

       interface tauSobolev
!-     --------------------
        module procedure tauSobolev_lte
       end interface tauSobolev

       contains
!- ---
!- --- initLteLineList :
!- --- Passes parameters to the KuruczCtrl module and the Kurucz module.
!- ---
       subroutine initLteLineList( kCtrlPath, kFilePath, nuxi )
!-     --------------------------------------------------------
        character( len = * ), intent( in ) :: kCtrlPath, kFilePath
        logical             , intent( in ) :: nuxi

        call initKuruczCtrl( kCtrlPath, nuxi )
        call initKurucz( kFilePath, nuxi )

       end subroutine initLteLineList
!- ***
!- *** LteLineList_new :
!- *** Constructor.  Defines file units and the line list parameter cuts.
!- ***
       subroutine LteLineList_new( self, kUnit, kcUnit, lambdaMin, lambdaMax, logGfMin )
!-     ---------------------------------------------------------------------------------
        type( LteLineListType ), intent( in out )           :: self
        integer                , intent( in     )           :: kUnit, kcUnit
        real( wp )             , intent( in     )           :: lambdaMin, lambdaMax
        real( wp )             , intent( in     ), optional :: logGfMin

        call LteLineList( self )

        self%kUnit = kUnit
!- FIX  Make sure this choice is okay.

        self%kcUnit = kcUnit
!- FIX  Make sure this choice is okay.

        self%capacity = INIT_CAPACITY

        self%numLine  = 0

        self%lambdaMin = lambdaMin
        checkLambdaMin : if( self%lambdaMin .lt. 0.0_wp )then
         call warn( 'LteLineList', 'setting lambdaMin to zero : lambdaMin is less than zero' )
         self%lambdaMin = 0.0_wp
        end if checkLambdaMin

        self%lambdaMax = lambdaMax
        checkLambdaMax : if( self%lambdaMin .gt. self%lambdaMax )then
         call warn( 'LteLineList', 'aborting init : lambdaMin is greater than lambdaMax' )
         call LteLineList( self )
         return
        end if checkLambdaMax

        if( present( logGfMin ) )then
         self%logGfMin = logGfMin
        end if

        self%recordMin = kuruczRecord( self%kUnit, self%lambdaMin )
        self%recordMax = kuruczRecord( self%kUnit, self%lambdaMax )

        allocate( self%key( self%recordMin : self%recordMax ) )
        self%key = 0

        allocate( self%line( self%capacity ) )

       end subroutine LteLineList_new
!- ***
!- *** LteLineList_copy :
!- *** Copy constructor.
!- ***
       subroutine LteLineList_copy( self, orig )
!-     -----------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        type( LteLineListType ), intent( in     ) :: orig

        call LteLineList( self )

        self%kUnit    = orig%kUnit
        self%kcUnit   = orig%kcUnit
        self%capacity = orig%capacity
        self%numLine  = orig%numLine

        self%lambdaMin = orig%lambdaMin
        self%lambdaMax = orig%lambdaMax
        self%logGfMin  = orig%logGfMin
        self%recordMin = orig%recordMin
        self%recordMax = orig%recordMax

        if( self%recordMin .le. self%recordMax )then
         allocate( self%key( self%recordMin : self%recordMax ) )
         self%key = orig%key
        end if

        if( self%capacity .gt. 0 )then
         allocate( self%line( self%capacity ) )
         self%line = orig%line
        end if

       end subroutine LteLineList_copy
!- ***
!- *** LteLineList_die :
!- *** Destructor.
!- ***
       subroutine LteLineList_die( self )
!-     ----------------------------------
        type( LteLineListType ), intent( in out ) :: self

        self%kUnit    = 0
        self%kcUnit   = 0
        self%capacity = 0
        self%numLine  = 0

        self%lambdaMin = 0.0_wp          
        self%lambdaMax = + huge( 1.0_wp )
        self%logGfMin  = - huge( 1.0_wp )
        self%recordMin = + huge( 1 )
        self%recordMax = - huge( 1 )

        if( associated( self%key  ) ) deallocate( self%key  )
        if( associated( self%line ) ) deallocate( self%line )

       end subroutine LteLineList_die
!- ***
!- *** dumpLineList :
!- *** Basic line list printer, right now just does basic stuff, we will make it
!- *** look pretty later on.
!- ***
       subroutine dumpLineList( self, ui )
!-     -----------------------------------
        type( LteLineListType ), intent( in )           :: self
        integer                , intent( in ), optional :: ui

        integer :: u, i

        if( self%numLine .eq. 0 ) return

        u = STDOUT
        if( present( ui ) )then
         if( ui .gt. 0 ) u = ui
        end if

        do i = 1, self%numLine
         write( u, '( 2( I8, 8X ), 3( F15.10, 1X ) )' ) self%line( i )
        end do

       end subroutine dumpLineList
!- ***
!- *** identInsert_one :
!- *** Adds a single line to the line list.
!- ***
       subroutine identInsert_one( self, ident )
!-     -----------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ident

        integer :: itemp( 1 )

        itemp = (/ ident /)
        call identInsert( self, itemp )

       end subroutine identInsert_one
!- ***
!- *** identInsert_many :
!- *** Base insert method.  All other insert methods end up calling this one.
!- *** Given list of idents, this subroutine inserts corresponding lines 
!- *** subject to the existing cuts.  This form of insert avoids duplication
!- *** of the lines.  Note that some cuts can be applied to the ident list
!- *** before reading actually starts.  Others require that we first read
!- *** in the lines and then apply the cuts (logGfMin is the most obvious).
!- ***
       subroutine identInsert_many( self, ident )
!-     ------------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ident( : )
 
        integer    :: i, nid, nok, new
        integer    :: ix( size( ident ) ), sid( size( ident ) )
        logical    :: mask( size( ident ) )
        real( wp ) :: gfMin
        type( LteLineType ) :: buff( size( ident ) ), lbuf( self%capacity )

        !- Sort idents into ascending order.
        call indexx( ident, ix )
        sid = ident( ix )

        !- Cut idents too low or too high.
        mask = ( sid .ge. self%recordMin ) .and. ( sid .le. self%recordMax )
        if( .not. any( mask ) ) return

        !- Cut duplicates.
        nid = size( ident )
        do i = 1, nid
         if( .not. mask( i ) ) cycle
         mask( i ) = self%key( sid( i ) ) .eq. 0
        end do
        if( .not. any( mask ) ) return

        !- Close pack sorted ident's.
        nok = count( mask )
        sid = pack( sid, mask, spread( 1, 1, nid ) )

        !- Read lines into buffer.
        buff( 1 : nok )%ident = sid( 1 : nok )
        call KuruczList( self%kUnit, &
         buff( 1 : nok )%ident, buff( 1 : nok )%ionCode, buff( 1 : nok )%lambda, buff( 1 : nok )%gf, buff( 1 : nok )%chi )

        !- Cut gf's too high if needed.
        if( self%logGfMin .gt. - huge( 1.0_wp ) )then
         gfMin = 10.0_wp ** self%logGfMin
         mask( 1 : nok ) = buff( 1 : nok )%gf .ge. gfMin
         new = count( mask( 1 : nok ) )
         buff = pack( buff, mask, spread( LteLineType( 0, 0, 0.0_wp, 0.0_wp, 0.0_wp ), 1, nid ) )
        else
         mask( 1 : nok ) = .true.
         new = nok
        end if
        if( new .eq. 0 ) return

        !- Resize list if needed.
        if( self%capacity .lt. self%numLine + new )then
         do while( self%capacity .lt. self%numLine + new )
          self%capacity = self%capacity * GROW_FACTOR
         end do
         lbuf = self%line
         if( associated( self%line ) ) deallocate( self%line )
         allocate( self%line( self%capacity ) )
         self%line( 1 : size( lbuf ) ) = lbuf
        end if

        !- Append new lines.
        self%line( self%numLine + 1 : self%numLine + new ) = buff( 1 : new )
        self%numLine = self%numLine + new

        !- Sort line list.
        call identSort( self )

        !- Rebuild keys.
        call keyBuild( self )
                                    
       end subroutine identInsert_many
!- ***
!- *** identDelete_one :
!- *** 
!- ***
       subroutine identDelete_one( self, ident )
!-     -----------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ident

        call identDelete( self, (/ ident /) )

       end subroutine identDelete_one
!- ***
!- *** identDelete_many :
!- *** Given a list of idents, hunts them down and deletes them.
!- ***
       subroutine identDelete_many( self, ident )
!-     ------------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ident( : )

        integer :: i, ii
        logical :: mask( self%capacity )

        mask = .true.
        do i = 1, size( ident )
         ii = ident( i )
         if( ii .lt. self%recordMin ) cycle
         if( ii .gt. self%recordMax ) cycle
         if( self%key( ii ) .eq. 0  ) cycle
         mask( self%key( ii ) ) = .false.
        end do

        self%line = pack( self%line, mask, spread( LteLineType( 0, 0, 0.0_wp, 0.0_wp, 0.0_wp ), 1, self%capacity ) )
        self%numLine = count( self%line%ident .gt. 0 )

        call keyBuild( self )

       end subroutine identDelete_many
!- ***
!- *** ionCodeInsert_one :
!- ***
!- ***
       subroutine ionCodeInsert_one( self, ionCode )
!-     ---------------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ionCode

        call ionCodeInsert( self, (/ ionCode /) )

       end subroutine ionCodeInsert_one
!- ***
!- *** ionCodeInsert_many :
!- *** Insert lines according to ion code.
!- ***
       subroutine ionCodeInsert_many( self, ionCode )
!-     ----------------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ionCode( : )

        integer :: i, j, k
        integer, pointer :: itemp( : ) => null()
        integer, pointer :: ident( : ) => null() 

        if( associated( ident ) ) deallocate( ident )
        allocate( ident( sum( numRecord( ionCode ) ) ) )

        j = 1 
        do i = 1, size( ionCode )
         call recordList( self%kcUnit, ionCode( i ), itemp )
         k = size( itemp )
         if( k .eq. 0 ) cycle
         ident( j : j + k - 1 ) = itemp
         j = k + 1
        end do

        call identInsert( self, ident )

        if( associated( ident ) ) deallocate( ident )
        if( associated( itemp ) ) deallocate( itemp )

       end subroutine ionCodeInsert_many
!- ***
!- *** ionCodeDelete_one :
!- ***
!- ***
       subroutine ionCodeDelete_one( self, ionCode )
!-     ---------------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ionCode

        call ionCodeDelete( self, (/ ionCode /) )

       end subroutine ionCodeDelete_one
!- ***
!- *** ionCodeDelete_many :
!- *** Delete lines according to ion code.
!- ***
       subroutine ionCodeDelete_many( self, ionCode )
!-     ----------------------------------------------
        type( LteLineListType ), intent( in out ) :: self
        integer                , intent( in     ) :: ionCode( : )

        integer :: i, j, nd, ident( self%numLine )

        nd = 0
        do i = 1, self%numLine
         do j = 1, size( ionCode )
          if( self%line( i )%ionCode .ne. ionCode( j ) ) cycle
          nd = nd + 1
          ident( nd ) = self%line( i )%ident
          exit
         end do
        end do

        call identDelete( self, ident( 1 : nd ) )

       end subroutine ionCodeDelete_many
!- ***
!- *** tauSobolev_lte :
!- ***
!- ***
       subroutine tauSobolev_lte( self, age, temp, dens, tau )
!-     -------------------------------------------------------
        type( LteLineListType ), intent( in     ) :: self
        real( wp )             , intent( in     ) :: age, temp, dens( 0 :, : )
        real( wp )             , intent(    out ) :: tau( : )

        integer :: i, j, k

        do i = 1, self%numLine
         k = self%line( i )%ionCode / 100
         j = self%line( i )%ionCode - 100 * k
         tau( i ) = optConst * self%line( i )%gf * self%line( i )%lambda * exp( - self%line( i )%chi / kConstEV / temp )
         tau( i ) = tau( i ) * age * dens( j, k ) 
         tau( i ) = tau( i ) * ( 1.0_wp - exp( - ksiConst / self%line( i )%lambda / temp ) )
        end do
 
       end subroutine tauSobolev_lte
!- +++
!- +++ identSort :
!- +++ Sorts a list of LteLines by ident and passes them back.
!- +++
       subroutine identSort( self )
!-     ----------------------------
        type( LteLineListType ), intent( in out ) :: self

        integer             :: ix( self%numLine )
        type( LteLineType ) :: buf( self%numLine )

        call indexx( self%line( 1 : self%numLine )%ident, ix )
        buf = self%line( ix )
        self%line( 1 : self%numLine ) = buf

       end subroutine identSort
!- +++
!- +++ keyBuild :
!- +++ Builds or re-builds the key mapping.
!- +++
       subroutine keyBuild( self )
!-     ---------------------------
        type( LteLineListType ), intent( in out ) :: self

        integer :: i

        self%key = 0
        do i = 1, self%numLine
         self%key( self%line( i )%ident ) = i
        end do

       end subroutine keyBuild
!- +++
!- +++
!- +++
      end module LteLineListClass
!-
!- $Log: LteLineList.f,v $
!- Revision 1.12  2004/08/07 17:50:04  rthomas
!- Bugfix!  I had not initialized the key array after I allocated it.  This
!- resulted in mostly zeroes and then some garbage getting put in on the xlf
!- compiler.  Thanks to Darrin for helping me track this down.
!-
!- Revision 1.11  2004/07/28 23:48:15  rthomas
!- Added tauSobolev method.
!-
!- Revision 1.10  2004/07/27 05:57:37  rthomas
!- Minor edits.
!-
!- Revision 1.9  2004/07/22 23:51:43  rthomas
!- Added headers and logs to everything and corrected the stupid "Changed"
!- comment thing to "Created."
!-

!- 
!- File    : Sort.f
!- ----------------
!- Created : Mon Sep  8 10:22:03 PDT 2003
!- Authors : Rollin C. Thomas (RCT) - rcthomas@lbl.gov
!- Purpose : Provides sorting module
!- 
!- $Header: /home/users/rthomas/cvsroot/Brute/Sort.f,v 1.2 2004/07/22 23:51:43 rthomas Exp $
!-
      module Sort
!-    -----------
       use Global
       use Inform

       implicit none
       private

       public :: hpsort
       public :: indexx

       interface indexx
!-     ----------------
        module procedure indexx_int
!       module procedure indexx_real
       end interface indexx

       contains
!- ***
!- *** hpsort :
!- *** Sorts an array arr( : ) into ascending numerical order using the
!- *** heapsort algorithm.  The array is replaced on output by its sorted
!- *** rearrangement.
!- ***
       subroutine hpsort( arr )
!-     ------------------------
        real( wp ), intent( in out ) :: arr( : )

        integer    :: i, n
        real( wp ) :: b

        n = size( arr )

        do i = n / 2, 1, - 1
         call sift( i, n )
        end do
        do i = n, 2, - 1
         b = arr( 1 )
         arr( 1 ) = arr( i )
         arr( i ) = b
         call sift( 1, i - 1 )
        end do

        contains

        subroutine sift( l, r )
!-      -----------------------
         integer, intent( in ) :: l, r

         integer    :: j, jold
         real( wp ) :: a

         a = arr( l )
         jold = l
         j = l + l
         do
          if( j .gt. r ) exit
          if( j .lt. r )then
           if( arr( j ) .lt. arr( j + 1 ) ) j = j + 1
          end if
          if( a .ge. arr( j ) ) exit
          arr( jold ) = arr( j )
          jold = j
          j = j + j
         end do
         arr( jold ) = a

        end subroutine sift

       end subroutine hpsort
!- ***
!- *** indexx_int :
!- *** Indexes in array, outputting idx( : ) such that arr( idx( 1 : n ) )
!- *** is in ascending order for j = 1 .. n.  The input quantity arr is not
!- *** changed.
!- ***
       subroutine indexx_int( arr, idx )
!-     ---------------------------------
        integer, intent( in     ) :: arr( : )
        integer, intent(    out ) :: idx( : )

        integer, parameter :: NN     = 15
        integer, parameter :: NSTACK = 50

        integer :: a, n, k, i, j, idxt, jstack, l, r, istack( NSTACK )

        n = size( idx )
        chkArrIdxSize: if( size( arr ) .ne. n )then
         call die( 'Sort', 'arr and idx are not the same size' )
        end if chkArrIdxSize

        idx = (/ ( i, i = 1, n ) /)
        jstack = 0
        l = 1
        r = n
        do
         if( r - l < NN )then
          do j = l + 1, r
           idxt = idx( j )
           a = arr( idxt )
           do i = j - 1, 1, - 1
            if ( arr( idx( i ) ) <= a ) exit
            idx( i + 1 ) = idx( i )
           end do
           idx( i + 1 ) = idxt
          end do
          if ( jstack == 0 ) RETURN
          r = istack( jstack )
          l = istack( jstack - 1 )
          jstack = jstack - 2
         else
          k = ( l + r ) / 2
          call swap( idx( k ), idx( l + 1 ) )
          call icomp_xchg( idx( l ), idx( r ) )
          call icomp_xchg( idx( l + 1 ), idx( r ) )
          call icomp_xchg( idx( l ), idx( l + 1 ) )
          i = l + 1
          j = r
          idxt = idx( l + 1 )
          a = arr( idxt )
          do
           do
            i = i + 1
            if ( arr( idx( i ) ) >= a ) exit
           end do
           do
            j = j - 1
            if (arr( idx( j ) ) <= a ) exit
           end do
           if ( j < i ) exit
           call swap( idx( i ), idx( j ) )
          end do
          idx( l + 1 ) = idx( j )
          idx( j ) = idxt
          jstack = jstack + 2
          if( jstack > NSTACK ) call die( 'Sort', 'indexx NSTACK overflow' )
          if ( r - i + 1 >= j - l ) then
           istack( jstack ) = r
           istack( jstack - 1 ) = i
           r = j - 1
          else
           istack( jstack ) = j - 1
           istack( jstack - 1 ) = l
           l = i
          end if
         end if
        end do

        contains

        subroutine icomp_xchg( i, j )
!-      -----------------------------
         integer, intent( in out ) :: i, j

         if ( arr( j ) < arr( i ) ) call swap( i, j )

        end subroutine icomp_xchg

        subroutine swap( i, j )
!-      -----------------------
         integer, intent( in out ) :: i, j

         integer :: k

         k = i
         i = j
         j = k

        end subroutine swap

       end subroutine indexx_int
!- +++
!- +++
!- +++
      end module Sort
!-
!- $Log: Sort.f,v $
!- Revision 1.2  2004/07/22 23:51:43  rthomas
!- Added headers and logs to everything and corrected the stupid "Changed"
!- comment thing to "Created."
!-

program testBinaryTree

!*****************************************************************************80
!
!! MAIN is the main program for the binary tree example.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 2007
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer data_num

  write ( *, '(a)' ) ' '
  call timestamp ( )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BINARY_TREE:'
  write ( *, '(a)' ) '  FORTRAN90 version.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Demonstrate how pointers can be used to define'
  write ( *, '(a)' ) '  and manipulate a binary tree.'

  data_num = 10
  call test01 ( data_num )

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'BINARY_TREE:'
  write ( *, '(a)' ) '  Normal end of execution.'

  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop
end

subroutine test01 ( data_num )

!*****************************************************************************80
!
!! TEST01 uses a binary tree to store and sort random data.
!
!  Discussion:
!
!    This routine requires a binary tree library.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 December 2007
!
!  Author:
!
!    John Burkardt
!
  use binary_tree_library

  implicit none

  integer data_num
  type ( record ), pointer :: head
  integer i
  type ( record ), pointer :: item
  real r

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Create, one at a time, a sequence of integers.'
  write ( *, '(a)' ) '  As each integer is created, insert it into a sorted'
  write ( *, '(a)' ) '  binary tree.  Print the binary tree at the end.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Initial data generation:'
  write ( *, '(a)' ) ' '

  nullify ( head )

  do i = 1, data_num
!
!  Generate a new item.
!
    allocate ( item )

    item%generation = i

    call random_number ( harvest = r )

    item%value = int ( 1000.0 * r )

    write ( *, '(2x,i8,2x,i8)' ) i, item%value
!
!  Insert the new item into the linked list.
!  The INSERT routine takes care of initializing the other fields in the
!  ITEM data.
!
    call binary_tree_insert ( item, head )

  end do
!
!  Print the binary tree.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '  Contents of sorted binary tree:'
  write ( *, '(a)' ) ' '

  call binary_tree_print ( head )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    06 August 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  character ( len = 8  ) ampm
  integer   ( kind = 4 ) d
  integer   ( kind = 4 ) h
  integer   ( kind = 4 ) m
  integer   ( kind = 4 ) mm
  character ( len = 9  ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer   ( kind = 4 ) n
  integer   ( kind = 4 ) s
  integer   ( kind = 4 ) values(8)
  integer   ( kind = 4 ) y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end

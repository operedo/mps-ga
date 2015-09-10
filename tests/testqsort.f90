program testqsort
  
  implicit none

  integer(4),parameter  :: len = 20
  integer(4)            :: irow,icol
  integer(4)          :: array(len)
  integer(4)            :: indexes(len)
  integer(4)            :: aux


  do irow=1,len
    array(irow) = len-irow
    indexes(irow) = irow
  end do

  array(1)=3
  array(2)=4
  array(3)=5
  array(4)=6
  array(5)=7
  array(6)=8
  array(7)=0
  array(8)=1
  array(9)=2
  array(10)=3

  print *,'pre-array:'
  write(*,'(20I3)')array

  print *,'pre-indexes:'
  write(*,'(20I3)')indexes

  call qsort_int(array,indexes,20)

  print *,'pos-array:'
  write(*,'(20I3)')array

  print *,'pos-indexes:'
  write(*,'(20I3)')indexes

end program testqsort   

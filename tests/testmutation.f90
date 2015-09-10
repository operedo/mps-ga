program testmutation
  
  implicit none

  type individual
     integer(4), pointer    :: matrix(:,:)
     integer(4), pointer    :: histogram(:)
  end type individual

  type(individual)          :: indiv

  integer(4)            :: rows, cols
  integer(4)            :: irow,icol
!  integer(4), pointer   :: array(:)
  real(8)               :: perc_mutation
  real(8)               :: rand1, rand2

  open (unit = 13, file = "../data/data_testmutation.txt")
  read (13,*)  rows, cols, perc_mutation
  close(13)

  allocate(indiv%matrix(rows,cols))

  open (unit = 13, file = "../data/image.txt")
  read (13,*) indiv%matrix
  close(13)

!  allocate(array(rows*cols))                     
!  call oneDimensional(rows,cols,indiv%matrix,array)
  write (*,'(10I2)')indiv%matrix

  call initRandomSeed()

  call mutation(indiv%matrix,rows,cols,perc_mutation,1)
  write (*,*) ""
  write (*,'(10I2)')indiv%matrix
 
  deallocate(indiv%matrix)
!  deallocate(array)

end program testmutation      

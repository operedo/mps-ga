program testmutation
  
  implicit none

  type individual
     integer(4), pointer    :: matrix(:,:)
     integer(4), pointer    :: histogram(:)
  end type individual

  type(individual)          :: indivA, indivB, indivC

  integer(4)            :: number, rows, cols
  integer(4)            :: irow,icol
!  integer(4), pointer   :: parentA(:), parentB(:)
!  integer(4), pointer   :: childC(:), childD(:)
  integer(4)            :: cut_points
  real(8),pointer       :: rand(:)

  open (unit = 13, file = "../data/data_testcrossover.txt")
  read (13,*) number, rows, cols, cut_points
  close(13)

  allocate(indivA%matrix(rows,cols))
  allocate(indivB%matrix(rows,cols))
  allocate(indivC%matrix(rows,cols))

  open (unit = 13, file = "../data/image_A_crossover.txt")
  read (13,*) indivA%matrix
  close(13)
  
  open (unit = 13, file = "../data/image_B_crossover.txt")
  read (13,*) indivB%matrix
  close(13)

!  allocate(parentA(rows*cols))                     
!  call oneDimensional(rows,cols,indivA%matrix,parentA)
!  write (*,*) "parentA:"
!  write (*,'(100I1)'),parentA
!  allocate(parentB(rows*cols))                     
!  call oneDimensional(rows,cols,indivB%matrix,parentB)
!  write (*,*) "parentB:"
!  write (*,'(100I1)'),parentB

  call initRandomSeed()

  allocate(rand(cut_points))

  do irow=1,cut_points
    call random_number(rand(irow))
!    print *,'rand(',irow,')=',rand(irow)
  end do

!  allocate(childC(rows*cols))
!  allocate(childD(rows*cols))

  call crossover(indivA%matrix,indivB%matrix,indivC%matrix,rows,cols,cut_points)

  write (*,*) "child:"
  write (*,'(100I1)')indivC%matrix
!  write (*,*) "childD:"
!  write (*,'(100I1)'),childD
 

  deallocate(rand)
  deallocate(indivA%matrix)
  deallocate(indivB%matrix)
  deallocate(indivC%matrix)
!  deallocate(parentA)
!  deallocate(parentB)
!  deallocate(childC)
!  deallocate(childD)

end program testmutation      

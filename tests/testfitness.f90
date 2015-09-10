program testfitness
  
  implicit none

  type individual
     integer(4), pointer    :: matrix(:,:)
     integer(4), pointer    :: histogram(:)
  end type individual

  type(individual)          :: indivTI, indivRE

  integer(4)            :: number, rows, cols, tem_rows, tem_cols
  integer(4)            :: irow,icol
  integer(4)            :: value

  open (unit = 13, file = "../data/data_testfitness.txt")
  read (13,*) number, rows, cols, tem_rows, tem_cols
  close(13)

  allocate(indivTI%histogram(2**(tem_rows*tem_cols)))
  allocate(indivRE%histogram(2**(tem_rows*tem_cols)))
  allocate(indivTI%matrix(rows,cols))
  allocate(indivRE%matrix(rows,cols))

  open (unit = 13, file = "../data/image_TI_fitness.txt")
  read (13,*) indivTI%matrix
  close(13)
  
  open (unit = 13, file = "../data/image_RE_fitness.txt")
  read (13,*) indivRE%matrix
  close(13)

  call calculateHistogram(indivTI%matrix,rows,cols,tem_rows,tem_cols,indivTI%histogram)

  print *,'TI histogram:'
  write(*,'(16I2)')indivTI%histogram
  
  print *,'RE histogram:'

  call fitnessFunction(indivRE%matrix,rows,cols,tem_rows,tem_cols,indivTI%histogram,value)

  print *, 'fitness=', value
  
  deallocate(indivTI%histogram)
  deallocate(indivRE%histogram)
  deallocate(indivTI%matrix)
  deallocate(indivRE%matrix)

end program testfitness      

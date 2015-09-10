program testHistogram

  implicit none

  type individual
     integer(4), pointer    :: matrix(:,:)
     integer(4), pointer    :: histogram(:)
  end type individual

  type(individual)          :: indiv

  integer(4)            :: number, rows, cols, tem_rows, tem_cols
  integer(4)            :: irow,icol
  integer(4), pointer   :: array(:)
  real(8)               :: perc_mutation


  open (unit = 13, file = "../data/data_testHistogram.txt")
  read (13,*) rows, cols, tem_rows, tem_cols
  close(13)

  allocate(indiv%matrix(rows,cols))

  !open (unit = 13, file = "../data/channels100x100x1.txt")
  open (unit = 13, file = "../data/image.txt")
  
  !open (unit = 13, file = "../data/image_100x100_3x3_blocks_0.txt")
  read (13,*) indiv%matrix
  close(13)

  allocate(indiv%histogram(2**(tem_rows*tem_cols)))


  !do irow=1,10000
     call calculateHistogram(indiv%matrix,rows,cols,tem_rows,tem_cols,indiv%histogram)
  !end do
  write (*,*) ""
  do irow= 1,2**(tem_rows*tem_cols)
     print *, indiv%histogram(irow)
  end do
  !write (*,*) ""
  !write (*,'(10I2)'),indiv%matrix
  


end program testHistogram

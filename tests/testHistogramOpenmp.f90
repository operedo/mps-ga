program testHistogramOpenmp
  use omp_lib

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
  integer(4)            :: nthreads, id

  

 
   !$omp parallel private(id)
   id = omp_get_thread_num()
   write (*,*) 'Hello World from thread', id
   !!!!   !$omp barrier
   if ( id == 0 ) then
     nthreads = omp_get_num_threads()
     write (*,*) 'There are', nthreads, 'threads'
   end if
   !$omp end parallel

  open (unit = 13, file = "../data/data_testHistogramOpenmp.txt")
  read (13,*) rows, cols, tem_rows, tem_cols
  close(13)

  allocate(indiv%matrix(rows,cols))

  open (unit = 13, file = "../data/channels100x100x1.txt")
  read (13,*) indiv%matrix
  close(13)

  allocate(indiv%histogram(2**(tem_rows*tem_cols)))
  
  do irow=1,10000
     call calculateHistogramOpenmp(indiv%matrix,rows,cols,tem_rows,tem_cols,indiv%histogram)
  end do
  write (*,*) ""
  !write (*,*) indiv%histogram
  !write (*,*) ""
  !write (*,'(10I2)'),indiv%matrix
end program testHistogramOpenmp

program testfitnessChainOperationsCells_trace

  use chainOperationsV2
  !use chainOperations


  implicit none

  type individual
     integer(4), allocatable :: matrix(:,:)
     integer(4), allocatable :: histogram(:)
  end type individual

  type(individual)          :: indivTI, indivRE

  integer(4)            :: number, rows, cols, tem_rows, tem_cols
  integer(4)            :: irow,icol,ii,unrolls,blocks
  integer(4)            :: value,tem_cells
  integer(4),allocatable    :: tem_cells_rows(:),tem_cells_cols(:)
  integer(4)            :: clock_start, clock_end, clock_rate
  integer(4)::ierr  

  call system_clock(COUNT_RATE=clock_rate)


  !open (unit = 13, file = "../../data/data_testfitnessCells.txt")
  open (unit = 13, file = "../../data/data_testfitnessCells1000x1000.txt")
  read (13,*) rows, cols, tem_rows, tem_cols
  read (13,*) tem_cells
  if(tem_cells>0) then
     allocate(tem_cells_rows(tem_cells))
     allocate(tem_cells_cols(tem_cells))
     do ii=1,tem_cells
        read (13,*) tem_cells_rows(ii), tem_cells_cols(ii)
     end do
  end if
  close(13)

  !allocate(indivTI%histogram(2**(tem_rows*tem_cols)))
  !allocate(indivRE%histogram(2**(tem_rows*tem_cols)))
  allocate(indivTI%matrix(rows,cols))
  allocate(indivRE%matrix(rows,cols))

  !open (unit = 13, file = "../../data/channels100x100x1.dat")
  open (unit = 13, file = "../../data/ellipsim_1000x1000_ok.dat")
  read (13,*) indivTI%matrix
  close(13)
  
  !open (unit = 13, file = "../../data/image_RE_fitness.txt")
  open (unit = 13, file = "../../data/image_RE_1000x1000.txt")
  read (13,*) indivRE%matrix
  close(13)

  call loadHistogramCells(indivTI%matrix,rows,cols,tem_rows,tem_cols, tem_cells, tem_cells_rows, tem_cells_cols,0)
  !call loadHistogramCells_baseline(indivTI%matrix,rows,cols,tem_rows,tem_cols, tem_cells, tem_cells_rows, tem_cells_cols,0)

!  print *,'TI histogram:'
!  write(*,'(17I7)')chainArray(:)%frequency
  
!  print *,'RE histogram:'


  unrolls=1
  blocks=1
  !do while(ii<=64)
  !do while(ii<=1)
  !do while(ii<=7*5)

  call system_clock(COUNT=clock_start)
  call fitnessFunctionChainOperationsCells(indivRE%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows,tem_cells_cols,unrolls,blocks)
  !call fitnessFunctionChainOperationsCellsACC(indivRE%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows,tem_cells_cols,unrolls,blocks)
  !call fitnessFunctionChainOperationsCells_baseline(indivRE%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows,tem_cells_cols,unrolls,blocks)
  !call fitnessFunctionChainOperationsCellsMPI(indivRE%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows,tem_cells_cols,unrolls,blocks)
  !call fitnessFunctionChainOperationsCellsUnroll(indivRE%matrix,rows,cols,tem_rows,tem_cols,value, tem_cells, tem_cells_rows,tem_cells_cols,unrolls,blocks)
  call system_clock(COUNT=clock_end)
  print *, unrolls,"CLOCK=",real((real(clock_end)-real(clock_start))/real(clock_rate))
  print *, "rate=",real(clock_rate)," diff=",((clock_end)-(clock_start)) 
  print *, 'fitness=', value
  ii=ii*2
  !ii=ii+5
  !end do
  
  !deallocate(indivTI%histogram)
  !deallocate(indivRE%histogram)
  deallocate(indivTI%matrix)
  deallocate(indivRE%matrix)
  deallocate(tem_cells_rows)
  deallocate(tem_cells_cols)


end program testfitnessChainOperationsCells_trace     

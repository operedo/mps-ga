
subroutine loadHistogramCells(matrix,rows,cols,tem_rows,tem_cols,tem_cells,tem_cells_rows,tem_cells_cols,clock_rate)

  use chainOperationsV2
  !use chainOperations

  integer(4), intent(in)                   :: rows, cols
  integer(4), intent(in)                   :: matrix(rows,cols)
  integer(4), intent(in)                   :: tem_rows, tem_cols, tem_cells
  integer(4), intent(in)                   :: clock_rate
  !type(chainType), intent(inout), pointer  :: histogram(:)
  integer(4)                               :: irow,icol,jrow,jcol,ichain, icell
  integer(4)                               :: decimalNumber,nchunks,counter,total,id,clockIni,clockEnd
  integer(4)                               :: localChain(tem_rows*tem_cols), tem_cells_rows(tem_cells), tem_cells_cols(tem_cells)
  real(8)                                  :: totalinv    

  !nchunks=int(floor(real(tem_cells-1)*0.032258065))+1 
  
  counter=1
  total=(rows-tem_rows+1)*(cols-tem_cols+1) 
  totalinv=1.0/real(total)

  
  !remove this if running on serial
  ! this is done just to allocate internal variables in chainOperations
  !do icell = 1, tem_cells 
  !   localChain(icell)=matrix(1+tem_cells_rows(icell)-1,1+tem_cells_cols(icell)-1)  
  !end do
  !call chainInsertion(tem_cells,localChain)
  


  !!$OMP PARALLEL 
  id=0! OMP_get_thread_num() 
  !!$OMP DO PRIVATE(localChain,icol,irow,icell)
  !!     SHARED(matrix,tem_cells_cols,tem_cells_rows,chainArray,nchains,rootPos,counter,total,totalinv)
  do icol = 1,cols-tem_cols+1!,tem_cols
     do irow = 1,rows-tem_rows+1!,tem_rows
           !print *,'loading histogram(',id,')... ',counter,'/',total,' (',real(counter)*totalinv*100,'%)'
           do icell = 1, tem_cells 
                 localChain(icell)=matrix(irow+tem_cells_rows(icell)-1,icol+tem_cells_cols(icell)-1)  
           end do

           call system_clock(COUNT=clockIni)
           call chainInsertionV2(tem_cells,localChain)
           !call chainInsertion(tem_cells,localChain)
           call system_clock(COUNT=clockEnd)

           !call chainInsertionRealCodes(nchunks,tem_cells,localChain)

           !print *,'success(',id,')... ',counter,'/',total,' time=',real((real(clockEnd)-real(clockIni))/real(clock_rate)) 

           counter=counter+1
     end do
  end do
  !!$OMP END DO
  !!$OMP END PARALLEL

  !allocate(histogram(nchains))
  !do ichain=1,nchains
  !  print *,chainArray(ichain)%chain(:)
  !  print *,chainArray(ichain)%frequency
  !end do

end subroutine loadHistogramCells

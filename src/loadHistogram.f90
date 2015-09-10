
subroutine loadHistogram(matrix,rows,cols,tem_rows,tem_cols,clock_rate)

  use chainOperations

  integer(4), intent(in)                   :: rows, cols
  integer(4), intent(in)                   :: matrix(rows,cols)
  integer(4), intent(in)                   :: tem_rows, tem_cols
  integer(4), intent(in)                   :: clock_rate
  !type(chainType), intent(inout), pointer  :: histogram(:)
  integer(4)                               :: irow,icol,jrow,jcol,ichain
  integer(4)                               :: decimalNumber
  integer(4)                               :: localChain(tem_rows*tem_cols)

  do irow = 1,rows-tem_rows+1
     do icol = 1,cols-tem_cols+1
        !decimalNumber = 0
        !index = tem_rows*tem_cols 
        do jrow = 1,tem_rows
           do jcol = 1,tem_cols
              !index = index - 1
              !decimalNumber = decimalNumber + matrix(irow+jrow-1,icol+jcol-1) * 2**index
              localChain(jrow+tem_rows*(jcol-1))=matrix(irow+jrow-1,icol+jcol-1)  
           end do
        end do
        !decimalNumber = decimalNumber + 1
        !histogram(decimalNumber) = histogram(decimalNumber) + 1
        call chainInsertion(tem_rows*tem_cols,localChain)
     end do
  end do

  !allocate(histogram(nchains))
  !do ichain=1,nchains
  !  print *,chainArray(ichain)%chain(:)
  !  print *,chainArray(ichain)%frequency
  !end do

end subroutine loadHistogram

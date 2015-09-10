
subroutine printHistogram(realization_matrix,rows,cols,tem_rows, tem_cols, tem_cells,tem_cells_rows,tem_cells_cols,histoFile)

  use chainOperations

  integer(4), intent(in)    :: rows, cols
  integer(4), intent(in)    :: realization_matrix(rows,cols)
  integer(4), intent(in)    :: tem_rows, tem_cols
!  integer(4), intent(in)    :: histogramTI (2**(tem_rows*tem_cols))
!  integer(4), pointer       :: histogramRE (:)
  !integer(4)                :: histogramRE (2**(tem_rows*tem_cols))
  integer(4), intent(in)    :: tem_cells
  integer(4), intent(in)    :: tem_cells_rows(tem_cells),tem_cells_cols(tem_cells)
  character *50, intent(in) :: histoFile 
  integer(4)                :: ii,irow,icol,jrow,jcol,pos,sum1,sum2
  real(8)                   :: sum1inv,sum2inv
  integer(4)                :: freq_realization(nchains),localChain(tem_rows*tem_cols)
  real(8)                   :: inverseWeight,expo

  do ii=1,nchains
     freq_realization(ii)=0!chainArray(ii)%frequency
  end do
  
  !$OMP PARALLEL 
  !$OMP DO PRIVATE(localChain,pos,icol,irow,icell)
  !     SHARED(realization_matrix,tem_cells_cols,tem_cells_rows,freq_realization)
  do icol = 1,cols-tem_cols+1
     do irow = 1,rows-tem_rows+1
        do icell = 1, tem_cells 
              localChain(icell)=realization_matrix(irow+tem_cells_rows(icell)-1,icol+tem_cells_cols(icell)-1)  
        end do
        call chainSearch(tem_cells,localChain,pos)
        if(pos/=-1) then
           freq_realization(pos)=freq_realization(pos)+1
        end if
     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL
  
  open(unit=67, file=histoFile)
  
  !do ii=1,nchains
  !   write(67,*) chainArray(ii)%frequency, freq_realization(ii)
  !end do

  sum1=sum(chainArray(1:nchains)%frequency)
  sum2=sum(freq_realization(1:nchains))
  sum1inv=1.0/real(sum1)
  sum2inv=1.0/real(sum2)

  do ii=1,nchains
     write(67,*) real(chainArray(ii)%frequency)*sum1inv, real(freq_realization(ii))*sum2inv
  end do

  !sum1inv=0.0000001
  !sum2inv=1.0

  !write(67,*) sum1inv,sum1inv 
  !write(67,*) sum2inv,sum2inv 

  close(67)

end subroutine printHistogram 

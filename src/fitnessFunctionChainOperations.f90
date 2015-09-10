
subroutine fitnessFunctionChainOperations(realization_matrix,rows,cols,tem_rows, tem_cols, value)

  use chainOperations

  integer(4), intent(in)    :: rows, cols
  integer(4), intent(in)    :: realization_matrix(rows,cols)
  integer(4), intent(in)    :: tem_rows, tem_cols
!  integer(4), intent(in)    :: histogramTI (2**(tem_rows*tem_cols))
!  integer(4), pointer       :: histogramRE (:)
  !integer(4)                :: histogramRE (2**(tem_rows*tem_cols))
  integer(4), intent(out)   :: value
  integer(4)                :: ii,irow,icol,jrow,jcol,pos
  integer(4)                :: freq_realization(nchains),localChain(tem_rows*tem_cols)
  real(8)                   :: inverseWeight,expo

  inverseWeight=0.0
  do ii=1,nchains
     freq_realization(ii)=chainArray(ii)%frequency
     inverseWeight=inverseWeight + 1.0/(real(chainArray(ii)%frequency))
  end do
  
  value=0

  do irow = 1,rows-tem_rows+1
     do icol = 1,cols-tem_cols+1
        do jrow = 1,tem_rows
           do jcol = 1,tem_cols
              localChain(jrow+tem_rows*(jcol-1))=realization_matrix(irow+jrow-1,icol+jcol-1)  
           end do
        end do
        call chainSearch(tem_rows*tem_cols,localChain,pos)
        if(pos/=-1) then
           freq_realization(pos)=freq_realization(pos)-1
        !! norma1:
        !else
        !   value=value+1
        end if
     end do
  end do

  expo=1.0
  do ii=1,tem_rows
     expo=expo*10.0
  end do

  do ii=1,nchains
     !! norma2primaprima
     !print *,'ii=',ii,', adding: ',int( (( 1.0/real(chainArray(ii)%frequency))/inverseWeight) * real(abs(freq_realization(ii)))*real(abs(freq_realization(ii))) * real(rows) * real(cols)  ) 
     value=value + int( (( 1.0/real(chainArray(ii)%frequency))/inverseWeight) * real(abs(freq_realization(ii)))*real(abs(freq_realization(ii)))  * expo  ) 
     !! norma2prima:
     !value=value + freq_realization(ii)*freq_realization(ii) 
     !! norma1:
     !value=value +abs(freq_realization(ii)) 
  end do
  !print *,'value=',value
  ! print *,freq_realization(:)

end subroutine fitnessFunctionChainOperations


subroutine fitnessFunctionChainOperationsCellsUnroll(realization_matrix,rows,cols,tem_rows, tem_cols, value,tem_cells,tem_cells_rows,tem_cells_cols,unrolls,blockSize)

  use chainOperationsV2
  !use chainOperations

  integer(4), intent(in)     :: unrolls,blockSize

  integer(4), intent(in)    :: rows, cols
  integer(4), intent(in)    :: realization_matrix(rows,cols)
  integer(4), intent(in)    :: tem_rows, tem_cols
  integer(4), intent(out)   :: value
  integer(4), intent(in)    :: tem_cells
  integer(4), intent(in)    :: tem_cells_rows(tem_cells),tem_cells_cols(tem_cells)
  integer(4), target        :: aux_cells_rows(tem_cells),aux_cells_cols(tem_cells)
  integer(4)                :: ii,irow,icol,jrow,jcol,pos,tem_cells_rows_tmp,tem_cells_cols_tmp,colmin,colmax
  integer(4),dimension(tem_cells)::tem_cells_cols_tmp_array 
!  integer(4)                :: freq_realization(nchains),localChain(tem_rows*tem_cols),iblock,icheck

  integer(4)                ::freq_realization(nchains),localChain(tem_cells,unrolls),iblock,icheck
  integer(4),pointer        :: localChainAux(:)

!  integer(4)                ::freq_realization(nchains),iblock,icheck
!  integer(4),dimension(tem_cells,unrolls) ::localChain
  integer(4),dimension(unrolls,tem_cells),target ::localChainTranspose
!  integer(4) :: cell01,cell02,cell03,cell04,cell05,cell06,cell07,cell08,cell09,cell10
!  integer(4) :: cell11,cell12,cell13,cell14,cell15,cell16,cell17,cell18,cell19,cell20
  real(8)                   :: inverseWeight,expo

  inverseWeight=0.0

  print *, 'NCHAINS: ',nchains

  do ii=1,nchains
     !freq_realization(ii)=chainArray(ii)%frequency
     freq_realization(ii)=frequency(ii)
  end do
  
  value=0

  if(1==1)then
        
  !$OMP PARALLEL PRIVATE(localChain,pos,icol,irow,icell,tem_cells_rows_tmp,tem_cells_cols_tmp)  &
  !$OMP          SHARED(realization_matrix,tem_cells_cols,tem_cells_rows,freq_realization) 
  !$OMP DO 
  do icol = 0,cols-tem_cols
  !do iblock =0, blocks
  !colmin=int(iblock*(cols-tem_cols)/blocks)
  !colmax=int(min(int((iblock+1)*(cols-tem_cols)/blocks),cols-tem_cols))
  !do icol = colmin,colmax
     do irow = 0,rows-tem_rows,unrolls

        !localChain(1:4,1:10)=realization_matrix((irow+5):(9+irow+5),(icol+1):(icol+1+4)) 
        !do icell = 1, tem_cells
        do icell = 1, tem_cells
              tem_cells_rows_tmp =irow+tem_cells_rows(icell) 
              tem_cells_rows_tmp2=tem_cells_rows_tmp+unrolls-1
              tem_cells_cols_tmp=icol+tem_cells_cols(icell) 
              !tem_cells_cols_tmp=icol+tem_cells_cols(icell)+unrolls-1
              !localChain(icell,1:unrolls)=realization_matrix((0+tem_cells_rows_tmp):(unrolls-1+tem_cells_rows_tmp),tem_cells_cols_tmp)  
             !localChain(icell,1:unrolls)=realization_matrix(tem_cells_rows_tmp:tem_cells_rows_tmp,tem_cells_cols_tmp)  ! this is working
              !localChain(icell,1:unrolls)=realization_matrix(tem_cells_rows_tmp:tem_cells_rows_tmp2,tem_cells_cols_tmp)  
              localChainTranspose(1:unrolls,icell)=realization_matrix(tem_cells_rows_tmp:tem_cells_rows_tmp2,tem_cells_cols_tmp)  
!              localChain(icell,1:unrolls)=realization_matrix(tem_cells_rows_tmp:tem_cells_rows_tmp2,tem_cells_cols_tmp)  
        end do
        !localChain(14:17,1:10)=realization_matrix((irow+5):(9+irow+5),(icol+6):(icol+6+4)) 


        !localChain=transpose(localChainTranspose)



        do icheck=1,unrolls
           localChainAux => localChainTranspose(icheck,1:tem_cells)
           call chainSearchV2(tem_cells,localChainAux,pos)
           !call chainSearchV2(tem_cells,localChain(1:tem_cells,icheck),pos)
!           call chainSearch(tem_cells,localChain(1:tem_cells,icheck),pos)
           if(pos/=-1) then
              freq_realization(pos)=freq_realization(pos)-1
           end if
        end do

     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  end if


  if(1==0)then
  !$OMP PARALLEL PRIVATE(localChain,pos,icol,irow,icell,tem_cells_rows_tmp,tem_cells_cols_tmp)  &
  !$OMP          SHARED(realization_matrix,tem_cells_cols,tem_cells_rows,freq_realization) 
  !$OMP DO 
  do icol = 0,cols-tem_cols

     do icell = 1, tem_cells
        tem_cells_cols_tmp_array(icell)=icol+tem_cells_cols(icell)
     end do

     do irow = 0,rows-tem_rows,unrolls
        do icell = 1, tem_cells
              tem_cells_rows_tmp =irow+tem_cells_rows(icell) 
              tem_cells_rows_tmp2=tem_cells_rows_tmp+unrolls-1
              !tem_cells_cols_tmp=icol+tem_cells_cols(icell) 
              localChainTranspose(1:unrolls,icell)=realization_matrix(tem_cells_rows_tmp:tem_cells_rows_tmp2,tem_cells_cols_tmp_array(icell))  
!              localChain(icell,1:unrolls)=realization_matrix(tem_cells_rows_tmp:tem_cells_rows_tmp2,tem_cells_cols_tmp)  
        end do
        do icheck=1,unrolls
           localChainAux => localChainTranspose(icheck,1:tem_cells)
           call chainSearchV2(tem_cells,localChainAux,pos)
           !call chainSearchV2(tem_cells,localChain(1:tem_cells,icheck),pos)
           if(pos/=-1) then
              freq_realization(pos)=freq_realization(pos)-1
           end if
        end do

     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL
  end if

  do ii=1,nchains
     value=value + freq_realization(ii)*freq_realization(ii) 
  end do

end subroutine fitnessFunctionChainOperationsCellsUnroll

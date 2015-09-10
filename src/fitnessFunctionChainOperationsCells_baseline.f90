
subroutine fitnessFunctionChainOperationsCells_baseline(realization_matrix,rows,cols,tem_rows, tem_cols, value,tem_cells,tem_cells_rows,tem_cells_cols,unrolls,nblocks)

  use chainOperations

  integer(4), intent(in)     :: unrolls,nblocks
  integer(4), intent(in)    :: rows, cols
  integer(4), intent(in)    :: realization_matrix(rows,cols)
  integer(4), intent(in)    :: tem_rows, tem_cols
  integer(4), intent(out)   :: value
  integer(4), intent(in)    :: tem_cells
  integer(4), intent(in)    :: tem_cells_rows(tem_cells),tem_cells_cols(tem_cells)
  integer(4), target        :: aux_cells_rows(tem_cells),aux_cells_cols(tem_cells)
  integer(4)                :: ii,irow,icol,jrow,jcol,pos,tem_cells_rows_tmp,tem_cells_cols_tmp,irowmin,irowmax
  integer(4)                :: localChain(tem_rows*tem_cols)
  integer(4)                :: freq_realization(nchains)
  integer(4)                :: iblockCol,iblockRow,icheck,icolmin,icolmax,blockSizeCol,blockSizeRow,valuetmp
  integer(4)::rowplus1,rowplus2,rowplus3,rowplus5,rowplus6,rowplus7,rowplus11
  !integer(4)::row2plus1,row2plus4,row2plus5,row2plus6,row2plus9
  !integer(4)::row3plus1,row3plus4,row3plus5,row3plus6,row3plus9
  integer(4)::colplus1,colplus2,colplus3,colplus5,colplus6,colplus7,colplus11,id

  integer dotmulsimd
  integer dotsquaresimd

  real(8) :: valuereal


  do ii=1,nchains
     freq_realization(ii)=chainArray(ii)%frequency
  end do

  value=0
  valuetmp=0
  valuereal=0.0


  !$OMP PARALLEL PRIVATE(localChain,pos,icol,irow,icell)&
  !$OMP          SHARED(freq_realization) 
  !$OMP DO 
  do icol = 0,cols-tem_cols
     do irow = 0,rows-tem_rows
        do icell = 1, tem_cells
              localChain(icell)=realization_matrix(irow+tem_cells_rows(icell),icol+tem_cells_cols(icell))  
        end do
        call chainSearch(tem_cells,localChain,pos)
        if(pos/=-1) then
           freq_realization(pos)=freq_realization(pos)-1
        end if

     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  do ii=1,nchains
     value=value + freq_realization(ii)*freq_realization(ii) 
     !value=value + abs(freq_realization(ii)) 
  end do

end subroutine fitnessFunctionChainOperationsCells_baseline

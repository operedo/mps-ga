
subroutine fitnessFunctionChainOperationsCells(realization_matrix,rows,cols,tem_rows, tem_cols, value,tem_cells,tem_cells_rows,tem_cells_cols,unrolls,nblocks)

  use omp_lib
  use chainOperationsV2
  !use chainOperations

  integer(4), intent(in)     :: unrolls,nblocks
  integer(4), intent(in)    :: rows, cols
  integer(4), intent(in)    :: realization_matrix(rows,cols)
  integer(4), intent(in)    :: tem_rows, tem_cols
  !integer(4), intent(out)   :: value
  real(8), intent(out)   :: value
  integer(4), intent(in)    :: tem_cells
  integer(4), intent(in)    :: tem_cells_rows(tem_cells),tem_cells_cols(tem_cells)
  integer(4), target        :: aux_cells_rows(tem_cells),aux_cells_cols(tem_cells)
  integer(4)                :: ii,jj,irow,icol,jrow,jcol,pos,tem_cells_rows_tmp,tem_cells_cols_tmp,irowmin,irowmax
  integer(4)                :: localChain(tem_rows*tem_cols)
  integer(4)                :: freq_realization(nchains),freq_realization_abs(nchains),freq_realization_byid(nchains,12)
  integer(4)                :: iblockCol,iblockRow,icheck,icolmin,icolmax,blockSizeCol,blockSizeRow,valuetmp
  integer(4)::rowplus1,rowplus2,rowplus3,rowplus5,rowplus6,rowplus7,rowplus11
  !integer(4)::row2plus1,row2plus4,row2plus5,row2plus6,row2plus9
  !integer(4)::row3plus1,row3plus4,row3plus5,row3plus6,row3plus9
  integer(4)::colplus1,colplus2,colplus3,colplus5,colplus6,colplus7,colplus11,id
  integer(4) :: idthread,numthreads

  integer dotmulsimd
  integer dotsquaresimd

  real(8) :: valuereal

  freq_realization_byid=0

  do ii=1,nchains
     !freq_realization(ii)=chainArray(ii)%frequency
     freq_realization(ii)=frequency(ii)
     !freq_realization(ii)=0
  end do
  !freq_realization(nchains+1)=0  

  value=0
  valuetmp=0
  valuereal=0.0
  pos=-1


  !$OMP PARALLEL PRIVATE(localChain,pos,icol,irow,colplus1,colplus5,colplus6,colplus7,colplus11,rowplus1,rowplus5,rowplus6,rowplus7,rowplus11)
  idthread=omp_get_thread_num()
  idthread=idthread+1
  numthreads=omp_get_num_threads()
  !$OMP DO SCHEDULE(RUNTIME) 
  do icol = 0,cols-tem_cols

     colplus1=icol+1
     colplus5=icol+5
     colplus6=icol+6
     colplus7=icol+7
     colplus11=icol+11

!     colplus1=icol+1
!     colplus2=icol+2
!     colplus3=icol+3

     do irow = 0,rows-tem_rows
!        do icell = 1, tem_cells
!              localChain(icell)=realization_matrix(irow+tem_cells_rows(icell),icol+tem_cells_cols(icell))  
!        end do

        rowplus1=irow+1
        rowplus5=irow+5
        rowplus6=irow+6
        rowplus7=irow+7
        rowplus11=irow+11

        localChain(1)=realization_matrix(rowplus1,colplus1)  
        localChain(2)=realization_matrix(rowplus6,colplus1)  
        localChain(3)=realization_matrix(rowplus11,colplus1)  
        localChain(4)=realization_matrix(rowplus5,colplus5)  
        localChain(5)=realization_matrix(rowplus6,colplus5)  
        localChain(6)=realization_matrix(rowplus7,colplus5)  
        localChain(7)=realization_matrix(rowplus1,colplus6)  
        localChain(8)=realization_matrix(rowplus5,colplus6)  
        localChain(9)=realization_matrix(rowplus6,colplus6)  
        localChain(10)=realization_matrix(rowplus7,colplus6)  
        localChain(11)=realization_matrix(rowplus11,colplus6)  
        localChain(12)=realization_matrix(rowplus5,colplus7)  
        localChain(13)=realization_matrix(rowplus6,colplus7)  
        localChain(14)=realization_matrix(rowplus7,colplus7)  
        localChain(15)=realization_matrix(rowplus1,colplus11)  
        localChain(16)=realization_matrix(rowplus6,colplus11)  
        localChain(17)=realization_matrix(rowplus11,colplus11)  



!        rowplus1=irow+1
!        rowplus2=irow+2
!        rowplus3=irow+3
!
!        localChain(1)=realization_matrix(rowplus1,colplus1)  
!        localChain(2)=realization_matrix(rowplus2,colplus1)  
!        localChain(3)=realization_matrix(rowplus3,colplus1)  
!        localChain(4)=realization_matrix(rowplus1,colplus2)  
!        localChain(5)=realization_matrix(rowplus2,colplus2)  
!        localChain(6)=realization_matrix(rowplus3,colplus2)  
!        localChain(7)=realization_matrix(rowplus1,colplus3)  
!        localChain(8)=realization_matrix(rowplus2,colplus3)  
!        localChain(9)=realization_matrix(rowplus3,colplus3)  

        call chainSearchV2(tem_cells,localChain,pos)
        if(pos/=-1) then
           !freq_realization(pos)=freq_realization(pos)-1
           freq_realization_byid(pos,idthread)=freq_realization_byid(pos,idthread)+1
        !else
        !   valuetmp=valuetmp+1
        end if

     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  do jj=1,numthreads
     do ii=1,nchains
        freq_realization(ii)=freq_realization(ii)-freq_realization_byid(ii,jj)
     end do
  end do


  do ii=1,nchains
     !valuereal=valuereal + real(freq_realization(ii))*real(freq_realization(ii)) 
     value=value + real(freq_realization(ii))*real(freq_realization(ii))
     !value=value + abs(freq_realization(ii))
     !freq_realization_abs(ii) = abs(freq_realization(ii)) 
  end do
  !value = maxval(freq_realization_abs) + value

  !value=value+valuetmp*valuetmp

  !value=dotmulsimd(nchains,freq_realization,freq_realization)
  !value=dotsquaresimd(nchains,freq_realization)
  !value=valuetmp

end subroutine fitnessFunctionChainOperationsCells

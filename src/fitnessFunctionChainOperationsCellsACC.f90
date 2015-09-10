
subroutine fitnessFunctionChainOperationsCellsACC(realization_matrix,rows,cols,tem_rows, tem_cols, value,tem_cells,tem_cells_rows,tem_cells_cols,unrolls,nblocks)

  use chainOperationsV2
  use accel_lib
  !use chainOperations

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

  integer(4) :: minn,maxx,isFinded,ipos

  integer dotmulsimd
  integer dotsquaresimd

  real(8) :: valuereal


  do ii=1,nchains
     !freq_realization(ii)=chainArray(ii)%frequency
     freq_realization(ii)=frequency(ii)
     !freq_realization(ii)=0
  end do
  !freq_realization(nchains+1)=0  

  value=0
  valuetmp=0
  valuereal=0.0

!$acc data region copyin(realization_matrix)
!$acc region
  do icol = 0,cols-tem_cols

     colplus1=icol+1
     colplus5=icol+5
     colplus6=icol+6
     colplus7=icol+7
     colplus11=icol+11

!$acc do parallel
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

        !call chainSearchV2(tem_cells,localChain,pos)
        minn = 1
        maxx = nchains
        isFinded=0
        !do while ( minn <= maxx .and. isFinded == 0 )
        !do while ( minn <= maxx )
        do ipos= minn,maxx
           if(isFinded==0)then
              pos = int(real(minn+maxx)* 0.5)
              !auxtmp => chainArray(1:nrows,pos) 
   
              !call chainComparationV2(nrows,auxtmp,oneChain,value)
              valuetmp = 0
              icell = tem_cells + 1
              !do while ( valuetmp == 0 .and. icell > 1 )
              !do while ( icell > 1 )
              do icell=tem_cells,1,-1
   !              icell = icell - 1       
                 if(valuetmp==0)then
                    valuetmp=chainArray(icell,pos) - localChain(icell)
                 end if
                 !if(valuetmp/=0) icell=0
              end do
              !!!!!!!!!!!!!!!!!!!!
              if (valuetmp == 0) then
                 isFinded = 1
              elseif (valuetmp == 1) then
                 maxx = pos - 1
              else
                 minn = pos + 1
              end if
           end if
        end do
        if (isFinded == 0) pos = -1
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        if(pos/=-1) then
           freq_realization(pos)=freq_realization(pos)-1
        end if

     end do
  end do
!$acc end region
!$acc end data region


  do ii=1,nchains
     !valuereal=valuereal + real(freq_realization(ii))*real(freq_realization(ii)) 
     !value=value + freq_realization(ii)*freq_realization(ii) 
     value=value + abs(freq_realization(ii)) 
  end do

  !value=value+valuetmp*valuetmp

  !value=dotmulsimd(nchains,freq_realization,freq_realization)
  !value=dotsquaresimd(nchains,freq_realization)
  !value=valuetmp

end subroutine fitnessFunctionChainOperationsCellsACC

subroutine fitnessFunctionChainOperationsCellsMPI(realization_matrix,rows,cols,tem_rows, tem_cols, value,tem_cells,tem_cells_rows,tem_cells_cols,unrolls,nblocks)
  use mpi
  use chainOperationsV2
  integer(4), intent(in)     :: unrolls,nblocks
  integer(4), intent(in)    :: rows, cols
  integer(4), intent(in)    :: realization_matrix(rows,cols)
  integer(4), intent(in)    :: tem_rows, tem_cols
  integer(4), intent(out)   :: value
  integer(4), intent(in)    :: tem_cells
  integer(4), intent(in)    :: tem_cells_rows(tem_cells),tem_cells_cols(tem_cells)
  integer(4), target        :: aux_cells_rows(tem_cells),aux_cells_cols(tem_cells)
  integer(4)                :: ii,irow,icol,jrow,jcol,pos,tem_cells_rows_tmp,tem_cells_cols_tmp,irowmin,irowmax
  integer(4)                :: freq_realization(nchains),freq_realizationRecv(nchains),localChain(tem_rows*tem_cols)
  integer(4)                :: iblockCol,iblockRow,icheck,icolmin,icolmax,blockSizeCol,blockSizeRow,valuetmp
  integer(4),dimension(unrolls,tem_cells) ::localChainTranspose
  real(8)                   :: inverseWeight,expo
  integer(4)::rowplus1,rowplus4,rowplus5,rowplus6,rowplus9
  integer(4)::colplus1,colplus4,colplus5,colplus6,colplus9
  integer dotmulsimd
  integer dotsquaresimd
  integer(4)::ierr,p,id
  integer(4)    :: realization_matrixRecv(rows,cols)
  integer status(MPI_STATUS_SIZE)

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD,p,ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD,id,ierr)

  do ii=1,nchains
     !freq_realization(ii)=frequency(ii)
     freq_realization(ii)=0
  end do

  if(id==0)then
     do ii=1,p-1
     call MPI_Send(cols,1,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(rows,1,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(tem_cols,1,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(tem_rows,1,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(tem_cells,1,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(nchains,1,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(realization_matrix,rows*cols,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(freq_realization,nchains,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     call MPI_Send(chainArray,tem_cells*nchains,MPI_INTEGER,ii,0,MPI_COMM_WORLD,ierr)
     end do
!     icolmin=0
!     icolmax=
  else
     call MPI_Recv(cols,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(rows,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(tem_cols,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(tem_rows,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(tem_cells,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(nchains,1,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(realization_matrix,rows*cols,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(freq_realization,nchains,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
     call MPI_Recv(chainArray,tem_cells*nchains,MPI_INTEGER,0,0,MPI_COMM_WORLD,status,ierr)
  end if

if(id==0)then
  icolmin=id*(cols-tem_cols)/p
  icolmax=min((id+1)*(cols-tem_cols)/p,cols-tem_cols)
  print *,id,icolmin,icolmax
  value=0
  !$OMP PARALLEL PRIVATE(localChain,pos,icol,irow,colplus1,colplus4,colplus5,colplus6,colplus9,rowplus1,rowplus4,rowplus5,rowplus6,rowplus9)  &
  !$OMP          SHARED(freq_realization) 
  !$OMP DO 
  do icol = icolmin,icolmax
  !do icol = 0,cols-tem_cols
     colplus1=icol+1
     colplus4=icol+4
     colplus5=icol+5
     colplus6=icol+6
     colplus9=icol+9
     do irow = 0,rows-tem_rows
     !do irow = irowmin,irowmax
        rowplus1=irow+1
        rowplus4=irow+4
        rowplus5=irow+5
        rowplus6=irow+6
        rowplus9=irow+9
        localChain(1)=realization_matrix(rowplus1,colplus1)  
        localChain(2)=realization_matrix(rowplus5,colplus1)  
        localChain(3)=realization_matrix(rowplus9,colplus1)  
        localChain(4)=realization_matrix(rowplus4,colplus4)  
        localChain(5)=realization_matrix(rowplus5,colplus4)  
        localChain(6)=realization_matrix(rowplus6,colplus4)  
        localChain(7)=realization_matrix(rowplus1,colplus5)  
        localChain(8)=realization_matrix(rowplus4,colplus5)  
        localChain(9)=realization_matrix(rowplus5,colplus5)  
        localChain(10)=realization_matrix(rowplus6,colplus5)  
        localChain(11)=realization_matrix(rowplus9,colplus5)  
        localChain(12)=realization_matrix(rowplus4,colplus6)  
        localChain(13)=realization_matrix(rowplus5,colplus6)  
        localChain(14)=realization_matrix(rowplus6,colplus6)  
        localChain(15)=realization_matrix(rowplus1,colplus9)  
        localChain(16)=realization_matrix(rowplus5,colplus9)  
        localChain(17)=realization_matrix(rowplus9,colplus9)  
        call chainSearchV2(tem_cells,localChain,pos)
        if(pos/=-1) then
        freq_realization(pos)=freq_realization(pos)-1
        end if
     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  print *,"contrib:",id
  print *,freq_realization
  !do ii=1,nchains
  !   value=value+freq_realization(ii)*freq_realization(ii) 
  !end do
  !print *,id,value
else

  icolmin=id*(cols-tem_cols)/p
  icolmax=min((id+1)*(cols-tem_cols)/p,cols-tem_cols)
  print *,id,icolmin,icolmax
  value=0
  !$OMP PARALLEL PRIVATE(localChain,pos,icol,irow,colplus1,colplus4,colplus5,colplus6,colplus9,rowplus1,rowplus4,rowplus5,rowplus6,rowplus9)  &
  !$OMP          SHARED(freq_realization) 
  !$OMP DO 
  do icol = icolmin,icolmax
  !do icol = 0,cols-tem_cols
     colplus1=icol+1
     colplus4=icol+4
     colplus5=icol+5
     colplus6=icol+6
     colplus9=icol+9
     do irow = 0,rows-tem_rows
     !do irow = irowmin,irowmax
        rowplus1=irow+1
        rowplus4=irow+4
        rowplus5=irow+5
        rowplus6=irow+6
        rowplus9=irow+9
        localChain(1)=realization_matrix(rowplus1,colplus1)  
        localChain(2)=realization_matrix(rowplus5,colplus1)  
        localChain(3)=realization_matrix(rowplus9,colplus1)  
        localChain(4)=realization_matrix(rowplus4,colplus4)  
        localChain(5)=realization_matrix(rowplus5,colplus4)  
        localChain(6)=realization_matrix(rowplus6,colplus4)  
        localChain(7)=realization_matrix(rowplus1,colplus5)  
        localChain(8)=realization_matrix(rowplus4,colplus5)  
        localChain(9)=realization_matrix(rowplus5,colplus5)  
        localChain(10)=realization_matrix(rowplus6,colplus5)  
        localChain(11)=realization_matrix(rowplus9,colplus5)  
        localChain(12)=realization_matrix(rowplus4,colplus6)  
        localChain(13)=realization_matrix(rowplus5,colplus6)  
        localChain(14)=realization_matrix(rowplus6,colplus6)  
        localChain(15)=realization_matrix(rowplus1,colplus9)  
        localChain(16)=realization_matrix(rowplus5,colplus9)  
        localChain(17)=realization_matrix(rowplus9,colplus9)  
        call chainSearchV2(tem_cells,localChain,pos)
        if(pos/=-1) then
        freq_realization(pos)=freq_realization(pos)-1
        end if
     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  print *,"contrib:",id
  print *,freq_realization
  !do ii=1,nchains
  !   value=value+freq_realization(ii)*freq_realization(ii) 
  !end do
  !print *,id,value
end if  
  
  call MPI_Reduce(freq_realization,freq_realizationRecv,nchains,MPI_INTEGER,MPI_SUM,0,MPI_COMM_WORLD,ierr)
  
if(id==0)then
  print *,"reduce:",id
  print *,freq_realizationRecv

  do ii=1,nchains
     freq_realization(ii)=frequency(ii)-freq_realizationRecv(ii)
     value=value+freq_realization(ii)*freq_realization(ii) 
  end do
end if

  call MPI_Finalize(ierr)

end subroutine fitnessFunctionChainOperationsCellsMPI

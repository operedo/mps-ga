module chainOperations

  implicit none

  integer(4) :: nchains
  !integer(4) :: nchunks ! number of reals to be used in the codification
  type chainType
     integer(4), pointer :: chain(:)
     integer(4)          :: frequency 
     integer(4), pointer    :: code(:)
  end type chainType
  type(chainType), pointer :: chainArray(:) 

  integer(4) :: rootPos

contains
  
  subroutine chainComparation(nrows,oneChain1,oneChain2,value)
    !
    ! Compare two chains.
    ! If value = 0, both chains are equals
    ! If value = 1, the first chain is bigger
    ! If value = 2, the second chain is bigger
    !
    integer(4), intent(in)   :: nrows
    integer(4), intent(in)   :: oneChain1(nrows),oneChain2(nrows)
    integer(4), intent(out)  :: value
    integer(4)               :: irow
    !integer(4)               :: auxChain(nrows)
    
    value = 0
    irow = nrows + 1
    !write(*,*)'--------------------------------------------------'
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')oneChain1
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')oneChain2
    !auxChain=oneChain1-oneChain2  
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')auxChain
    !write(*,*)'norm=', norm(auxChain) 
    !if(norm(auxChain)==0.0) then
    !   value=0
    !else

    do while ( value == 0 .and. irow > 1 )
       irow = irow - 1       
       if (     oneChain1(irow) > oneChain2(irow) ) then
       !if (     auxChain(irow) > 0 ) then
          value = 1        
       elseif ( oneChain1(irow) < oneChain2(irow) ) then
       !elseif ( auxChain(irow) < 0 ) then
          value = 2
       end if
    end do

    !end if



  end subroutine chainComparation

  subroutine chainComparationRealCodes(nchunks,oneCode1,oneCode2,value)
    !
    ! Compare two chains.
    ! If value = 0, both chains are equals
    ! If value = 1, the first chain is bigger
    ! If value = 2, the second chain is bigger
    !
    integer(4), intent(in)   :: nchunks
    integer(4), intent(in)   :: oneCode1(nchunks),oneCode2(nchunks)
    integer(4), intent(out)  :: value
    integer(4)               :: ichunk
    !integer(4)               :: auxChain(nrows)
    
    value = 0
    ichunk = nchunks + 1
    !write(*,*)'--------------------------------------------------'
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')oneChain1
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')oneChain2
    !auxChain=oneChain1-oneChain2  
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')auxChain
    !write(*,*)'norm=', norm(auxChain) 
    !if(norm(auxChain)==0.0) then
    !   value=0
    !else

    do while ( value == 0 .and. ichunk > 1 )
       ichunk = ichunk - 1       
       if (     oneCode1(ichunk) > oneCode2(ichunk) ) then
       !if (     auxChain(irow) > 0 ) then
          value = 1        
       elseif ( oneCode1(ichunk) < oneCode2(ichunk) ) then
       !elseif ( auxChain(irow) < 0 ) then
          value = 2
       end if
    end do

    !end if

  end subroutine chainComparationRealCodes



  subroutine chainInsertion(nrows,oneChain)
    !
    ! Insert a chain into an array. The array is sorted in increasing order, i.e., array(1) < array(2) < ...
    !
    integer(4), intent(in)    :: nrows
    integer(4), intent(in)    :: oneChain(nrows)
    

    type(chainType), pointer  :: tempChainArray(:) 

    integer(4)                :: isNew, value
    integer(4)                :: pos,ichain,irow,newichain,istat,poslocal

    
    if (nchains == 0) then

       allocate(chainArray(1))
       allocate(chainArray(1)%chain(nrows))
       do irow = 1,nrows
          chainArray(1)%chain(irow) = oneChain(irow)
       end do
       chainArray(1)%frequency = 1
       
       nchains = 1
    else              

       pos = 0

       isNew    = 0
       do while (isNew == 0 .and. pos < nchains )
          pos = pos + 1
         
          call chainComparation(nrows,chainArray(pos)%chain(1:nrows),oneChain,value)

          if (value == 1) then
             pos = pos -1
             isNew = 1
          elseif (value == 0) then
             chainArray(pos)%frequency = chainArray(pos)%frequency + 1
             isNew = -1
          end if          
       end do

!!$OMP PARALLEL PRIVATE(poslocal,value) SHARED(isNew,pos,nchains,chainArray,oneChain)
!!$OMP DO
!       do poslocal=1,nchains-1
!          
!          if(isNew==0) then
!
!             call chainComparation(nrows,chainArray(poslocal)%chain(1:nrows),oneChain,value)
!
!!$OMP CRITICAL 
!             if (value == 1) then
!                pos = poslocal -1
!                isNew = 1
!             elseif (value == 0) then
!                chainArray(poslocal)%frequency = chainArray(poslocal)%frequency + 1
!                pos = poslocal
!                isNew = -1
!             end if          
!!$OMP END CRITICAL 
!
!          end if
!       end do
!!$OMP END DO
!!$OMP END PARALLEL

       if ( isNew /= -1 ) then

          allocate(tempChainArray(nchains), stat=istat)
          do ichain = 1, nchains
             allocate(tempChainArray(ichain)%chain(nrows), stat=istat)
          end do

!!$OMP PARALLEL PRIVATE(ichain) SHARED(nchains,tempChainArray,chainArray)
!!$OMP DO
          do ichain = 1,nchains
             !do irow = 1,nrows
             !   tempChainArray(ichain)%chain(irow) = chainArray(ichain)%chain(irow)
             !end do
             tempChainArray(ichain)%chain(1:nrows) = chainArray(ichain)%chain(1:nrows)
             tempChainArray(ichain)%frequency = chainArray(ichain)%frequency
          end do
!!$OMP END DO
!!$OMP END PARALLEL
          

          do ichain = 1, nchains
             deallocate(chainArray(ichain)%chain)
          end do
          deallocate(chainArray)

          allocate(chainArray(nchains+1), stat = istat)
          do ichain = 1, nchains+1
             allocate(chainArray(ichain)%chain(nrows), stat=istat)
          end do


!!$OMP PARALLEL PRIVATE(newichain) SHARED(pos,tempChainArray,chainArray)
!!$OMP DO
          do newichain = 1,pos
             !do irow = 1,nrows
             !    chainArray(newichain)%chain(irow) = tempChainArray(newichain)%chain(irow)
             !end do
             chainArray(newichain)%chain(1:nrows) = tempChainArray(newichain)%chain(1:nrows)
             chainArray(newichain)%frequency = tempChainArray(newichain)%frequency
          end do
!!$OMP END DO
!!$OMP END PARALLEL


          do irow = 1,nrows
             chainArray(pos+1)%chain(irow) = oneChain(irow)
          end do
          chainArray(pos+1)%frequency = 1!tempChainArray(pos)%frequency         


!!$OMP PARALLEL PRIVATE(newichain) SHARED(pos,nchains,tempChainArray,chainArray)
!!$OMP DO
          do newichain = pos+2,nchains+1
             !do irow = 1,nrows
             !   chainArray(newichain)%chain(irow) = tempChainArray(newichain-1)%chain(irow)
             !end do
             chainArray(newichain)%chain(1:nrows) = tempChainArray(newichain-1)%chain(1:nrows)
             chainArray(newichain)%frequency = tempChainArray(newichain-1)%frequency
          end do
!!$OMP END DO
!!$OMP END PARALLEL


          do ichain = 1, nchains
             deallocate(tempChainArray(ichain)%chain, stat=istat)
          end do
          deallocate(tempChainArray)

          nchains = nchains + 1
       end if                    
    endif
    rootPos = int(real(nchains) * 0.5)

  !if(mod(nchains,100)==0)then
  !   print *,'HISTOGRAM:'
  !   call printChainArray(nrows)
  !   print *,'--------------'
  !end if
    
  end subroutine chainInsertion

  subroutine printChainArray(nrows)
     integer(4),intent(in)  :: nrows
     integer(4)             :: ichain,irow

     print *,nchains
     do ichain=1,nchains
        print *,chainArray(ichain)%chain(1:nrows),' -- ',chainArray(ichain)%frequency
     end do

  end subroutine 

  !subroutine chainCalculateCodeChunks(codesize,chainsize,nchunks)
  !  integer(4),intent(in)  :: codesize,chainsize
  !  integer(4),intent(out) :: nchunks
  !  if(chainsize<codesize-1)then
  !     nchunks=1
  !  elseif(chainsize)
  !     nchunks=ceiling(chainsize)
  !  end if
  !end subroutine

  subroutine chainToRealCode(nchunks,nrows,oneChain,codetmp)
    integer(4) , intent(in)   :: nchunks,nrows
    integer(4) , intent(in)   :: oneChain(nrows)
    integer(4) , intent(inout):: codetmp(nchunks)
    integer(4)                :: ichunk,iilocal,ii
    do ichunk=1,nchunks
       codetmp(ichunk)=0
    end do
    !write(*,*)'chaintoRealCode: ',oneChain
    do ii=0, nrows-1
       ichunk=int(floor(real(ii)*0.032258065))+1
       iilocal=mod(ii,31)
       !write(*,*)'ichunk=',ichunk,', iilocal=',iilocal
       !write(*,*)'codetmp(',ichunk,')=',codetmp(ichunk),'+',oneChain(ii),'*',2**iilocal  
       codetmp(ichunk)=codetmp(ichunk)+oneChain(ii+1)*2**iilocal
    end do
    !write(*,*)'chaintoRealCode: ',codetmp
  end subroutine


  subroutine chainInsertionRealCodes(nchunks,nrows,oneChain)
    !
    ! Insert a chain into an array. The array is sorted in increasing order, i.e., array(1) < array(2) < ...
    !
    integer(4), intent(in)    :: nchunks
    integer(4), intent(in)    :: nrows
    integer(4)                :: ncodes
    integer(4), intent(in)    :: oneChain(nrows)
    

    type(chainType), pointer  :: tempChainArray(:) 

    integer(4)                :: isNew, value
    integer(4),pointer        :: codetmp(:)
    integer(4)                :: ii,pos,ichain,irow,newichain,istat,ichunk

    !nchunks=int(floor(real(nrows-1)/real(31)))+1 
    ! ncodes= ( (number of cells-1) * k) / 63  +1, it represents how many
                                  ! integers(8) are needed to codify this chain
                                  ! in this example, k=1
    allocate(codetmp(nchunks))

    if (nchains == 0) then

       allocate(chainArray(1))
       allocate(chainArray(1)%chain(nrows))
       allocate(chainArray(1)%code(nchunks))
       do irow = 1,nrows
          chainArray(1)%chain(irow) = oneChain(irow)
       end do

       call chainToRealCode(nchunks,nrows,oneChain,codetmp)
       do ichunk = 1,nchunks
          chainArray(1)%code(ichunk) = codetmp(ichunk)
       end do
       chainArray(1)%frequency = 1
       
       nchains = 1
    else              

       pos = 0

       isNew    = 0
       do while (isNew == 0 .and. pos < nchains )
          pos = pos + 1
         
          call chainComparation(nrows,chainArray(pos)%chain(1:nrows),oneChain,value)
          !call chainComparationRealCodes(nchunks,chainArray(pos)%codes(1:nchunks),oneChain,value)

          if (value == 1) then
             pos = pos -1
             isNew = 1
          elseif (value == 0) then
             chainArray(pos)%frequency = chainArray(pos)%frequency + 1
             isNew = -1
          end if          
       end do
       if ( isNew /= -1 ) then

          allocate(tempChainArray(nchains), stat=istat)
          do ichain = 1, nchains
             allocate(tempChainArray(ichain)%chain(nrows), stat=istat)
             allocate(tempChainArray(ichain)%code(nchunks), stat=istat)
          end do

          do ichain = 1,nchains
             do irow = 1,nrows
                tempChainArray(ichain)%chain(irow) = chainArray(ichain)%chain(irow)
             end do
             do ichunk = 1,nchunks
                tempChainArray(ichain)%code(ichunk) = chainArray(ichain)%code(ichunk)
             end do
             tempChainArray(ichain)%frequency = chainArray(ichain)%frequency
          end do
          

          do ichain = 1, nchains
             deallocate(chainArray(ichain)%chain)
             deallocate(chainArray(ichain)%code)
          end do
          deallocate(chainArray)

          allocate(chainArray(nchains+1), stat = istat)
          do ichain = 1, nchains+1
             allocate(chainArray(ichain)%chain(nrows), stat=istat)
             allocate(chainArray(ichain)%code(nchunks), stat=istat)
          end do

          do newichain = 1,pos
             do irow = 1,nrows
                 chainArray(newichain)%chain(irow) = tempChainArray(newichain)%chain(irow)
             end do
             do ichunk = 1,nchunks
                 chainArray(newichain)%code(ichunk) = tempChainArray(newichain)%code(ichunk)
             end do
             chainArray(newichain)%frequency = tempChainArray(newichain)%frequency
          end do

          do irow = 1,nrows
             chainArray(pos+1)%chain(irow) = oneChain(irow)
          end do
          !do ichunk = 1,nchunks
          !   chainArray(pos+1)%code(ichunk) = oneChain(irow)
          !end do
          call chainToRealCode(nchunks,nrows,oneChain,codetmp)
          do ichunk = 1,nchunks
             chainArray(pos+1)%code(ichunk) = codetmp(ichunk)
          end do
          chainArray(pos+1)%frequency = 1!tempChainArray(pos)%frequency         

          do newichain = pos+2,nchains+1
             do irow = 1,nrows
                chainArray(newichain)%chain(irow) = tempChainArray(newichain-1)%chain(irow)
             end do
             do ichunk = 1,nchunks
                chainArray(newichain)%code(ichunk) = tempChainArray(newichain-1)%code(ichunk)
             end do
             chainArray(newichain)%frequency = tempChainArray(newichain-1)%frequency
          end do

          do ichain = 1, nchains
             deallocate(tempChainArray(ichain)%chain, stat=istat)
             deallocate(tempChainArray(ichain)%code, stat=istat)
          end do
          deallocate(tempChainArray)

          nchains = nchains + 1
       end if                    
    endif
    rootPos = int(real(nchains) * 0.5)
    deallocate(codetmp)

  end subroutine chainInsertionRealCodes


  subroutine chainSearch(nrows,oneChain,pos)

    integer(4), intent(in)   :: nrows
    integer(4), intent(in)   :: oneChain(nrows)
    integer(4), intent(out)  :: pos
    integer(4)               :: isFinded,minn,maxx,value,ii,jj

    minn = 1
    maxx = nchains
   
    !print *,nchains 


    !write(*,*)'-------------------------------'
    !write(*,*)'Finding:'
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')oneChain 
    !write(*,*)'Into:'
    !do ii =1,nchains
    !    write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')chainArray(ii)%chain(1:nrows) 
    !end do
    !write(*,*)'-------------------------------'


    isFinded=0
 
    do while ( minn <= maxx .and. isFinded == 0 )
       pos = int(real(minn+maxx)* 0.5)
       !print *,'(while) pos=',pos
       call chainComparation(nrows,chainArray(pos)%chain(1:nrows),oneChain,value)
       !print *,'(while) pos=',pos
       !print *,'(while) value=',value
       if (value == 0) then
          isFinded = 1
       elseif (value == 1) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do

    if (isFinded == 0) pos = -1

  end subroutine chainSearch

  subroutine chainSearchRealCodes(nchunks,nrows,oneChain,pos)

    integer(4), intent(in)   :: nchunks,nrows
    integer(4), intent(in)   :: oneChain(nrows)
    integer(4), intent(out)  :: pos
    integer(4)               :: isFinded,minn,maxx,value,ii,jj
    integer(4),pointer       :: codetmp(:)


    !nchunks=int(floor(real(nrows-1)/real(31)))+1 ! ncodes= ( (number of cells-1) * k) / 63  +1, it represents how many
                                  ! integers(8) are needed to codify this chain
                                  ! in this example, k=1
    allocate(codetmp(nchunks))
    call chainToRealCode(nchunks,nrows,oneChain,codetmp)

    minn = 1
    maxx = nchains
   
    !print *,nchains 


    !write(*,*)'-------------------------------'
    !write(*,*)'Finding:'
    !write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')oneChain 
    !write(*,*)'Into:'
    !do ii =1,nchains
    !    write(*,'(I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2,I2)')chainArray(ii)%chain(1:nrows) 
    !end do
    !write(*,*)'-------------------------------'


    isFinded=0
 
    do while ( minn <= maxx .and. isFinded == 0 )
       pos = int(real(minn+maxx)* 0.5)
       !print *,'(while) pos=',pos
       call chainComparationRealCodes(nchunks,chainArray(pos)%code(1:nchunks),codetmp,value)
       !print *,'(while) pos=',pos
       !print *,'(while) value=',value
       if (value == 0) then
          isFinded = 1
       elseif (value == 1) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do

    if (isFinded == 0) pos = -1

    deallocate(codetmp)
  end subroutine chainSearchRealCodes


end module chainOperations

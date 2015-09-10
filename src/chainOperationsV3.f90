module chainOperationsV3

  implicit none

  integer(4) :: nchains
  !integer(4) :: nchunks ! number of reals to be used in the codification
  !type chainType
  !   integer(4), pointer :: chain(:)
  !   integer(4)          :: frequency 
  !   integer(4), pointer    :: code(:)
  !end type chainType
  !type(chainType), pointer :: chainArray(:) 
  
  integer(4), pointer :: chainArray(:,:)
  integer(4), pointer :: frequency(:)

  integer(4) :: rootPos

  integer(4) :: indexmmin(8), indexmmax(8)

contains

!  subroutine chainCreateIndex(nrows)
!    integer(4), intent(in)  :: nrows 
!    integer(4)              :: i1,i2,i3
!    integer(4)              :: limitsup,limitinf,diff
!    integer(4)              :: mig01,mig0_01,mig1_01,mig0_0_01,mig0_1_01,mig1_0_01,mig1_1_01
!
!    limitinf=1
!    diff=chainArray(nrows,1)
!    i1=1
!    do while(i1<=nchains)
!       if(chainArray(nrows,i1)/=diff)then
!          mig01=i1-1
!       end if
!    end do
!
!    i1=1
!    diff=chainArray(2,1)
!    do while(i1<=mig01)
!       if(chainArray(i1)/=diff)then
!          mig01=i1-1
!       end if
!    end do
!
!  end subroutine chainCreateIndex
  
  subroutine chainComparationV3(nrows,oneChain1,oneChain2,value)
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
    !character                :: string1*17, string2*17
    integer(4)               :: auxChain(nrows),aux
    

    !write(string1,'(17I1)')oneChain1
    !write(string2,'(17I1)')oneChain2
    !
    !print *,string1
    !print *,string2
    !
    !value=0
    !if (string1 .eq. string2) value=0
    !if (string1 .lt. string2) value=1
    !if (string1 .gt. string2) value=2
    ! 
    !print *,value 

!    value = 0
!    irow = nrows + 1
!    do while ( value == 0 .and. irow > 1 )
!       irow = irow - 1       
!       if (     oneChain1(irow) > oneChain2(irow) ) then
!          value = 1        
!       elseif ( oneChain1(irow) < oneChain2(irow) ) then
!          value = 2
!       end if
!    end do

    value = 0
    !irow = nrows + 1
    irow = 0
    do while ( value == 0 .and. irow > 1 )
       !irow = irow - 1       
       irow = irow + 1       
       value=oneChain1(irow) - oneChain2(irow)
       !if ( aux>=0 ) then
       !   value = 1        
       !else
       !   value = 2
       !end if
    end do
    !if(value==-1)then
    !   value=2
    !end if

  end subroutine chainComparationV3


  subroutine chainInsertionV3(nrows,oneChain)
    !
    ! Insert a chain into an array. The array is sorted in increasing order, i.e., array(1) < array(2) < ...
    !
    integer(4), intent(in)    :: nrows
    integer(4), intent(in)    :: oneChain(nrows)
    
    integer(4),pointer        :: tempChainArray(:,:)    
    integer(4),pointer        :: tempFrequency(:)    

    integer(4)                :: isNew, value
    integer(4)                :: pos,ichain,irow,newichain,istat,poslocal

    
    if (nchains == 0) then

       allocate(chainArray(nrows,1))
       allocate(frequency(1))
       chainArray(1:nrows,1) = oneChain(1:nrows)
       frequency(1) = 1
       
       nchains = 1
    else              

       pos = 0

       isNew    = 0
       do while (isNew == 0 .and. pos < nchains )
          pos = pos + 1
         
          call chainComparationV3(nrows,chainArray(1:nrows,pos),oneChain,value)

          if (value == 1) then
             pos = pos -1
             isNew = 1
          elseif (value == 0) then
             frequency(pos) = frequency(pos) + 1
             isNew = -1
          end if          
       end do

       if ( isNew /= -1 ) then

          allocate(tempChainArray(nrows,nchains))
          allocate(tempFrequency(nchains))

          !do ichain = 1,nchains
          !   tempChainArray(1:nrows,ichain) = chainArray(1:nrows,ichain)
          !   tempFrequency(ichain) = frequency(ichain)
          tempChainArray(1:nrows,1:nchains) = chainArray(1:nrows,1:nchains)
          tempFrequency(1:nchains) = frequency(1:nchains)
          !end do

          deallocate(chainArray)
          deallocate(frequency)

          allocate(chainArray(nrows,nchains+1), stat = istat)
          allocate(frequency(nchains+1), stat = istat)

          !do newichain = 1,pos
          !   chainArray(1:nrows,newichain) = tempChainArray(1:nrows,newichain)
          !   frequency(newichain) = tempFrequency(newichain) 
          chainArray(1:nrows,1:pos) = tempChainArray(1:nrows,1:pos)
          frequency(1:pos) = tempFrequency(1:pos) 
          !end do


          !do irow = 1,nrows
          !   chainArray(irow,pos+1) = oneChain(irow)
          chainArray(1:nrows,pos+1) = oneChain(1:nrows)
          !end do
          frequency(pos+1) = 1!tempChainArray(pos)%frequency         


          !do newichain = pos+2,nchains+1
          !   chainArray(1:nrows,newichain) = tempChainArray(1:nrows,newichain-1)
          !   frequency(newichain) = tempFrequency(newichain-1)
          chainArray(1:nrows,pos+2:nchains+1) = tempChainArray(1:nrows,pos+1:nchains)
          frequency(pos+2:nchains+1) = tempFrequency(pos+1:nchains)
          !end do

          deallocate(tempChainArray)
          deallocate(tempFrequency)

          nchains = nchains + 1
       end if                    
    endif
    rootPos = int(real(nchains) * 0.5)

  !if(mod(nchains,100)==0)then
  !   print *,'HISTOGRAM:'
  !   call printChainArray(nrows)
  !   print *,'--------------'
  !end if
    
  end subroutine chainInsertionV3

  subroutine printChainArrayV3(nrows)
     integer(4),intent(in)  :: nrows
     integer(4)             :: ichain,irow

     print *,nchains
     do ichain=1,nchains
        print *,chainArray(1:nrows,ichain),' -- ',frequency(ichain)
     end do

  end subroutine 

  subroutine chainSearchV3(nrows,oneChain,pos)
  
    !use omp_lib

    integer(4), intent(in)   :: nrows
    integer(4), intent(in)   :: oneChain(nrows)
    integer(4), intent(out)  :: pos
    integer(4)               :: isFinded,minn,maxx,value,ii,jj,pos2
    integer(4),pointer       :: auxtmp(:)
    !integer(4)               :: num_threads,id,block

    !num_threads=OMP_get_num_threads() 
    !id= OMP_get_thread_num() 
    !block=nchains/num_threads

    !allocate(auxtmp(nrows))

    minn = 1
    maxx = nchains
    !minn = id*block +1
    !maxx = min((id+1)*block, nchains)
   
    isFinded=0
 
    do while ( minn <= maxx .and. isFinded == 0 )
       pos = int(real(minn+maxx)* 0.5)
       auxtmp => chainArray(1:nrows,pos) 
       !call chainComparationV3(nrows,chainArray(1:nrows,pos),oneChain,value)
       call chainComparationV3(nrows,auxtmp,oneChain,value)
       if (value == 0) then
          isFinded = 1
       elseif (value == 1) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do

    !pos2=1
    !pos=nchains+1
    !do ii=1,nchains
    !   call chainComparationV3(nrows,chainArray(1:nrows,ii),oneChain,value)
    !   pos2=min(pos2*value,pos2)
    !   pos=min(ii*(1-pos2),pos)
    !end do

    if (isFinded == 0) pos = -1
    !if (pos == 0) pos = -1

    !deallocate(auxtmp)

  end subroutine chainSearchV3

end module chainOperationsV3

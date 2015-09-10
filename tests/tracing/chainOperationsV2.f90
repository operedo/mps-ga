module chainOperationsV2

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

contains
  
  subroutine chainComparationV2(nrows,oneChain1,oneChain2,value)
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


    value=oneChain1(17) - oneChain2(17)
    if(value/=0)return
    value=oneChain1(16) - oneChain2(16)
    if(value/=0)return
    value=oneChain1(15) - oneChain2(15)
    if(value/=0)return
    value=oneChain1(14) - oneChain2(14)
    if(value/=0)return
    value=oneChain1(13) - oneChain2(13)
    if(value/=0)return
    value=oneChain1(12) - oneChain2(12)
    if(value/=0)return
    value=oneChain1(11) - oneChain2(11)
    if(value/=0)return
    value=oneChain1(10) - oneChain2(10)
    if(value/=0)return
    value=oneChain1(9) - oneChain2(9)
    if(value/=0)return
    value=oneChain1(8) - oneChain2(8)
    if(value/=0)return
    value=oneChain1(7) - oneChain2(7)
    if(value/=0)return
    value=oneChain1(6) - oneChain2(6)
    if(value/=0)return
    value=oneChain1(5) - oneChain2(5)
    if(value/=0)return
    value=oneChain1(4) - oneChain2(4)
    if(value/=0)return
    value=oneChain1(3) - oneChain2(3)
    if(value/=0)return
    value=oneChain1(2) - oneChain2(2)
    if(value/=0)return
    value=oneChain1(1) - oneChain2(1)
    if(value/=0)return

!    value = 0
!    irow = nrows + 1
!    do while ( value == 0 .and. irow > 1 )
!       irow = irow - 1       
!       value=oneChain1(irow) - oneChain2(irow)
!    end do

  end subroutine chainComparationV2


  subroutine chainInsertionV2(nrows,oneChain)
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
         
          call chainComparationV2(nrows,chainArray(1:nrows,pos),oneChain,value)

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
    
  end subroutine chainInsertionV2

  subroutine printChainArrayV2(nrows)
     integer(4),intent(in)  :: nrows
     integer(4)             :: ichain,irow

     print *,nchains
     do ichain=1,nchains
        print *,chainArray(1:nrows,ichain),' -- ',frequency(ichain)
     end do

  end subroutine 

  subroutine chainSearchV2(nrows,oneChain,pos)
  
    integer(4), intent(in)   :: nrows
    integer(4), intent(in)   :: oneChain(nrows)
    integer(4), intent(out)  :: pos
    integer(4)               :: isFinded,minn,maxx,value,ii,jj,pos2
    integer(4),pointer       :: auxtmp(:)
    integer(4)               :: oc1,oc2,oc3,oc4,oc5,oc6,oc7,oc8,oc9,oc10,oc11
    integer(4)               :: oc12,oc13,oc14,oc15,oc16,oc17

!!
    oc1=oneChain(1)
    oc2=oneChain(2)
    oc3=oneChain(3)
    oc4=oneChain(4)
    oc5=oneChain(5)
    oc6=oneChain(6)
    oc7=oneChain(7)
    oc8=oneChain(8)
    oc9=oneChain(9)
    oc10=oneChain(10)
    oc11=oneChain(11)
    oc12=oneChain(12)
    oc13=oneChain(13)
    oc14=oneChain(14)
    oc15=oneChain(15)
    oc16=oneChain(16)
    oc17=oneChain(17)
!!


    minn = 1
    maxx = nchains
   
    isFinded=0
    
    value=-1
    pos=-1 
    do while ( minn <= maxx .and. isFinded == 0 )
    !do while ( minn <= maxx .and. value /= 0 )
       pos = int(real(minn+maxx)* 0.5)

       !auxtmp => chainArray(1:nrows,pos) 
       !call chainComparationV2(nrows,auxtmp,oneChain,value)

!!
       do while (.true.)

       value=chainArray(17,pos) - oc17
       if(value/=0)exit
       value=chainArray(16,pos) - oc16
       if(value/=0)exit
       value=chainArray(15,pos) - oc15
       if(value/=0)exit
       value=chainArray(14,pos) - oc14
       if(value/=0)exit
       value=chainArray(13,pos) - oc13
       if(value/=0)exit
       value=chainArray(12,pos) - oc12
       if(value/=0)exit
       value=chainArray(11,pos) - oc11
       if(value/=0)exit
       value=chainArray(10,pos) - oc10
       if(value/=0)exit
       value=chainArray(9,pos) - oc9
       if(value/=0)exit
       value=chainArray(8,pos) - oc8
       if(value/=0)exit
       value=chainArray(7,pos) - oc7
       if(value/=0)exit
       value=chainArray(6,pos) - oc6
       if(value/=0)exit
       value=chainArray(5,pos) - oc5
       if(value/=0)exit
       value=chainArray(4,pos) - oc4
       if(value/=0)exit
       value=chainArray(3,pos) - oc3
       if(value/=0)exit
       value=chainArray(2,pos) - oc2
       if(value/=0)exit
       value=chainArray(1,pos) - oc1
       if(value/=0)exit

       if(value==0)exit
       end do
!!

       if (value == 0) then
          isFinded = 1
       elseif (value == 1) then
       !if (value == 1) then
          maxx = pos - 1
       !elseif(value==-1)then
       else
          minn = pos + 1
       end if
    end do

    if (isFinded == 0) pos = -1


  end subroutine chainSearchV2

end module chainOperationsV2

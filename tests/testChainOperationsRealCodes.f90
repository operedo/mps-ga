program testChainOperationsRealCodes


  use chainOperations

  implicit none


  integer(4)               :: oneChain1(5),oneChain2(5),oneChain3(5),oneChain4(5),oneChain5(5),oneChain6(5),oneChain7(5),oneChain8(5),oneChain9(5) 
  integer(4)               :: ichain,oneValue,pos,nchunks,nrows
  
  oneChain1 = (/ 0, 0, 0, 0, 0 /)
  oneChain2 = (/ 0, 0, 0, 0, 1 /)
  oneChain3 = (/ 0, 0, 0, 1, 1 /)
  oneChain4 = (/ 0, 1, 0, 1, 1 /)
  oneChain5 = (/ 1, 0, 0, 1, 1 /)
  oneChain6 = (/ 1, 1, 0, 1, 1 /)
  oneChain7 = (/ 1, 1, 1, 1, 1 /)

  oneChain8 = (/ 1, 1, 1, 1, 0 /)
  oneChain9 = (/ 1, 0, 0, 0, 0 /)


  nchains = 0
  nrows=5
  nchunks=int(floor(real(nrows-1)/real(31)))+1

  print *,"nchunks=",nchunks

  call chainInsertionRealCodes(nchunks,5,oneChain1)
  call chainInsertionRealCodes(nchunks,5,oneChain2)
  call chainInsertionRealCodes(nchunks,5,oneChain3)
  call chainInsertionRealCodes(nchunks,5,oneChain4)
  call chainInsertionRealCodes(nchunks,5,oneChain5)
  call chainInsertionRealCodes(nchunks,5,oneChain6)
  call chainInsertionRealCodes(nchunks,5,oneChain7)       
  call chainInsertionRealCodes(nchunks,5,oneChain1)
  call chainInsertionRealCodes(nchunks,5,oneChain2)
  call chainInsertionRealCodes(nchunks,5,oneChain3)
  call chainInsertionRealCodes(nchunks,5,oneChain4)
  call chainInsertionRealCodes(nchunks,5,oneChain5)
  call chainInsertionRealCodes(nchunks,5,oneChain6)
  call chainInsertionRealCodes(nchunks,5,oneChain7)
  call chainInsertionRealCodes(nchunks,5,oneChain7)
  call chainInsertionRealCodes(nchunks,5,oneChain7)

  do ichain = 1,nchains
     print *, ""
     print *, "***************************** chain: ",ichain,"*****************************" 
     print *, "frequency: ",chainArray(ichain)%frequency
     print *, "chain:     ",chainArray(ichain)%chain(1:5)
     print *, "code:     ",chainArray(ichain)%code(1:nchunks)
     print *, "*******************************************************************************"
  end do

  call chainSearch(5,oneChain6,pos)
     print *, ""
     print *, ""
     print *, ""
     print *, "*******************************************************************************"
     print *, "position: ",pos
     print *, "chain:    ",oneChain6
     print *, "*******************************************************************************"

  call chainSearch(5,oneChain8,pos)
     print *, ""
     print *, "*******************************************************************************"
     print *, "position: ",pos
     print *, "chain:    ",oneChain8
     print *, "*******************************************************************************"

  call chainSearch(5,oneChain9,pos)
     print *, ""
     print *, "*******************************************************************************"
     print *, "position: ",pos
     print *, "chain:    ",oneChain9
     print *, "*******************************************************************************"



end program testChainOperationsRealCodes

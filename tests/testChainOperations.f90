program testChainOperations


  use chainOperations

  implicit none


  integer(4)               :: oneChain1(5),oneChain2(5),oneChain3(5),oneChain4(5),oneChain5(5),oneChain6(5),oneChain7(5),oneChain8(5)
  integer(4)               :: ichain,oneValue,pos
  
  oneChain1 = (/ 0, 0, 255, 0, 0 /)
  oneChain2 = (/ 0, 0, 0, 0, 1 /)
  oneChain3 = (/ 0, 0, 0, 1, 1 /)
  oneChain4 = (/ 1, 0, 0, 1, 255 /)
  oneChain5 = (/ 0, 1, 0, 1, 1 /)
  oneChain6 = (/ 1, 1, 2, 1, 2 /)
  oneChain7 = (/ 1, 1, 0, 1, 2 /)

  oneChain8 = (/ 1, 1, 1, 1, 2 /)


  nchains = 0
  call chainInsertion(5,oneChain1)
  call chainInsertion(5,oneChain1)
  call chainInsertion(5,oneChain1)
  call chainInsertion(5,oneChain2)
  call chainInsertion(5,oneChain2)
  call chainInsertion(5,oneChain3)
  call chainInsertion(5,oneChain3)
  call chainInsertion(5,oneChain4)
  call chainInsertion(5,oneChain4)
  call chainInsertion(5,oneChain5)
  call chainInsertion(5,oneChain5)
  call chainInsertion(5,oneChain6)
  call chainInsertion(5,oneChain6)
  call chainInsertion(5,oneChain7)       
  call chainInsertion(5,oneChain7)
  call chainInsertion(5,oneChain7)
  call chainInsertion(5,oneChain7)

  do ichain = 1,nchains
     print *, "***************************** chain: ",ichain,"*****************************" 
     print *, "frequency: ",chainArray(ichain)%frequency
     print *, "chain:     ",chainArray(ichain)%chain(1:5)
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



end program testChainOperations

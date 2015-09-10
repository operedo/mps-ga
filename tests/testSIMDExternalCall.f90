program testSIMDExternalCall
 
  implicit none

  integer(4),allocatable :: A(:),B(:)
  integer(4) :: clock_start, clock_end, clock_rate, n,i,j
  integer(4) :: res  
  integer dotmulsimd
  integer dotsquaresimd

  call system_clock(COUNT_RATE=clock_rate)

  n=936
  !n=10000

  allocate(A(n))
  allocate(B(n))

  do i=1,n
    A(i)=2
    B(i)=2
  end do
  
  call system_clock(COUNT=clock_start)
  do i=1,1000000
     !res=dotmulsimd(n,A,B)
     res=dotsquaresimd(n,A)
     !res=0
     !do j=1,n
     !   res=res+A(j)*B(j)
     !end do
     !res=dot_product(A,B)
  end do
  call system_clock(COUNT=clock_end)
  
  !res=dotsquaresimd(n,A)
  print *, "CLOCK=",real((real(clock_end)-real(clock_start))/real(clock_rate))
  print *,res

  deallocate(A)
  deallocate(B)

end program testSIMDExternalCall

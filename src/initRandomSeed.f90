!> initRandomSeed.f90
!> @file initRandomSeed.f90 
!> @fn initRandomSeed 
!> Text automatically added for recognition of doxygen parser
!>
subroutine initRandomSeed()
  INTEGER :: i, n, clock,clock_rate
  INTEGER, DIMENSION(:), ALLOCATABLE :: seed
  
  CALL RANDOM_SEED(size = n)
  ALLOCATE(seed(n))
  
  CALL SYSTEM_CLOCK(COUNT=clock)

  CALL system_clock(COUNT_RATE=clock_rate)
  
!  print *,clock,clock_rate

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)

!  print *,'seed=',seed

  CALL RANDOM_SEED(PUT = seed)
  
  DEALLOCATE(seed)
end subroutine initRandomSeed

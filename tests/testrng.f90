program testrng

  use rng
  use omp_lib

  implicit none

  integer, parameter :: nmc = 100       ! number of trials
  integer, parameter :: n = 100000      ! sample size
  real, dimension(nmc) :: mu            ! result of each trial
  real :: mean, stdev                   ! mean and standard deviation
  integer :: j
  type(rng_t), dimension(nmc) :: rngval

  !$OMP PARALLEL DO
  do j = 1, nmc
     print *, 'Experiment', j, ' by thread ',omp_get_thread_num()
     call rng_seed(rngval(j), 932117 + j)
     mu(j) = monte_carlo(rngval(j),n)
  end do
  !$OMP END PARALLEL DO

  mean = sum(mu) / dble(nmc)
  stdev = sqrt( sum( (mu - mean)**2 ) ) / dble(nmc)

  print *, 'mean', mean
  print *, 'stdev', stdev

contains

  ! Draws n Uniform(0,1) random numbers and returns the sample mean
  function monte_carlo(rng, n) result(y)


     type(rng_t), intent(inout) :: rng
     integer, intent(in) :: n
     integer :: i
     real :: y, u

     y = 0.d0
     do i = 1, n
        u = rng_uniform(rng)           ! draw from Uniform(0,1)
        y = y + u                      ! sum the draws
     end do
     y = y / dble(n)                   ! return the sample mean
  end function monte_carlo
end program testrng

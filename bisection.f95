program bisection
  ! Nicholas Maynard
  ! CSI 501
  ! Lab 8
  ! 03/23/2023
  ! This program performs a bisection for the 

  ! Clear out the memory
  implicit none

  ! Declare our Variables
  real :: a, b, m, Tolerance, MyFunc

  ! Ask the user for our input files
  print*,'Enter a number a:'
  read*, a
  print*,'Enter a number b:'
  read*, b

  Tolerance = 1.0e-5

  ! Check for if a root exists
  if (MyFunc(a) * MyFunc(b) .ge. 0.0) then 
    print*, "x-values do not produce opposite signs! quitting."
    stop 
  endif

  do
    m = (a + b) / 2.0 ! Computes x midpoint
    if (abs(MyFunc(m)) .lt. Tolerance) exit
    if (sign(1.0,MyFunc(a)) .eq. sign(1.0,MyFunc(m))) then
      a = m
    else
      b = m
    endif
  enddo

  print*, "root is: ", m
  print*, "f(m) is:", MyFunc(m)


end program bisection

function MyFunc(x) result(y)
  ! Remove space in memory
  implicit none
  ! Initialize variables for the function
  real :: x, y

  ! Create our function
  y = sin(x) + 1.5 - (0.15 * x)

end function

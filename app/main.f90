program main
  use CaesarShift
  implicit none

  character(:), allocatable :: testStr
  testStr = "Aol ylcvsbapvu dpss uva il alslcpzlk"

  call solve(testStr, 26)
  
end program main

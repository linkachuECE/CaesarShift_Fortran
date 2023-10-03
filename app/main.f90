program main
  use CaesarShift
  implicit none

  character(:), allocatable :: testStr
  character(:), allocatable :: testOutput
  integer :: iShiftAmount = 6

  testStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  allocate(character(len(testStr)) :: testOutput)

  call encrypt(testStr, iShiftAmount, testOutput)
  print*, testOutput

  testStr = testOutput
  call decrypt(testStr, iShiftAmount, testOutput)
  print*, testOutput
  
end program main

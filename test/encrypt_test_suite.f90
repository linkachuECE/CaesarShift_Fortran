module encrypt_test_suite
    use CaesarShift
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none   
    
    private
    public :: collect_encrypt_suite

    contains

    subroutine collect_encrypt_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
      
        testsuite = [ &
          new_unittest("Empty string", test_emptyString), &
          new_unittest("String of the same characters", test_sameCharacters), &
          new_unittest("Uppercase string", test_uppercaseAlpha), &
          new_unittest("Number string", test_lowercaseAlpha), &
          new_unittest("Punctuation string", test_punctuation), &
          new_unittest("Random sentence 1", test_rand1), &
          new_unittest("Random sentence 2", test_rand2), &
          new_unittest("Random sentence 3", test_rand3) &
        ]
      
    end subroutine collect_encrypt_suite
      

    subroutine test_emptyString(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = ""
      testExpected = ""

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_emptyString


    subroutine test_sameCharacters(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = "aaa"
      testExpected = "bbb"

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_sameCharacters
        

    subroutine test_uppercaseAlpha(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      testExpected = "BCDEFGHIJKLMNOPQRSTUVWXYZA"

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return
    end subroutine test_uppercaseAlpha


    subroutine test_lowercaseAlpha(error)
      type(error_type), allocatable, intent(out) :: error

      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = "abcdefghijklmnopqrstuvwxyz"
      testExpected = "bcdefghijklmnopqrstuvwxyza"

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_lowercaseAlpha

    subroutine test_punctuation(error)
            
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = "!@#$%^&*()/."
      testExpected = "!@#$%^&*()/."

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_punctuation


    subroutine test_rand1(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 7

      testStr = "Real eyes realize real lies"
      testExpected = "Ylhs lflz ylhspgl ylhs splz"

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_rand1
        

    subroutine test_rand2(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 19

      testStr = "Supercalifragilisticexpialidocious"
      testExpected = "Lnixkvtebyktzbeblmbvxqibtebwhvbhnl"

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return
    
    end subroutine test_rand2


    subroutine test_rand3(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 24

      testStr = "Everybody wants to rule the world"
      testExpected = "Ctcpwzmbw uylrq rm psjc rfc umpjb"

      allocate(character(len(testStr)) :: testOutput)

      call encrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return
    
    end subroutine test_rand3

end module
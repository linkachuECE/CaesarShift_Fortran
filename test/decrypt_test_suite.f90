module decrypt_test_suite
    use CaesarShift
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none   
    
    private
    public :: collect_decrypt_suite

    contains

    subroutine collect_decrypt_suite(testsuite)
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
      
    end subroutine collect_decrypt_suite
      

    subroutine test_emptyString(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = ""
      testExpected = ""

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
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
      testExpected = "zzz"

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_sameCharacters
        

    subroutine test_uppercaseAlpha(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = "BCDEFGHIJKLMNOPQRSTUVWXYZA"
      testExpected = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return
    end subroutine test_uppercaseAlpha


    subroutine test_lowercaseAlpha(error)
      type(error_type), allocatable, intent(out) :: error

      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 1

      testStr = "bcdefghijklmnopqrstuvwxyza"
      testExpected = "abcdefghijklmnopqrstuvwxyz"

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
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

      call decrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_punctuation


    subroutine test_rand1(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 7

      testStr = "Ylhs lflz ylhspgl ylhs splz"
      testExpected = "Real eyes realize real lies"

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return

    end subroutine test_rand1
        

    subroutine test_rand2(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 19

      testStr = "Lnixkvtebyktzbeblmbvxqibtebwhvbhnl"
      testExpected = "Supercalifragilisticexpialidocious"

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return
    
    end subroutine test_rand2


    subroutine test_rand3(error)
      type(error_type), allocatable, intent(out) :: error
      
      character(:), allocatable :: testStr
      character(:), allocatable :: testOutput
      character(:), allocatable :: testExpected
      
      integer :: iShiftAmount = 24

      testStr = "Ctcpwzmbw uylrq rm psjc rfc umpjb"
      testExpected = "Everybody wants to rule the world"

      allocate(character(len(testStr)) :: testOutput)

      call decrypt(testStr, iShiftAmount, testOutput)
      
      call check(error, testOutput, testExpected)

      if(allocated(error)) return
    
    end subroutine test_rand3

end module
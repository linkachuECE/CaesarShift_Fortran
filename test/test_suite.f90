module test_suite
    use CaesarShift
    use testdrive, only : new_unittest, unittest_type, error_type, check
    implicit none   
    
    private
    public :: test_encrypt, test_decrypt, test_solve, collect_suite

    contains

    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
      
        testsuite = [ &
          new_unittest("Encrypt", test_encrypt), &
          new_unittest("Decrypt", test_decrypt), &
          new_unittest("Solve", test_solve) &
        ]
      
    end subroutine collect_suite
      

    subroutine test_encrypt(error)
      type(error_type), allocatable, intent(out) :: error

        ! Lowercase string
        
            
        ! Uppercase string
    
        ! Empty string

        ! Punctuation string

        ! Number string

    end subroutine test_encrypt


    subroutine test_decrypt(error)
      type(error_type), allocatable, intent(out) :: error

    end subroutine test_decrypt
        

    subroutine test_solve(error)
      type(error_type), allocatable, intent(out) :: error
    
    end subroutine test_solve


end module
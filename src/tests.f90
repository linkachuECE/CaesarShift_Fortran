module Tests
    use CaesarShift
    implicit none
    
    private
    public :: test_encrypt, test_decrypt, test_show

    contains

    logical function test_encrypt()

        test_encrypt = .true.

    end function test_encrypt


    logical function test_decrypt()

    test_decrypt = .true.

    end function test_decrypt
        

    logical function test_show()
    
    test_show = .true.
    
    end function test_show


end module
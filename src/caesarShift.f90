module CaesarShift
    implicit none
    private
    public :: encrypt, decrypt, solve

    contains

    character function nextChar(cCurrChar, iShiftAmount)
        character(1), intent(in) :: cCurrChar
        integer, intent(in) :: iShiftAmount
        integer :: iNextASCII

        iNextASCII = iachar(cCurrChar) + iShiftAmount

        if(isLower(cCurrChar) .and. .not. isLower(char(iNextASCII))) then
            if(iNextASCII < iachar('a')) then
                iNextASCII = iChar('z') - (iachar('a') - iNextASCII - 1)
            else
                iNextASCII = iachar('a') + (iNextASCII - 1 - iachar('z'))
            end if
        else if(isUpper(cCurrChar) .and. .not. isUpper(char(iNextASCII))) then
            if(iNextASCII < iachar('A')) then
                iNextASCII = iChar('Z') - (iachar('A') - iNextASCII - 1)
            else
                iNextASCII = iachar('A') + (iNextASCII - 1 - iachar('Z'))
            end if
        end if

        nextChar = char(iNextASCII)

    end function

    logical function isLower(cChar)
    character(1), intent(in) :: cChar

        isLower = (iachar(cChar) >= iachar('a') .and. iachar(cChar) <= iachar('z'))

    end function

    logical function isUpper(cChar)
    character(1), intent(in) :: cChar

        isUpper = (iachar(cChar) >= iachar('A') .and. iachar(cChar) <= iachar('Z'))

    end function

    logical function isAlphabetic(cChar)
    character(1), intent(in) :: cChar

        isAlphabetic = (isLower(cChar) .or. isUpper(cChar))

    end function

    subroutine shift(strInput, iShiftAmount, strOutput)
        character(len=*), intent(in) :: strInput
        character(len=*), intent(out) :: strOutput
        integer, intent(in) :: iShiftAmount
        integer :: i, iCurrASCII
        character(1) :: cCurrChar, cNewChar
        
        ! print *, "Shifting: '", strInput, "' by ", iShiftAmount
        ! print *, len(strInput)


        do i = 1, len(strInput)
            cCurrChar = strInput(i:i)
            iCurrASCII = iachar(cCurrChar)

            if (isAlphabetic(cCurrChar)) then
                cNewChar = nextChar(cCurrChar, iShiftAmount)
            else 
                cNewChar = cCurrChar
            end if

            strOutput(i:i) = cNewChar
        end do

        ! print *, "Shifted to: '", strOutput, "'"

    end subroutine shift


    subroutine encrypt(strInput, iShiftAmount, strOutput)
        character(*), intent(in) :: strInput
        character(*), intent(out) :: strOutput
        integer, intent(in) :: iShiftAmount
        
        call shift(strInput, iShiftAmount, strOutput)
    end subroutine encrypt


    subroutine decrypt(strInput, iShiftAmount, strOutput)
        character(*), intent(in) :: strInput
        character(*), intent(out) :: strOutput
        integer, intent(in) :: iShiftAmount
        
        call shift(strInput, -iShiftAmount, strOutput)
    end subroutine decrypt

    subroutine solve(strInput, iMaxShiftValue)
        character(*), intent(in) :: strInput
        integer, intent(in) :: iMaxShiftValue

        integer :: iShiftValue

        character(:), allocatable :: strOutput
        allocate(character(len(strInput)) :: strOutput)

        do iShiftValue = 1, iMaxShiftValue
      
            call decrypt(strInput, iShiftValue, strOutput)

            write(*,fmt="(A,i2,A,A)") "Caesar ", iShiftValue, ": ", strOutput
        end do

    end subroutine

end module
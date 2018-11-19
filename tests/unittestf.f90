module unittestf

    private
    public :: assert_char

    contains

function assert_char(c, expected)
    implicit none

    character, intent(in) :: c        ! Value returned by getopt
    character, intent(in) :: expected ! The value we expect
    logical :: assert_char

    if ( c == expected) then
        assert_char = .TRUE.
    else
        assert_char = .FALSE.
    end if
    
    return
end function


end module unittestf

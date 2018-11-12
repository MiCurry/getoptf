module unittestf

    private
    public :: assert_opt

    contains



function assert_char(c, expected):
    implicit none

    character, intent(in) :: c        ! Value returned by getopt
    character, intent(in) :: expected ! The value we expect
    logical :: assert_opt

    if ( c == expected) then
        assert_opt = .TRUE.
    else
        assert_opt = .FALSE.
    end if
    
    return
end function


end module unittestf

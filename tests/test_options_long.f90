program test_options_simple

! Test the basic functionality of getoptf. Only test a small number of options
!
! The test input for this program will be:
!
! ./test_options_simple -a -b -c -12 -3
!

    use getoptf, only : getopt
    use unittestf, only : assert_char

    implicit none

    integer, parameter :: verbose = 1

    ! getoptf variables
    character (len=255) :: argv
    integer :: argc
    character :: c
    !character (len=255) :: optString = "abc123"


    ! debug variables 
    integer :: optcnt ! Will count our arguments
    integer :: numTests, numErrs, numSuc
    ! "Option: 1 was character: a expedted: a - Test - PASSED"
    character (len=300), parameter :: OPT_TST_FMT = "(A, I2, A, A, A, A, I2, A)"



    if (verbose > 0) then
        write(0, *) "FORTRAN DEBUGING STATEMENTS ON!"
    end if

    argc = command_argument_count()

    if( argc < 1 ) then
        write(0,*) "Please launch this program from the command line as:"
        write(0,*) "./test_options -a -b -c -12 -3"
        stop
    endif

    call get_command(argv)

    if (verbose > 0) then
        write(0, *) "FDEBUB test_options: argc: ", argc
        write(0, *) "FDEBUG test_options: argv: ", argv
    end if
    
    optcnt = 0 
    numErrs = 0
    numSuc = 0
do while(getopt(argc, argv, c, "abcdefghijklmnopqrstuvwxyz&
								&ABCDEFGHIJKLMNOPQRSTUVWXYZ&
								&1234567890&
								&!@#$%^*()_=+{[}]\|;'<>,./~`")) 
end do

write(0, *) "Test finish"
write(0, *) "Total tests: ", numSuc + numErrs
write(0, *) "Number of Success: ", numSuc
write(0, *) "Number of Failures: ", numErrs

end program test_options_simple

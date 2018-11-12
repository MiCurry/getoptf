program test_options_simple

! Test the basic functionality of getoptf. Only test a small number of options
!
! The test input for this program will be:
!
! ./test_options_simple -a -b -c -12 -3
!

    use getoptf_m
    use unittestf, only assert_char

    implicit none

    integer, parameter :: verbose = 1

    ! getoptf variables
    character (len=255) :: argv
    integer :: argc
    character :: c 

    ! debug variables 
    integer :: optcnt ! Will count our arguments
    integer :: numTests, numErrs, numSuc
    ! "Option: 1 was character: a expedted: a - Test - PASSED"
    character (len=300), parameter :: OPT_TST_FMT = "A, I2, A, A, A, A, I, A"


    if (verbose > 0) then
        write(0, *) "FORTRAN DEBUGING STATEMENTS ON!"
    end if

    argc = command_argument_count()
    call get_command(argv)

    if (verbose > 0) then
        write(0, *) "FDEBUB test_options: argc: ", argc
        write(0, *) "FDEBUG test_options: argv: ", argv
    end if
    
    optcnt = 0 
    numErrs = 0
    numSuc = 0
do while(getopt(argc, argv, c "abc123") /= -1) 
        select case (c)
            case ('a'):
        if(assert_opt(c, 'a')) then
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- PASSED"
            numSuc = numSuc + 1
        else
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- FAILED"
            numErrs = numErrs + 1
        end if
    case ('b'):
        if(assert_opt(c, 'b')) then
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- PASSED"
            numSuc = numSuc + 1
        else
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- FAILED"
            numErrs = numErrs + 1
        end if
    case ('c'):
        if(assert_opt(c, 'c')) then
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- PASSED"
            numSuc = numSuc + 1
        else
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- FAILED"
            numErrs = numErrs + 1
        end if
    case ('1'):
        if(assert_opt(c, '1')) then
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- PASSED"
            numSuc = numSuc + 1
        else
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- FAILED"
            numErrs = numErrs + 1
        end if
    case ('2'):
        if(assert_opt(c, '2')) then
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- PASSED"
        else
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- FAILED"
            numErrs = numErrs + 1
        end if
    case ('3'):
        if(assert_opt(c, '3')) then
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- PASSED"
        else
            write(0, OPT_TST_FMT) "Option: ", optcnt, "was char: ", c, "expected: ", 'a', &
                                  &"- Test", optcnt, "- FAILED"
            numErrs = numErrs + 1
        end if
end do

write(0, *) "Test finish"
write(0, *) "Total tests: ", numSuc + numErr
write(0, *) "Number of Success: ", numSuc
write(0, *) "Number of Failures: ", numErr

end program test_options_simple

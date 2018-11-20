program getoptTest

    use getoptf_m, only : getoptf

    implicit none

    character(len=255) :: argv  ! This holds all of our arguments
    integer :: argc             ! The number of command line arguments
    integer :: c 

    argc = command_argument_count()
    call get_command(argv)

    c = getoptf(argc, argv, c, " ")

    !do while ( getopt(argc, argv, c, " ") /= -1 )
        ! Switch statement here

    !end do
    


end program getoptTest

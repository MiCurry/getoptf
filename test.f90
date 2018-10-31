program getoptTest

    !use getoptf_m only, getopt

    implicit none

    character(len=255) :: argv  ! This holds all of our arguments
    integer :: argc             ! The number of command line arguments
    integer :: c 


    argc = command_argument_count()
    argv = get_command()

    do while ( getopt(argc, argv, c, " ") /= -1 )
        select case(c)
            case (v)
            
            case (h)

            case (v)

        end select

    end do
    


end program getoptTest

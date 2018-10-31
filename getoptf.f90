module getoptf_m

    private 
    public :: getopt                
    public :: opterr, optopt, optind
    public :: optarg

    integer :: opterr, optopt, optind
    character(len=256) :: optarg

    integer :: ARG_COUNT                ! Big argc
    character(len=256) :: ARG_ARRAY     ! Big argv
    character(len=256) :: ARG_FORMAT    ! Big format specificatoin

    logical :: first_call_flag = .TRUE. ! Set to false after first call

    type option 
        logical :: argumet = .FALSE.     ! If False == no options if True == options
        logical :: arg_required          ! if False == option not required if True == option is required
        character :: short_opt  ! The speifiers for the option
        character(len=256) :: long_opt   ! The long specifier for the optoin
        character(len=256) :: options
        type (option), pointer :: next => null()
    end type option 

    contains

    function parse_format(format)
        implicit none

        character(len=256), intent(in) :: format
        integer :: parse_format
    end function parse_format


    function getoptf(argc, argv, c, format)

    !do while ( getopt(argc, argv, c, " ") /= -1 )
        implicit none
    
        ! Input arguments
        integer, intent(in) :: argc                 ! Argument count
        character(len=256), intent(in) :: argv      ! Arguments
        character :: c                              ! Current option (return)
        character(len=256), intent(in) :: format    ! Format string for the arguments

        ! Return value
        integer :: getoptf 


        if(first_call_flag) then
            ! Parse though the format list and allocate an argument type
            ! Check to be sure that they're correctly specified
        end if


    end function getoptf

end module getoptf_m

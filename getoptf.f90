module getoptf

    private 
    public :: getopt

    ! External Variables
    public :: opterr
    public :: optopt
    public :: optind
    public :: optarg

    integer :: opterr, optopt, optind
    character(len=256) :: optarg

    integer :: ARG_COUNT                ! Global argc
    character(len=256) :: ARG_ARRAY     ! Global argv
    character(len=256) :: ARG_FORMAT    ! Global format specificatoin

    ! Local List of argument string, arg count  that we'll use to keep track
    ! of what options we have to proccess
    integer :: argc_l
    character(len=256) :: argv_l

    logical :: first_call_flag = .TRUE. ! Set to false after first call

    integer, parameter :: FORMAT_PARSE_ERROR = -1
    integer, parameter :: OPTION_PARSE_ERROR = -1
    integer, parameter :: ERROR = -1

    INTEGER, PARAMETER :: DEBUG = 1

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! type option - The option struct
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type option 
        logical :: argument = .FALSE.     ! If False == no options if True == options
        character :: short_opt  ! The speifiers for the option
        character(len=256) :: long_opt   ! The long specifier for the optoin
        type (option), pointer :: next => null()
    end type option 
    integer :: optcnt = 0

    type (option), target :: optlist

    contains

    ! 100% a debugger function
    subroutine print_valid_options()
        implicit none
        
        type(option), pointer :: cur
        integer :: i

        if (optcnt == 0) then
            write(0,*) "There are no options to print"
            return 
        endif

        write(0,*) ""
        write(0,*) "DEBUG: printing valid options - (found in the optstring)"
        write(0,*) "There are ", optcnt, " options"
        cur => optlist%next
        do i = 1, optcnt, 1
            write(0,*) "Option Num: ", i, " of: ", optcnt
            write(0,*) "Option name: ", cur%short_opt
            write(0,*) "Arg Required: ", cur%argument 
            cur => cur%next
        enddo

        write(0,*) "DEBUG: End list of valid options"
        write(0,*) ""

    end subroutine


    ! Allocate and add the option to the list
    subroutine add_option(opt)
        implicit none

        type(option), pointer :: opt, cur
        integer :: i

        if (DEBUG > 0) then
            write(0,*) "DEBUG: add_option - About to add an option.."
            write(0,*) "optcnt = ", optcnt
            write(0,*) "option%short_opt = ", opt%short_opt
            write(0,*) "option%argument = ", opt%argument
        endif

        if (optcnt == 0) then
            optlist%next=>opt
            optcnt = optcnt + 1
        else
            opt%next=>optlist%next
            optlist%next=>opt
            optcnt = optcnt + 1
        endif

        allocate(opt)

    end subroutine add_option

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: parse_format 
    !
    ! Description: Parse through the programmer specified format string and create an 
    ! 'option' type above, allocate it and save it to the list. Continue 
    ! for each specifier.
    !
    ! Input: optString -- The specified format of the string by the programmer
    !
    ! Return value: 0 on success -1 on error
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Development Notes:
    !
    ! 1. Print out encourted errors if opterr is set
    !   - Also determine what errors can be caused when parsing the optstring
    !
    ! It appears that no errors are genereated if the format string contains
    ! an error or an invalid character! An error is only produced when
    ! the user supplies an invalid option.
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function parse_format(optString)
        implicit none

        character(len=*), intent(in) :: optString
        integer :: parse_format ! Return Value

        integer :: i, j, k

        type(option), pointer :: opt

        if(DEBUG > 0) write(0,*) "getopt: ", optString


        ! For each option we encounter
        !  check to see if its valid
        !  - If its valid then allocated it to our ll of valid options
        !  - If its not valid then stop and report an error
        do i = 1, len(optString), 1

            if(DEBUG > 1) then
                call print_valid_options() 
            endif

            if( optString(i:i) == '?' ) then      ! Error - Illegal Option
                write(0,*) "getoptf: Illegal option in optString: ", optString(i:i)
                stop
            elseif( optString(i:i) =='-' ) then   ! Error - Illegal Option
                write(0,*) "getoptf: Illegal option in optString: ", optString(i:i)
                stop
            elseif( optString(:1) == ':' .AND. optString(2:2) == ':' ) then ! Error
                ! Error - 2 ':' next to each other
                write(0,*) "getoptf: Illegal option in optString: ", optString(i:i)
                write(0,*) "getoptf: Cannot have ':' followed by a ':'"
                write(0,*)
                stop
            elseif( optString(i:i) == ':' .AND. i == 1 ) then
                ! Surpress Error Messages
            else
                ! Then we have a valid option
                opt%short_opt = optString(i:i)
                opt%argument = .FALSE.

                if( i == len(optString)) then 
                    ! Ensure we are not tryhing to access area out-of-bounds
                    if(DEBUG>0) write(0,*) " No argument required for this option"
                else
                    if (optString(i+1:i+1) == ':') then
                        opt%argument = .TRUE.
                    endif
                endif
                call add_option(opt)                 ! Add the option to the optList
            endif
        enddo

        parse_format = 0
    end function parse_format

    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: parse_options
    !
    ! Description: Parse through the commands and return each option (with its arguments
    ! and stuff. Then remove that argument from the command so we can parse
    ! through options that we haven't parsed through yet.
    !
    ! Input: commands - The list of options specfieid by the user/call
    !
    ! Return:
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Development Notes:
    !
    ! 1. Determine errors that can be caused when parsing options
    ! 2. Invalid Characters
    !   * `-:`  -- './a.out invalid option -- '-'
    !   * `-?`  -- './a.out No Match.'
    !   * `---` -- Prints out the following twice: './a.out invalid option -- '-''
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function parse_options(options)
        implicit none

        character(len=*) :: options
        integer :: parse_options

        parse_options = 0
    end function parse_options


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Name: getoptf 
    !
    ! Input: argc - intent(in)  -- The argument count - Integer
    !        argv - intent(in)  -- The string of options and their arguments (if any) - character string
    !           c - intent(out) -- A character to hold the currently proccessed valid option
    !      format - intent(in)  -- The optString of valid options
    !
    ! Return: -1 -- When all options have been proccessed
    !          1 -- Otherwise
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function getopt(argc, argv, c, format)
        !Example call to get opt: do while ( getopt(argc, argv, c, " ") /= -1 )

        implicit none
    
        ! Input arguments
        integer, intent(in)             :: argc      ! The argument count
        character(len=*), intent(in)    :: argv      ! The list of Arguments
        character (len=1), intent(out)  :: c         ! The current option
        character(len=*), intent(in)    :: format    ! Format string for the valid options

        ! Return value - getoptf -1 when all options are proccessed otherwise - 1
        logical :: getopt
        
        ! Local variables
        integer :: ierr

        argc_l = argc
        argv_l = argv

        c = '?'

        if(first_call_flag) then
             ierr = parse_format(format)
             if (ierr == FORMAT_PARSE_ERROR) then 
                 !report error
             end if
             if(DEBUG>0) then
                 call print_valid_options()
             endif
        end if

        getopt = .FALSE.
    end function getopt
end module getoptf

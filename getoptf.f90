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
        type (option), pointer :: prev => null()
    end type option 

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: print_list()
    !
    ! Description: Prints a option type linked list
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine print_list(list)
        implicit none
        
        ! Input variables
        type(option), pointer, intent(in) :: list

        ! Local variables
        type(option), pointer :: cur
        integer :: i = 1

        cur => list
        if (associated(cur%next) == .FALSE. ) then
            write(0,*) "There are no options to print"
        else
            cur => list%next
            do while(associated(cur%next))
                write(0,*) "Option Num: ", i
                write(0,*) "Option name: ", cur%short_opt
                write(0,*) "Arg Req: ", cur%argument 
                write(0,*) ""
                cur => cur%next
                i = i + 1

                if (associated(cur, list)) then
                    write(0,*) "We are at the list head"
                    exit
                endif
            enddo
        endif

    end subroutine print_list


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: add_option
    !
    ! Description: 
    !
    ! Input: opt - an allocated and initialized option type
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine add_option(opt, list)
        implicit none

        ! Input variables
        type(option), pointer :: opt
        type(option), pointer :: list

        ! Local variables
        integer :: i

        if (associated(list%next)) then
            list%next%prev => opt
            opt%next => list%next
            list%next => opt
            opt%prev => list
        else ! First item in the list
            list%next => opt 
            opt%next => list
        endif

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
    !                     Example: "ab1:v:f1:f2"
    !
    ! Return value: 
    !
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function parse_format(optString, list)
        implicit none

        ! Input variables
        character(len=*), intent(in) :: optString
        type(option), pointer :: list

        ! Return value
        integer :: parse_format 

        ! Local variables
        type(option), pointer :: opt
        integer :: i, j, k

        if(DEBUG > 0) write(0,*) "getopt: optstring is:", optString

        do i = 1, len(optString), 1

            ! Error
            if( optString(i:i) == '?' ) then
                write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                stop
            
            ! Error
            elseif( optString(i:i) =='-' ) then
                write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                stop

            ! Error - If we encouter
            elseif( optString(:1) == ':' .AND. optString(2:2) == ':' ) then
                write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                ! Error "If the 1st and the 2nd chars are both ':' throw an error
                write(0, *)
                stop

            ! Surpress Error Messages
            elseif( optString(1:1) == ':') then
                ! pass

            ! Valid option
            else 
                if(DEBUG > 1) then
                    write(0,*) "DEBUG: We have a new option: ", optString(i:i)
                endif

                allocate(opt)

                opt%short_opt = optString(i:i)

                if( i == len(optString)) then 
                    opt%argument = .FALSE.;
                else
                    if (optString(i+1:i+1) == ':') then 
                        opt%argument = .TRUE.
                    endif
                endif
                call add_option(opt, list)
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
    ! Example call to get opt: do while ( getopt(argc, argv, c, " ") /= -1 )
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

        implicit none
    
        ! Input arguments
        integer, intent(in)             :: argc      ! The argument count
        character(len=*), intent(in)    :: argv      ! The list of Arguments
        character (len=1), intent(out)  :: c         ! The current option
        character(len=*), intent(in)    :: format    ! Format string for the valid options

        ! Return value - getoptf -1 when all options are proccessed otherwise - 1
        logical :: getopt
        
        ! Local variables
        type(option), pointer :: optlist
        type(option), pointer :: arglist
        integer :: ierr

        allocate(optlist)
        allocate(arglist)

        argc_l = argc
        argv_l = argv

        c = '?'

        if(first_call_flag) then
             ierr = parse_format(format, optlist)
             if (ierr == FORMAT_PARSE_ERROR) then 
                 !report error
             end if
             if(DEBUG>0) then
                 call print_list(optlist)
             endif
        end if

        getopt = .FALSE.
    end function getopt
end module getoptf

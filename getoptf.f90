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

    logical :: FIRST_CALL_FLAG = .TRUE. ! Set to false after first call
    logical :: OUTPUT
    

    integer, parameter :: FORMAT_PARSE_ERROR = -1
    integer, parameter :: OPTION_PARSE_ERROR = -1
    integer, parameter :: ERROR = -1

    INTEGER, PARAMETER :: DEBUG = 1

    character, parameter :: SPACE = ' '
    character, parameter :: DASH = '-'
    character, parameter :: COLON = ':'
    character, parameter :: QUESTION_MARK = '?'
    character(len=*), parameter :: MAYBE = "MAYBE"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! type option - The option struct
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type option 
        logical :: argument     ! If False == no options if True == options
        character :: short_opt  ! The speifiers for the option
        character(len=256) :: long_opt   ! The long specifier for the optoin
        character(len=256) :: arg   ! The argument associated with this option
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
        if (associated(cur%next) .EQV. .FALSE. ) then
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
                    exit ! We have returned to the list head - Exit
                endif
            enddo
        endif

    end subroutine print_list

    ! Get the first option that was added
    function get_first(list, opt)
        implicit none
        ! Input variables
        type(option), pointer, intent(in) :: list
        type(option), intent(out) :: opt
        ! Return value
        logical :: get_first
        ! Local variables

        if(associated(list%prev)) then
            get_first = .TRUE.
            opt = list%prev
        else
            get_first = .FALSE.
        endif
    end function get_first

    ! Get the last option that was allocated
    function get_last(list, opt)
        implicit none
        ! Input variables
        type(option), pointer, intent(in) :: list
        type(option), intent(out) :: opt
        ! Return variable
        logical :: get_last
        
        if(associated(list%next)) then
            get_last = .TRUE.
            opt = list%next
        else
            get_last = .FALSE.
        endif
    end function get_last


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
        integer :: i

        if(DEBUG > 0) write(0,*) "getopt: optstring is:", optString

        write(0,*) "Optstring length=", len(optString)

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
            elseif ( len(optString) > 1) then
                if ( optString(1:1) == ':' .AND. optString(2:2) == ':' ) then
                    write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                    ! Error "If the 1st and the 2nd chars are both ':' throw an error
                    write(0, *)
                    stop
                endif
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
                else if (optString(i+1:i+1) == ':') then 
                        opt%argument = .TRUE.
                endif
                call add_option(opt, list)
            endif
        enddo

        parse_format = 0
    end function parse_format

    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: parse_argv
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
    function parse_argv(argv, list)
        implicit none

        ! Input variables 
        character(len=*), intent(in) :: argv
        type(option), pointer :: list

        ! Return variable
        logical :: parse_argv

        ! Local variables
        type(option), pointer :: arg
        integer :: i, j
        integer :: CMD_LENGTH

        ! Zip through argv, and gather all the arguments
        
        ! Deterime the program name - so we can save it and skip over it
        do i = 1, len(argv)
            if(argv(i:i) == SPACE) then
                CMD_LENGTH = i - 1
                exit
            endif
        enddo
        i = i - 1

        if(DEBUG>0) then
            write(0,*) "The length of the command was: ", i
            write(0,*) "And the command was: ", argv(1:i)
            write(0,*) ""
        endif

         
        do while( i < len(argv)) ! For the whole length of argv
            if( argv(i:i) == DASH ) then
                j = i
                do while ( argv(j:j) /= SPACE .OR. j == len(argv) )
                    j = j + 1
                enddo
                write(0,*) "Argument/Option: ", argv(i:j)
            endif
            i = i + 1
        enddo


        parse_argv = .FALSE.
    end function parse_argv


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

        if(argc /= 0) then
            write(0,*) "Now going to parse options"
            getopt = parse_argv(argv, arglist)
        else
            getopt = .FALSE.
            if(DEBUG>0) then
                write(0,*) "No commandline options or arguments were passed in - so we are returning"
            endif
            return
         endif
        
        if(DEBUG>0) then
            call print_list(arglist)
        endif

        write(0,*) ""

    end function getopt
end module getoptf

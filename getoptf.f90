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

    logical :: FIRST_CALL_FLAG = .TRUE. ! Set to false after first call

    character, parameter :: SPACE = ' '
    character, parameter :: DASH = '-'
    character, parameter :: COLON = ':'
    character, parameter :: QUESTION_MARK = '?'
    character(len=*), parameter :: MAYBE = "MAYBE"

    INTEGER, PARAMETER :: DEBUG = 1

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! type option - The option struct
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    type option
        character(len=:), allocatable :: cmd   ! This is the option
        character(len=:), allocatable :: arg   ! The argument associated with this option

        logical :: argument                    ! If False == no options if True == options
        logical :: list = .FALSE.              ! Marks if this is a list or not

        type (option), pointer :: next => null()
        type (option), pointer :: prev => null()
    end type option

    contains

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Name: print_list()
    ! Description: Prints a type(option) linked list
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine print_list(list)
        implicit none

        ! Input variables
        type(option), pointer, intent(in) :: list

        ! Local variables
        type(option), pointer :: cur
        integer :: i = 1

        i = 1

        cur => list
        if ( .NOT. associated(cur%next)) then
            write(0,*) "There are no options to print"
        else
            cur => list%next
            do while(associated(cur%next))
                if (associated(cur, list)) then
                    write(0,*) "End of the list"
                    write(0,*) ""
                    exit ! We have returned to the list head - Exit
                endif

                write(0,*) "Option Num: ", i
                write(0,*) "Option list: ", cur%list
                write(0,*) "Name : ", cur%cmd
                write(0,*) "Argument: ", cur%argument
                write(0,*) ""
                cur => cur%next
                i = i + 1

                !if (associated(cur, list)) then
            enddo
        endif
    end subroutine print_list

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: get_first
    !
    ! Description: Get the first type(option) (i.e. option, non-option,
    !              argument) in a list.
    !
    ! Input: list - An allocate list of type(option) that contains, options,
    !               commands or arguments
    !         opt - A pointer to a type(option) that will hold the first
    !               type(option) found.
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Get the first command that was added
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
        endif !TODO: Make sure this is implented
    end function get_first

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: get_first_option
    !
    ! Description: Get the first option in a list.
    !
    ! Input: list - An allocate list of type(option) that contains, options,
    !               commands or arguments
    !         opt - A pointer to a type(option) that will hold the first option
    !               found
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function get_first_option(list, opt)
        implicit none
        ! Input variables
        type(option), pointer, intent(in) :: list
        type(option), pointer, intent(out) :: opt
        ! Return value
        logical :: get_first_option
        type(option), pointer :: cur

        get_first_option = .FALSE.
        cur => list%prev
        do while(associated(cur%prev))
            if (allocated(cur%cmd)) then
                if ( cur%cmd(1:1) == DASH ) then
                    get_first_option = .TRUE.
                    opt => cur
                    return
                else
                    cur => cur%prev
                endif
           else
                get_first_option = .FALSE. ! No more options in the list
                cur => null()
                return ! We have returned to the list head - Exit
           endif
        enddo
    end function get_first_option

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: get_last
    !
    ! Description: Get the last option that was allocated to a list (ie the
    !              end of argv)
    !
    ! Input: list - An allocate list of type(option) that contains, options,
    !               commands or arguments
    !         opt - A pointer to a type(option) that will hold the first option
    !               found
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function get_last(list, opt) ! TODO: Make sure this is implemented
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
    ! Name: get_next
    !
    ! Description: Get the next type(option) of a list
    !
    ! Input: list - An allocate list of type(option) that contains, options,
    !               commands or arguments
    !         opt - The current type(option) within the list
    !         next - A pointer to a type(option) that will hold the next option
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function get_next(list, opt, next)
        implicit none
        !Input Variables
        type(option), pointer, intent(in) :: list
        type(option), intent(in) :: opt
        type(option), intent(out) :: next
        ! Return variables
        logical :: get_next

        get_next = .FALSE.

        if (associated(opt%prev, list)) then
            return ! We have returned to the list head - Exit
        endif

        if(associated(opt%next) .AND. .NOT. opt%next%list) then
            get_next = .TRUE.
            next = opt%next
        endif ! TODO: Make sure this is implemented
    end function get_next

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: get_prev
    !
    ! Description: Get the previous type(option) of a list
    !
    ! Input: list - An allocate list of type(option) that contains, options,
    !               commands or arguments
    !         opt - The current type(option) within the list
    !         next - A pointer to a type(option) that will hold the prev option
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function get_prev(opt) ! TODO: Make sure this is implemented
        implicit none
        !Input Variables
        type(option), intent(inout) :: opt
        ! Return variables
        logical :: get_prev

        write(0,*) "Opt prev", opt%prev%list

        get_prev = .FALSE.
        if(associated(opt%prev) .AND. .NOT. opt%prev%list) then ! opt%long_opt /= list%long_opt) then
            get_prev = .TRUE.
            opt = opt%prev
        endif
    end function get_prev

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: get_prev_option
    !
    ! Description: Get the prev option that was added to a list.
    !
    ! Input: list - An allocate list of type(option) that contains, options,
    !               commands or arguments
    !         opt - A pointer to a type(option) that will hold the first option
    !               found
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    function get_prev_option(opt)
        implicit none
        !Input Variables
        type(option), pointer, intent(inout) :: opt
        type(option), pointer :: cur
        ! Return variables
        logical :: get_prev_option

       get_prev_option = .FALSE.
       cur => opt

       !write(0,*) "This option is: ", opt%cmd
       !write(0,*) "Its prev is: ", opt%prev%cmd

        do while(associated(cur%prev))
            cur => cur%prev
            if (allocated(cur%cmd)) then
                if (cur%cmd(1:1) == DASH) then
                    get_prev_option = .TRUE.
                    opt => cur
                    return
                endif
           else
                get_prev_option = .FALSE. ! No more options in the list
                opt => null()
                return ! We have returned to the list head - Exit
           endif
        enddo
    end function get_prev_option

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !
    ! Name: add_option
    !
    ! Description: Add an element to and option list
    !
    ! Input: list - The list to be added to
    !        opt - an allocated and initialized option type
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine add_option(list, opt)
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
            list%prev => opt
            opt%next => list
            opt%prev => list
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
    !        list - The list hold the parsed optString
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

        do i = 1, len(optString), 1

            ! Error - '?' in format string
            if( optString(i:i) == '?' ) then
                write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                stop
            ! Error - '-' in format string
            elseif( optString(i:i) =='-' ) then
                write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                stop
            ! Error - If we encouter two ':' in a row
            elseif ( len(optString) > 1) then
                if ( optString(1:1) == ':' .AND. optString(2:2) == ':' ) then
                    write(0, *) "getoptf: Illegal option in optString: ", optString(i:i)
                    ! Error "If the 1st and the 2nd chars are both ':' throw an error
                    write(0, *)
                    stop
                endif
            endif

            ! If the start of the string is ':', then we must surpress output
            if ( optString(1:1) == ':') then
                ! pass - TODO: Surpress output
            endif

            ! Anything else is a valid option
            allocate(opt)
            allocate(character(1) :: opt%cmd)
            opt%cmd = optString(i:i)

            ! Check to see if this option has a ':' after it
            if( i == len(optString)) then
                opt%argument = .FALSE.
            else if (optString(i+1:i+1) == ':') then
                    opt%argument = .TRUE.
            endif
            call add_option(list, opt)
        enddo

        parse_format = 0 ! TODO: Check to see if we need this return value
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
    !            list - The list to hold the parsed argv
    !
    ! Return: TODO: Does this need a return?
    !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Invalid Characters
    !   * `-:`  -- './a.out invalid option -- '-'
    !   * `-?`  -- './a.out No Match.'
    !   * `--` -- 'Two dashes in a row'
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
        type(option), pointer :: cmd
        type(option), pointer :: cmdlist
        integer :: i, j
        integer :: CMD_LENGTH


        ! Deterime the program name - so we can save it and skip over it
        do i = 1, len(argv)
            if(argv(i:i) == SPACE) then
                CMD_LENGTH = i - 1
                exit
            endif
        enddo

        if(DEBUG>0) then
            write(0,*) "The length of the command was: ", i
            write(0,*) "And the command was: ", argv(1:i)
            write(0,*) ""
        endif

        allocate(cmdlist)

        !! Zip through argv, and gather all the options, arguments and non-options

        do while( i < len(argv))
            ! If we don't have a space, then we have an option, non-option or
            ! an argument. Determine its type, and add it to the list
            if( argv(i:i) /= SPACE ) then ! We have an argument
                allocate(cmd)

                j = i

                if( argv(j:j) == DASH) then
                    cmd%argument = .FALSE. ! This is not an argument
                else
                    cmd%argument = .TRUE. ! This is an argument
                endif

                ! Loop to the end of this option/argument
                do while ( argv(j:j) /= SPACE .OR. j == len(argv) )
                    j = j + 1
                enddo

                if(DEBUG > 1) then
                    write(0,*) "Option: ", argv(i:j)
                endif

                ! Allocate it and add it to the list of command list
                allocate(character(j) :: cmd%cmd)
                allocate(character(1) :: cmd%arg)

                cmd%cmd = argv(i:j)
                cmd%arg='-'

                call add_option(cmdlist, cmd)
                i = j ! Skip over this opt/arg in argv
            endif

            i = i + 1
        enddo

        if (DEBUG > 2) then
            call print_list(cmdlist)
        endif

        list => cmdList
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
        type(option), pointer :: cur_opt => null()
        ! type(option), pointer :: next_opt => null()
        ! type(option), pointer :: prev_opt => null()

        type(option), pointer :: optlist
        type(option), pointer :: arglist
        integer :: ierr

        c = '?'

        getopt = .FALSE.

        if(first_call_flag) then
            allocate(optlist)
            allocate(arglist)

            allocate(cur_opt)

            optlist%list = .TRUE.
            arglist%list = .TRUE.

             if(DEBUG>0) then
                write(0,*) "We ran the first call flag!"
             endif

             ierr = parse_format(format, optlist)
             if (ierr == -1) then
                 !report error
             end if

             if(DEBUG>0) then
                 write(0,*) " Going to print the optlist "
                 call print_list(optlist)
             endif

            if(argc /= 0) then
                write(0,*) "Now going to parse options"
                getopt = parse_argv(argv, arglist)

                if(DEBUG>0) then
                    write(0,*) " Going to print the arglist"
                    call print_list(arglist)
                endif
            else
                if(DEBUG>0) then
                    write(0,*) "No commandline options or arguments were passed in - so we are returning"
                endif

                getopt = .FALSE.
                return
             endif

             ! After the first time through, we no longer need to process the
             ! argument lists, so set this flag to .FALSE.
             first_call_flag = .FALSE.

             ! Get the first option of the list
             if(get_first_option(arglist, cur_opt)) then
                 c = cur_opt%cmd(2:2)
                 getopt = .TRUE.
             endif

             return ! The first character or nothing
        endif

        ! Now loop backwards through the the command (backwards, because the list
        ! in the order the options is in reverse order)
        if(get_prev_option(cur_opt)) then
            c = cur_opt%cmd(2:2)
            getopt=.TRUE.
            return
        else
            getopt = .FALSE.
        endif
    end function getopt

end module getoptf

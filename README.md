# getoptf.f90 

A Fortran implementation of getopt.

## Usage
```
do while(getopt(argc, argv, c ":f:v:12") /= -1) 
    select case (c)
        case ('f'):
            call appendFile(fileList, optarg)
        case ('v'):
            VERBOSE_FLAG = int(optarg)
        case ('1'):
            FLAG_1 = .TRUE.
        case ('2'):
            FLAG_2 = .TRUE.
        case (':'):
            write(0, *) "ERROR: Missing argument for opt ", optopt
            call usage()
            stop
        case ('?'):
            write(0, *) "ERROR: Unrecognized option!"
            call usage()
            stop
end do
```

## Example Usages
```
$ # All of the commands are equivilant
$ grep -l -i -f patterns *.c
$ grep -lif patterns *.c
$ grep -lifpatterns *c
$./a.out -v
$./a.out -f argument
$./a.out -f=argument
$./a.out -vf=argument
```

### Definitions
* Optstring - The string provided by the programmer that specifies the available
  options.
* Argument list (argv) - Can be any list of command line options, not just
  argv. But the list must be a string (character array). See the usage section
  above for examples.
* Here is an example of a valid character list:
   ```
    ./readfile -vi input_file -o=output_file
   ```
* Argument Count (argc) - The number of arguments in argv. Get this number
by using the `COMMAND_ARGUMENT_COUNT()` intrinsic function. If the argument
list is not supplied by the user (ie The programmer is specifying and creating
the argument list) use the getoptf - `get_arg_count()` command.

# Requirements 
* Retain most of the functionality as normal c getopt (see man 3 getopt)
* Options can be all alphanumeric characters ie. [a-zA-Z0-9]
    * Most special characters allowed
    * Invalid characters for options:
        1. Colon:           ':' 
        2. Question Mark:   '?' 
        3. Dash:            '-'
* A colon placed after an option ('v:') will mean that that option takes an argument
* The number of options allowed will only be limited by the set of possible
  characters for options. ie: The count of [a-zA-Z0-9] plus the allowed special
  characters.
* Portable - 100% Compatible with GNU, Intel, Portland Group 
* Function returns a logical type
    * If .TRUE.  - there are more options to be processed
    * If .FALSE. - The last option has been processed
* Contains the global variables `optind`, `optarg`, `opterr`, `optopt`,
  `optreset`
    * `optarg` - Points to an option argument 
    * `optintd` - Contains the index to the next argv argument. Can be set to
    skip over more or less argv entries
    * `opterr` - Can be set to 0 to turn off getoptf error messages
    * `optopt` - Saves the last known option character retuned by getoptf
    * `optreset`- If set to 1 - allows getoptf to evaluate multiple sets of
    arguments or to evaulate a single st of options more then once
* Handling Errors
    * Unrecognized Options - Upon receiving an unrecognized option, getoptf will
   set `c` to be `?` . If
        * `optopt` is set to the character that caused the error
    * Missing argument - If an argument is missing from an option, then `?` will
   be returned, unless `:` is specified at the front of the OptString then
   getoptf will return a `:` in the `c` variable.
        * `optopt` is set to the character that caused the error
* Opterr can be used to suppress getoptf error messages:
    * 1 (default) == produce error messages
    * 0 == Produce no error messages
    * Or error messages can be suppressed by placing a `:` at the front of
* Can process multiple arguments:
    * `./a.out -f file1 -f file2 -f file3`

# Design


## Resources
* https://www.gnu.org/software/libc/manual/html_node/Getopt.html
* https://gcc.gnu.org/onlinedocs/gfortran/GET_005fCOMMAND_005fARGUMENT.html#GET_005fCOMMAND_005fARGUMENT
* https://gcc.gnu.org/onlinedocs/gfortran/EXECUTE_005fCOMMAND_005fLINE.html#EXECUTE_005fCOMMAND_005fLINE
* https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-execute-command-line
* https://gcc.gnu.org/onlinedocs/gfortran/COMMAND_005fARGUMENT_005fCOUNT.html#COMMAND_005fARGUMENT_005fCOUNT
* https://gcc.gnu.org/onlinedocs/gfortran/GET_005fCOMMAND.html#GET_005fCOMMAND

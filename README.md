# getoptf.f90 

A Fortran implementation of getopt.

# TODO:

1. Design Document
2. Unit Tests!
3. Write code!

# Requirements 

## Usage
```
do while (getopt(argc, argv, c, "v:tf:c ") /= -1 )
```

## Example Usages
```
$ # All of the commands are equivilant
$ grep -l -i -f patterns *.c
$ grep -lif patterns *.c
* grep -lifpatterns *c
```
```
-v
-f argument
-f=argument
-vf=argument
```

### Definitions
* Optstring - The string provided by the programmer that specifies the available
  options.
* Argument list (argv) - Can be any list of command line options, not just
  argv. But the list must be a string (character array) and options must be
  specified as one of the following:
    * `-v`
    * `-f argument`
    * `-f=argument`
    * `-vf=argument`

   Here is an example of a valid character list:
   ```
    ./readfile -vi input_file -o=output_file
   ```
* Argument Count (argc) - The number of arguments in argv. Get this number
by using the `COMMAND_ARGUMENT_COUNT()` intrinsic function. If the argument
list is not supplied by the user (ie The programmer is specifying and creating
the argument list) use the getoptf - `get_arg_count()` command.


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
* Function returns an integer type
    * Returns -1 when the argument list is exhausted
    * Otherwise Returns 0
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

Take in a string of command line arguments from the user as well as a command 
line specification from the programmer and allow the programmer to obtain the 
values that are specified by the user.

Feature Ideas
* Give arguments default types
    * We could either give arguments default types or leave them as characters
    and leave it up to the programmer to type change. That might be a good
    thing as its not to hard for programmers to type change in Fortran and it
    allows flexibility. Changing the type for the programmer would be nice,
    but just might make things easier.
* Allow options to have >= 1 argument. 
    * That way we could do -f file1 file2 file3.
    * Or we could just do the normal getopt way which is -f file1 -f file2 -f file3
    etc.

## Programmer

* Parse a string of arguments and allocate an 'argument' type with the arguments 
information to be used to check against the input from the user.

## User
* Parse a string and grab the arguments while checking against the allocated
arguments specified by the programmer
    * If the argument is incorrect or does not match any allocated args, report
    an error.
    * If the argument does not contain a value when it should (or visa versa)
    then report an error
    * if the argument is good returns its value(s) to the programmer.

# Notes & Resources

Fortran poses a different challenge then C as it does not contain an argv 
variable that holds the entire command line arguments.

However, it does have the ability to get a SINGLE argument at a time via
GET_COMMAND_ARUGMENT(i) where i is the numbered argument you wish to get.
so we will have to use this to help us to parse through the command arguments.

THATS WRONG! It does via get_command! woohoo!


## Resources
* https://www.gnu.org/software/libc/manual/html_node/Getopt.html
* https://gcc.gnu.org/onlinedocs/gfortran/GET_005fCOMMAND_005fARGUMENT.html#GET_005fCOMMAND_005fARGUMENT
* https://gcc.gnu.org/onlinedocs/gfortran/EXECUTE_005fCOMMAND_005fLINE.html#EXECUTE_005fCOMMAND_005fLINE
* https://software.intel.com/en-us/fortran-compiler-developer-guide-and-reference-execute-command-line
* https://gcc.gnu.org/onlinedocs/gfortran/COMMAND_005fARGUMENT_005fCOUNT.html#COMMAND_005fARGUMENT_005fCOUNT
* https://gcc.gnu.org/onlinedocs/gfortran/GET_005fCOMMAND.html#GET_005fCOMMAND

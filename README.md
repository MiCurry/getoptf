# getoptf.f90 

A Fortran implementation of getopt.


# Design

Take in a string of command line arguments from the user as well as a command 
line specification from the programmer and allow the programmer to obtain the 
values that are specifed by the user.

Feature Ideas
* Give agruments default types
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
infromation to be used to check against the input from the user.

## User
* Parse a string and grab the arguments while checking against the allocated
arguments specifed by the programmer
    * If the argument is incorrect or does not match any allocated args, report
    an error.
    * If the argument does not contain a value when it should (or visa versa)
    then report an error
    * if the argument is good returns its value(s) to the programmer.

# Notes

Fortran poses a different challange then C as it does not contain an argv 
variable that holds the entire command line arguments.

However, it does have the ability to get a SINGLE argument at a time via
GET_COMMAND_ARUGMENT(i) where i is the numbered argument you wish to get.
so we will have to use this to help us to parse through the command arguments.

THATS WRONG! It does via get_command! woohoo!


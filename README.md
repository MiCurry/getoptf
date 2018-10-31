# getoptf.f90 

A Fortran implementation of getopt.


# Design

Take in a string of command line arguments from the user as well as a command 
line specification from the programmer and allow the programmer to obtain the 
values that are specifed by the user.


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

## Notes

Fortran poses a different challange then C as it does not contain an argv 
variable that holds the entire command line arguments.

However, it does have the ability to get a SINGLE argument at a time via
GET_COMMAND_ARUGMENT(i) where i is the numbered argument you wish to get.
so we will have to use this to help us to parse through the command arguments.

THATS WRONG! It does via get_command! woohoo!

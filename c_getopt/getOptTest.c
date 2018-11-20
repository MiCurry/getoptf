#include <unistd.h>
#include <stdio.h>


int main(int argc, char **argv)
{
    int c;

    while((c = getopt(argc, argv, "v?::??")) != -1) {
        switch(c) {
            case('v'):
                printf("Verbose!\n");
                break;
            case('-'):
                printf("Dash!!\n");
                break;
            case('?'):
                printf("??\n");
                break;
        }
    }
}

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>



void format_time(char * argc, unsigned char ** output, int *n){
    time_t rawtime;
    struct tm * timeinfo;
    static char out[100];
    time ( &rawtime );
    timeinfo = localtime ( &rawtime );
    strftime(out, 100, argc, timeinfo);
    *output = out;
    *n = strlen(*output); 
}
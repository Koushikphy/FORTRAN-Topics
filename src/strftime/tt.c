#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <string.h>



void format_time(char * argc, char * output, int *n){
    time_t rawtime;
    struct tm * timeinfo;
    time ( &rawtime );
    timeinfo = localtime ( &rawtime );
    strftime(output, 100, argc, timeinfo);
    *n = strlen(output); 
}



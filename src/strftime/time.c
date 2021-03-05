#include <stdlib.h>
#include <stdio.h>
#include <time.h>


char *format_time(char * argc){
    static char output[50];
    time_t rawtime;
    struct tm * timeinfo;
    time ( &rawtime );
    timeinfo = localtime ( &rawtime );
    strftime(output, sizeof(output), argc, timeinfo);
    return output;
}
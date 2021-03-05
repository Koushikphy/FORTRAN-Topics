#include <stdlib.h>
#include <stdio.h>



float myFunc(float x, float y){
    return x+y;
}


float myFuncRef(float *x, float *y){
    return *x+*y;
}


float myfuncauto_(float *x, float *y){
    return *x+*y;
}
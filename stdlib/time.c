#include <stdlib.h> 
#include <stdio.h> 
#include <string.h> 
#include <sys/time.h>

int main(int argc, char** args) {
  struct timeval tv_0;
  struct timeval tv_1;
  gettimeofday(&tv_0,0);
  system(args[1]);
  gettimeofday(&tv_1,0);
  printf("%s%d\n",
         //
         // second argument is the line prefix for the output...
         //
         args[2],
         //
         // ...and line ends with time, in micro seconds.
         //
         (int) ((tv_1.tv_sec - tv_0.tv_sec) * 1000000)
         +
         (int) (tv_1.tv_usec - tv_0.tv_usec)
         );
}

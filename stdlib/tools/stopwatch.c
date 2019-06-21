#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

int main(int argc, char** args) {
  struct timeval tv_0;
  struct timeval tv_1;

  if (argc < 2) {
    fprintf(stderr, "usage: %s <command-string> <output-line-prefix>\n", args[0]);
    return -1;
  }

  gettimeofday(&tv_0,0);
  //
  // first argument is the shell command to run and whose running time we measure:
  //
  system(args[1]);
  gettimeofday(&tv_1,0);
  printf("%s%.3f\n",
         //
         // second argument is the line prefix for the output...
         //
         args[2],
         //
         // ...and the output line ends with the measured time, in milliseconds.
         //
         ((float)
          (
           (int) ((tv_1.tv_sec - tv_0.tv_sec) * 1000000)
           +
           (int) (tv_1.tv_usec - tv_0.tv_usec) )
          )
         / 1000.0f
         );
}

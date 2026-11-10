#include <unistd.h>
#include <sys/times.h>

float etime(float *tarray)
{
struct tms t;
  (void) times(&t);
/*

---------<sys/times.h>---------------
           struct tms {
               clock_t    tms_utime;
               clock_t    tms_stime;
               clock_t    tms_cutime;
               clock_t    tms_cstime;
           };
-------------------------------------

tms_utime  :  user time
tms_stime  :  system time
tms_cutime :  user time, children
tms_cstime :  system time, children

unit 1/CLK_TCK sec

*/
  return (float)t.tms_utime/(float)sysconf(_SC_CLK_TCK);
}

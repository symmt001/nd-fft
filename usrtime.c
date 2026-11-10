#include <unistd.h>
#include <sys/times.h>

double usrtime_()
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
  return (double)t.tms_utime/(double)sysconf(_SC_CLK_TCK);
}

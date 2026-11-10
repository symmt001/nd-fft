/* See http://www.delorie.com/djgpp/doc/brennan/brennan_att_inline_djgpp.htm/ */
#include <stdio.h>

/*
void main (){
  int *b;
  b=malloc(sizeof(double));
  *b = 1;
  asm (
	"movl $12, %%ebx \n\t"
	"movl $13, %%ecx \n\t"
	"addl %%ebx, %%ecx \n\t"
	"movl %%ecx, (%%eax) \n\t"
	::"a"(b):"%ebx","%ecx","memory"
	);
  printf("Result:%i\n", *b);
}
*/

void main (){

  union {
	double d[8];
	long double q[4];
  } buffer;

  
  double *a, *b;
  long long int t[2];

  a=buffer.d;
  b=a+4;

  a[0] = 1.0;
  a[1] = 2.0; // (1.0 + i 2.0)
  a[2] = 3.0;
  a[3] = 4.0; // (3.0 + i 4.0)
  b[0] = 0.0;
  b[1] = 0.0; // the answer will be stored here

  asm (
	   "pushl %%edx \n\t"

	   "movl $-1, %%edx \n\t"
	   "pushl %%edx \n\t"
	   "negl  %%edx \n\t"
	   "pushl %%edx \n\t"
	   "cvtpi2pd (%%esp), %%xmm7 \n\t"
	   "addl $8, %%esp\n\t"

	   "pushl %%eax \n\t"
	   "rdtsc \n\t"
	   "movl %%eax,  (%%ecx) \n\t"
	   "movl %%edx, 4(%%ecx) \n\t"
	   "popl %%eax \n\t"

	   "movl $1000000,  %%edx\n\t"
	   "LoopS:\n\t"
	   "decl        %%edx \n\t"
       "jle LoopE \n\t"
	   /*
	   "movapd     %%xmm7, %%xmm1 \n\t" //xmm1:(-1,1)
	   "movapd    (%%eax), %%xmm0 \n\t" //xmm0:(b,a)
	   "mulpd      %%xmm0, %%xmm1 \n\t" //xmm1:(-b,a)
	   "movapd  16(%%eax), %%xmm2 \n\t" //xmm2:(d,c)
	   "shufpd $1, %%xmm0, %%xmm0 \n\t" //xmm0:(a,b)
	   "mulpd      %%xmm2, %%xmm1 \n\t" //xmm1:(-bd,ac)
	   "mulpd      %%xmm2, %%xmm0 \n\t" //xmm0:( ad,bc)
	   "movapd     %%xmm1, %%xmm2 \n\t" //xmm2:(-bd,ac)
	   "unpcklpd   %%xmm0, %%xmm1 \n\t" //xmm1:(bc,ac)
	   "unpckhpd   %%xmm0, %%xmm2 \n\t" //xmm2:(ad,-bd)
	   "addpd      %%xmm1, %%xmm2 \n\t" //xmm2:(ad+bc,ac-bd)
	   "movapd     %%xmm2, (%%ebx) \n\t"
	   */
	   /*
        "movsd    (%%eax), %%xmm2 \n\t"
        "movsd   8(%%eax), %%xmm1 \n\t"
        "movsd  16(%%eax), %%xmm5 \n\t"
        "movsd  24(%%eax), %%xmm3 \n\t"
        "movapd    %%xmm2, %%xmm4 \n\t"
        "movapd    %%xmm3, %%xmm0 \n\t"
        "mulsd     %%xmm2, %%xmm3 \n\t"
        "mulsd     %%xmm5, %%xmm4 \n\t"
        "mulsd     %%xmm1, %%xmm0 \n\t"
        "mulsd     %%xmm1, %%xmm5 \n\t"
        "subsd     %%xmm0, %%xmm4 \n\t"
        "movsd     %%xmm4, (%%ebx)\n\t"
        "addsd     %%xmm3, %%xmm5 \n\t"
        "movsd     %%xmm5,8(%%ebx)\n\t"
	   */

	   /*
	   "movapd     (%%eax), %%xmm3 \n\t" //xmm0:(b,a)
	   "xorpd       %%xmm4, %%xmm4 \n\t" //xmm1:(0,0)
	   "movapd   16(%%eax), %%xmm5 \n\t" //xmm2:(d,c)
	   "shufpd  $1, %%xmm3, %%xmm3 \n\t" //xmm0:(a,b)
	   "subsd    24(%%eax), %%xmm4 \n\t" //xmm1:(0,-d)
	   "unpcklpd    %%xmm5, %%xmm4 \n\t" //xmm1:(c,-d)
	   "mulpd       %%xmm3, %%xmm5 \n\t" //xmm2:(ad,bc)
	   "mulpd       %%xmm3, %%xmm4 \n\t" //xmm1:(ac,-bd)
	   "movapd      %%xmm4, %%xmm3 \n\t" //xmm0:(ac,-bd)
	   "unpcklpd    %%xmm5, %%xmm4 \n\t" //xmm1:(bc,-bd)
	   "unpckhpd    %%xmm5, %%xmm3 \n\t" //xmm0:(ad,ac)
	   "addpd       %%xmm4, %%xmm3 \n\t" //xmm0:(ad+bc,ac-bd)
	   "movapd      %%xmm3, (%%ebx) \n\t"
	   */
	   "movapd     %%xmm7, %%xmm1 \n\t" //xmm1:(-1,1)
	   "movapd    (%%eax), %%xmm0 \n\t" //xmm0:(b,a)
	   "mulpd      %%xmm0, %%xmm1 \n\t" //xmm1:(-b,a)
	   "movapd  16(%%eax), %%xmm2 \n\t" //xmm2:(d,c)
	   "shufpd $1, %%xmm0, %%xmm0 \n\t" //xmm0:(a,b)
	   "mulpd      %%xmm2, %%xmm1 \n\t" //xmm1:(-bd,ac)
	   "mulpd      %%xmm2, %%xmm0 \n\t" //xmm0:( ad,bc)
	   "movapd     %%xmm1, %%xmm2 \n\t" //xmm2:(-bd,ac)
	   "unpcklpd   %%xmm0, %%xmm1 \n\t" //xmm1:(bc,ac)
	   "unpckhpd   %%xmm0, %%xmm2 \n\t" //xmm2:(ad,-bd)
	   "addpd      %%xmm1, %%xmm2 \n\t" //xmm2:(ad+bc,ac-bd)
	   "movapd     %%xmm2, (%%ebx) \n\t"

	   "jmp LoopS \n\t"
	   "LoopE: \n\t"

	   "pushl %%eax \n\t"
	   "rdtsc \n\t"
	   "movl %%eax, 8(%%ecx) \n\t"
	   "movl %%edx,12(%%ecx) \n\t"
	   "popl %%eax \n\t"

	   "popl %%edx \n\t"

	   :
	   :"a"(a),"b"(b),"c"(t)
	   :"edx", "xmm0","xmm1","xmm2","xmm7", "memory"
	);


  printf("Input :%e,%e,%e,%e\n", *a,*(a+1),*(a+2),*(a+3));
  printf("Output:%e,%e\n", *b,*(b+1));
  printf("CLOCK: %lli\n", t[1]-t[0]);
}

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

  /*
  __declspec(align(16)) double buf[6];
  */

  union{
	double d[6];
	long double q[3];
  } buf;
  double *a, *b;
  long long int t[2];

  a=buf.d;
  b=a+4;

  a[0] = 1.0;
  a[1] = 2.0; // (1.0 + i 2.0)
  a[2] = 3.0;
  a[3] = 4.0; // (3.0 + i 4.0)
  b[0] = 0.0;
  b[1] = 0.0; // the answer will be stored here

  asm (
	   "pushl %%edx \n\t"             //(high-addr,low-addr)

	   "movl $1, %%edx \n\t"
	   "pushl %%edx \n\t"              //push 1
	   "negl  %%edx \n\t"
	   "pushl %%edx \n\t"              //push -1
	   "cvtpi2pd (%%esp), %%xmm7 \n\t" //xmm7:(1,-1)
	   "addl $8, %%esp\n\t"

	   "movlpd 8(%%eax), %%xmm6\n\t"
	   "movhpd 8(%%eax), %%xmm6\n\t" //xmm6:(b,b)
	   "mulpd   %%xmm6,  %%xmm7\n\t" //xmm7:(b,-b)
	   "movlpd  (%%eax), %%xmm6\n\t"
	   "movhpd  (%%eax), %%xmm6\n\t" //xmm6:(a,a)
	   

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
	   "movapd  16(%%eax), %%xmm0 \n\t" //xmm0:(d,c)
	   "movapd     %%xmm0, %%xmm1 \n\t" //xmm1:(d,c)
	   "shufpd $1, %%xmm0, %%xmm0 \n\t" //xmm0:(c,d)
	   "mulpd      %%xmm6, %%xmm1 \n\t" //xmm1:(ad,ac)
	   "mulpd      %%xmm7, %%xmm0 \n\t" //xmm0:(bc,-bd)
	   "addpd      %%xmm1, %%xmm0 \n\t" //xmm0:(ad+bc,ac-bd)
	   "movapd     %%xmm0, (%%ebx) \n\t"
	   */

	   "movapd  16(%%eax), %%xmm0 \n\t" //xmm0:(d,c)
	   "movapd     %%xmm0, %%xmm1 \n\t" //xmm1:(d,c)
	   "shufpd $1, %%xmm0, %%xmm0 \n\t" //xmm0:(c,d)
	   "mulpd      %%xmm6, %%xmm1 \n\t" //xmm1:(ad,ac)
	   "mulpd      %%xmm7, %%xmm0 \n\t" //xmm0:(bc,-bd)
	   "addpd      %%xmm1, %%xmm0 \n\t" //xmm0:(ad+bc,ac-bd)
	   "movapd     %%xmm0, (%%ebx) \n\t"

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
	   :"edx", "xmm0","xmm1","xmm2","xmm6","xmm7", "memory"
	);


  printf("Input :%e,%e,%e,%e\n", *a,*(a+1),*(a+2),*(a+3));
  printf("Output:%e,%e\n", *b,*(b+1));
  printf("CLOCK: %lli\n", t[1]-t[0]);
}

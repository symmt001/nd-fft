#include <stdio.h>
#include <emmintrin.h>

void main (){

  union {
	double d[8];
	__m128 m[4];
  } buffer;

  double *a, *b;

  a=buffer.d;
  b=a+4;

  a[0] = 1.0;
  a[1] = 2.0; // (1.0, 2.0)
  a[2] = 3.0;
  a[3] = 4.0; // (3.0, 4.0)
  b[0] = 0.0;
  b[1] = 0.0; // the answer will be stored here

   asm (

        "movapd    (%%eax), %%xmm0 \n\t"
        "movapd  16(%%eax), %%xmm1 \n\t"
        "addpd     %%xmm1,  %%xmm0 \n\t"
        "movapd    %%xmm0, (%%ecx)\n\t"

	   :
	   :"a"(a),"c"(b)
	   :"xmm0","xmm1", "memory"
	);

   printf("Output:%e,%e\n", *b,*(b+1));
}

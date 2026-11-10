/* See http://www.delorie.com/djgpp/doc/brennan/brennan_att_inline_djgpp.htm/ */
#include <stdio.h>

long long int get_rt(){
    asm (
		 "rdtsc \n\t"
	   :::
	);
}

/*
Fortran version, transp_0.f:1.538005e+02 Mbyetes/sec (16+16 bytes read+write per one complex)
Fortran version, transp_1.f:3.759964e+02 Mbyetes/sec (16+16 bytes read+write per one complex)
Fortran version, transp_2.f:5.743686e+02 Mbyetes/sec (16+16 bytes read+write per one complex)
Assembly version trans1.s:  6.810118e+02 Mbyetes/sec (16+16 bytes read+write per one complex)
*/
void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] );



/* for Reference, simple copy:
   -O3   :1.000233e+03 Mbyetes/sec (16+16 bytes read+write per one complex)
   -xW   :1.002818e+03 Mbyetes/sec (16+16 bytes read+write per one complex)
void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){
  int i;
  for(i=0; i<2 * *p * *q ; i++){
	dst[i]=src[i];
  }
}
*/
	  //dst[2*(k * *p + j)]  =src[2*(j * *q + k)];
	  //dst[2*(k * *p + j)+1]=src[2*(j * *q + k)+1];
		  /*
		  "movsd  (%%eax), %%xmm0 \n\t"
		  "movsd 8(%%eax), %%xmm1 \n\t"
		  "movsd %%xmm0,  (%%ebx) \n\t"
		  "movsd %%xmm1, 8(%%ebx) \n\t"
		  */

/*
  int pq2, one;
  one=1; pq2=*p * *q *2;
  dcopy_(&pq2,src,&one,dst,&one);
  //8.769951e+02 Mbyetes/sec (16+16 bytes read+write per one complex)
*/




/*
Inline ASM3.163518e+02 Mbyetes/sec (16+16 bytes read+write per one complex)

void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){

  double *cp_from, *cp_to;
  int j,k;
  for( j=0; j< *p; j ++ ){

  
	for( k=0; k< *q; k++ ){
	
	}
	for( k=0; k< *q; k++ ){

	  //dst[2*(k * *p + j)]  =src[2*(j * *q + k)];
	  //dst[2*(k * *p + j)+1]=src[2*(j * *q + k)+1];

	  cp_from=src+2*(j* *q +k);
	  cp_to  =dst+2*(k* *p+j);

	  asm(
		  "movapd (%%eax), %%xmm0 \n\t"   
		  "movntpd %%xmm0, (%%ebx) \n\t"

		  :
          :"a"(cp_from),"b"(cp_to)
		  :"memory"
	  );

	}
  }
}
*/

/*
void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){

  double *cp_from, *cp_to;
  int j,k,dsrc,ddst;
  for( j=0; j< *p; j += 1 ){
	for( k=0; k< *q; k += 2 ){

	  //dst[2*(k * *p + j)]  =src[2*(j * *q + k)];
	  //dst[2*(k * *p + j)+1]=src[2*(j * *q + k)+1];

	  cp_from=src+2*(j* *q +k);
	  cp_to  =dst+2*(k* *p+j);
	  dsrc   =16 * *q;
	  ddst   =16 * *p;

	  asm(
		  "movapd    (%%eax), %%xmm0 \n\t"
		  "movapd  16(%%eax), %%xmm1 \n\t"
		  //"addl    %%edx, %%eax \n\t"
		  //"movapd    (%%eax), %%xmm2 \n\t"
		  //"movapd  16(%%eax), %%xmm3 \n\t"
		  "movntpd %%xmm0,  (%%ebx) \n\t"
		  //"movntpd %%xmm2,16(%%ebx) \n\t"
		  "addl    %%ecx, %%ebx \n\t"
		  "movntpd %%xmm1,  (%%ebx) \n\t"
		  //"movntpd %%xmm3,16(%%ebx) \n\t"

		  :
          :"a"(cp_from),"b"(cp_to),"c"(ddst),"d"(dsrc)
		  :"memory"
	  );

	}
  }
}
*/


#define ROW 65536
#define COL 256

void main (){
  long long int t[2];
  int i, row, col, tot;

  double *src, *dst, mbps;

  row=ROW;
  col=COL;
  tot=row*col;
  printf("sizeof(double)=%i, (row,col)=(%i,%i)\n", sizeof(double), row, col);
  src=malloc(2*sizeof(double)*(tot+1));
  dst=malloc(2*sizeof(double)*(tot+1));
  src++;
  dst++;

  for(i=0; i< tot ; i++){
	src[2*i]  =(double) i/10;
	src[2*i+1]=(double)-i/10;
  }

  t[0]=get_rt();
  transp_(&row, &col, src, dst);
  t[1]=get_rt();
  printf("Input :%e,%e,%e,%e\n",src[0],src[1],src[2],src[3]);
  printf("Output:%e,%e,%e,%e\n",dst[0],dst[1],dst[2],dst[3]);
  printf("CLOCK: %lli\n", t[1]-t[0]);
  mbps=2e3*tot*32/(double)(t[1]-t[0]); //2GHz clock
  printf("%e Mbyetes/sec (16+16 bytes read+write per one complex)\n", mbps );
}

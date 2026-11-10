/* See http://www.delorie.com/djgpp/doc/brennan/brennan_att_inline_djgpp.htm/ */

#include <stdio.h>
#include <stdlib.h>

long long int get_rt(){
    asm (
		 "rdtsc \n\t"
	   :::
	);
}

/* for Reference, simple copy:
   -O3   :1.000233e+03 Mbyetes/sec (16+16 bytes read+write per one complex)
   -xW   :1.002818e+03 Mbyetes/sec (16+16 bytes read+write per one complex)
*/
#define UNRL 16
void copy_(int *num,
		   double src[],
		   double dst[] ){
  int i;
  for(i=0; i<= *num - UNRL ; i += UNRL){
	dst[i]=src[i];
	dst[i+1]=src[i+1];
	dst[i+2]=src[i+2];
	dst[i+3]=src[i+3];
	dst[i+4]=src[i+4];
	dst[i+5]=src[i+5];
	dst[i+6]=src[i+6];
	dst[i+7]=src[i+7];
	dst[i+8]=src[i+8];
	dst[i+9]=src[i+9];
	dst[i+10]=src[i+10];
	dst[i+11]=src[i+11];
	dst[i+12]=src[i+12];
	dst[i+13]=src[i+13];
	dst[i+14]=src[i+14];
	dst[i+15]=src[i+15];
  }
  for(; i< *num ; i++){
	dst[i]=src[i];
  }
}

#define MAX_N 1024*1024*100
#define Min_C 2e6

void main (){
  long long int t[2], tot, totb;
  int i,j,n, num;

  double *msrc, *mdst, *src, *dst, mbps;

  msrc=malloc(sizeof(double));
  mdst=malloc(sizeof(double));

  printf("block size, MByte/sec\n");
  for( num=1; num< MAX_N ; num *= 2 ){
	/*
	printf("number of elements, total byte=%i,%i\n", num,
		   num*sizeof(double)*2);
	*/
	free(msrc);
	free(mdst);
	msrc=malloc(sizeof(double)*(num+1));
	mdst=malloc(sizeof(double)*(num+1));
	src=msrc;
	dst=mdst;
	if( ((int)src & 0x0000000f) != 0 ){
	  //printf("%i\n",((int)src & 0x0000000f));
	  src++;
	}
	if( ((int)dst & 0x0000000f) != 0 ){
	  //printf("%i\n",((int)dst & 0x0000000f));
	  dst++;
	}
	
	for(i=0; i< num ; i++){
	  src[i]  =(double) i/10;
	}
	
	t[0]=get_rt();
	copy_(&num, src, dst);
	t[1]=get_rt();
	tot=t[1]-t[0];
	totb=num*sizeof(double)*2;
	if((double)tot < Min_C ){
	  n=Min_C/(double)tot;
	  t[0]=get_rt();
	  for(j=0; j< n ; j++){
		copy_(&num, src, dst);
	  }
	  t[1]=get_rt();
	  tot=t[1]-t[0];
	  totb*=n;
	}
	
	//printf("CLOCK: %lli\n", tot);
	mbps=2e3*totb/(double)tot; //2GHz clock, 8+8, read+write
	printf("%i, %e\n", num*sizeof(double),mbps );
  }
}

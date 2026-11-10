/*
************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
************************************************************************
*/
/* original
void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){
  int j,k;
  for( j=0; j< *p; j++ ){
	for( k=0; k< *q; k++ ){
	  dst[2*(k * *p + j)]  =src[2*(j * *q + k)];
	  dst[2*(k * *p + j)+1]=src[2*(j * *q + k)+1];
	}
  }
}
*/


/*
#include <emmintrin.h>

#define PAGESIZE 4096
#define NUM_PER_PAGE 256
*/
/* NUM_PER_PAGE = PAGESIZE/16 */

/* 

             // This is not a good idea.

void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){
  double tmp;
  int j,k, jq, kp, kk;
  for( j=0; j< *p; j++ ){
	jq=j * *q;
	tmp=src[jq + 2 * NUM_PER_PAGE]; // TLB setup
	for( k=0; k <= *q-NUM_PER_PAGE; k += NUM_PER_PAGE ){
	  for( kk=k+8; kk< kk+NUM_PER_PAGE; k+=8 ){
		_mm_prefetch((char*)&src[2*(jq+kk)], _MM_HINT_NTA);
	  }
	  for( kk=k; kk< kk+NUM_PER_PAGE; k+=8 ){
		kp=k* *p;
		_mm_stream_ps((float*)&dst[2*(kp+j)],
					  _mm_load_ps((float*)&src[2*(jq+kk)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+   *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+1)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+2* *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+2)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+3* *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+3)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+4* *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+4)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+5* *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+5)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+6* *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+6)]));
		_mm_stream_ps((float*)&dst[2*(kp+j+7* *p)],
					  _mm_load_ps((float*)&src[2*(jq+kk+7)]));
	  }
	}
	for( ; k < *q; k++ ){
	  _mm_prefetch((char*)&src[2*(jq+k)], _MM_HINT_NTA);
	  kp=k* *p;
	  _mm_stream_ps((float*)&dst[2*(kp+j)],
					  _mm_load_ps((float*)&src[2*(jq+k)]));
	}
  }
}
*/

void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){
  //long long int t[2];

  double *cp_from, *cp_to;
  int j,k,l,m;
  for( j=0; j< *p; j ++ ){
	for( k=0; k< *q; k ++ ){
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

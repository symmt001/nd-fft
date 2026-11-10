#include <math.h>
#include "fftw.h"

int nw[7],total_n;
fftwnd_plan fplan, bplan;

/*  typedef struct {double re, im;} complex; */
/*  printf("factor=%e,total=%i\n",factor_inv,total_n);*/

void fftprep1_(int *nd, int *nmax, int *n, int *bitrev)
{
  int i;
/* total_n is a global variable */
  total_n=1;

  for(i=0 ; i<*nd; i++){
	nw[i]=1<<n[(*nd)-i-1];
	total_n *= nw[i];
  };

#define SY_SPECIFIC
#define SY_INPLACE
  /* ****CAUTION*** OUT PLACE doesn't work */

#ifndef SY_SPECIFIC
#ifdef SY_INPLACE
  fplan=fftwnd_create_plan
	(*nd, nw, FFTW_FORWARD,  FFTW_ESTIMATE | FFTW_IN_PLACE );
  bplan=fftwnd_create_plan
	(*nd, nw, FFTW_BACKWARD, FFTW_ESTIMATE | FFTW_IN_PLACE );
#else
  fplan=fftwnd_create_plan
	(*nd, nw, FFTW_FORWARD,  FFTW_ESTIMATE );
  bplan=fftwnd_create_plan
	(*nd, nw, FFTW_BACKWARD, FFTW_ESTIMATE );
#endif
#else
  {
	FFTW_COMPLEX *cwk=
	  (FFTW_COMPLEX*)malloc(sizeof(FFTW_COMPLEX)*total_n);
	if(cwk==NULL){
	  printf("Error in fftprep1_ : Can't malloc cwk\n");
	  exit(1);
	}
#ifdef SY_INPLACE
	fplan=fftwnd_create_plan_specific
	  (*nd, nw, FFTW_FORWARD,  FFTW_MEASURE| FFTW_IN_PLACE ,cwk,1,cwk,1);
	bplan=fftwnd_create_plan_specific
	  (*nd, nw, FFTW_BACKWARD, FFTW_MEASURE| FFTW_IN_PLACE ,cwk,1,cwk,1);
#else
	fplan=fftwnd_create_plan_specific
	  (*nd, nw, FFTW_FORWARD,  FFTW_MEASURE ,cwk,1,cwk,1);
	bplan=fftwnd_create_plan_specific
	  (*nd, nw, FFTW_BACKWARD, FFTW_MEASURE ,cwk,1,cwk,1);
#endif
	free(cwk);
  }
#endif
}

void fftnd_(int *nd, int *nmax, int *n, FFTW_COMPLEX *z, FFTW_COMPLEX *cwk,
		   double *phase, int *bitrev)
{
  if(*(phase+3)>0){
	fftwnd(bplan, 1, z, 1, 0 , cwk, 1, 0);
  }
  else{
	fftwnd(fplan, 1, z, 1, 0 , cwk, 1, 0);
  };
}

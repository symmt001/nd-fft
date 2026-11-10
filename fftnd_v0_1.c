/*
************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension z(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2 & fftprep3
*     cwk:    Work Area
*     bitrev: Bit Reversal Table Prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+...+n(nd))
*     Written by S. Yamamoto
*     
*     Comments:
*     *odir       directive to the optimizer on SX-3   (NEC)
*     *vdir       directive to the compiler  on SX-3   (NEC)
*     *VOPTION    directive to the compiler  on S-3800 (HITACHI)
*     *VOCL       directive to the compiler  on VPP    (FUJITSU)
************************************************************************
*/

extern void rearr_( int *p, int *q, double *src, double *dst, int *bitrev);

extern void fftnd_(int *nd, int *nmax, int *n, double *z,
			 double *cwk, double *phase, int *bitrev)
{
  double xp, yp, xt, yt, x0, x1, y0, y1, *src, *dst;
  int   i,j,k,l,m,p,q,axis,iside, ia;
  iside=1;
  m=0;
  for( axis=0 ; axis < *nd ; axis++ ){
	m += *(n+axis);
  }
  m = 1 << (m-1);
  for( axis=0 ; axis < *nd ; axis++ ){
	l=1 << (*(n+axis)-1);
	p=l;
	q=m/l;
	/* bit-reversal */
	ia=2*l;
	if(iside == 1){
	  rearr_(&ia, &q, z, cwk, bitrev+axis*(*nmax) );
	}
	else{
	  rearr_(&ia, &q, cwk, z, bitrev+axis*(*nmax) );
	}
	/*  exchange z and cwk */
	iside = -iside;
	for( i=0; i < *(n+axis) ; i++ ){
	  for( k=0;  k < l; k += p ){
		xp=*(phase+  2*k+axis*(*nmax));
		yp=*(phase+1+2*k+axis*(*nmax));
		/* Doing radix 2 DIT & transpose at the sametime */
		if(iside == 1){
		  src=z;
		  dst=cwk;
		}else{
		  src=cwk;
		  dst=z;
		}
		for( j=k*q; j < (k+p)*q ; j++ ){
		  x0=*(src+4*j);
		  y0=*(src+4*j+1);
		  x1=*(src+4*j+2);
		  y1=*(src+4*j+3);
		  xt=x1*xp-y1*yp;
		  yt=x1*yp+y1*xp;
		  *(dst+2*   j   )=x0+xt;
		  *(dst+2*   j +1)=y0+yt;
		  *(dst+2*(m+j)  )=x0-xt;
		  *(dst+2*(m+j)+1)=y0-yt;
		}
	  }
	  p /= 2;
	  /* exchange z and cwk */
	  iside=-iside;
	}
  }
  if (iside != 1) {
	for ( i=0; i< 4*m ; i++ ){
	  *(z+i)=*(cwk+i);
	}
  }
}

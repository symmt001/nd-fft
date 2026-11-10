#define NRL 32
typedef struct {double re,im;}complex;

complex buf[32][32];

void transp_( int *q, int *p, complex *src, complex *dst){
  int j, k, jj, kk;
  complex *l, *jq, *kp;
  jq=src;
  for( j=0; j<=*p-NRL; j+= NRL ){
	kp=dst+j;
	for( k=0; k<=*q-NRL; k+=NRL ){
	  l=jq;
	  for( jj=0; jj<NRL; jj++ ){
		for( kk=0; kk<NRL; kk++ ){
		  buf[jj][kk]=l[k+kk];
		}
		l+=*q;
	  }
	  for( jj=0; jj<NRL; jj++ ){
		for( kk=0; kk<NRL; kk++ ){
		  kp[kk]=buf[kk][jj];
		}
		kp+=*p;
	  }
	}
	jq += NRL**q;
  }
}

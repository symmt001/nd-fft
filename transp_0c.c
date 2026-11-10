void transp_( int *q, int *p,
			  double src[],
			  double dst[] ){
  int j,k;
  for( j=0; j< *p; j++ ){
	for( k=0; k< *q; k++ ){
	  dst[2*(k * *p + j)]  =src[2*(j * *q + k)];
	  dst[2*(k * *p + j)+1]=src[2*(j * *q + k)+1];
	}
  }
}

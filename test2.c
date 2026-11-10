void transp_( int *q, int *p,
			  double src[2* *p * *q],
			  double dst[2* *p * *q] ){
  int i;
  for(i=0; i<2 * *p * *q ; i++){
	dst[i]=src[i];
  }
}

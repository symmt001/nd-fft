************************************************************************
*     rearrange complex data as follows
*     src(bitrev(p)+p,q)->dst(p,q)
*     Caution! p must be power of 2.
*     Caution! Not compatible with rearr_0.f
*     This routine must be used with fftprep1_1.f
************************************************************************
      subroutine rearr(p,q,src,dst,bitrev)
      implicit none
      integer p,q,i
      integer bitrev(0:p-1)
      complex*16 src(0:p*q-1),dst(0:p*q-1)
c     maximizing vector length for vector machines
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*VOPTION INDEP
*POPTION INDEP
      do i=0, p*q-1
         dst(i)=src(bitrev(iand(i,p-1))+i)
      end do
      end

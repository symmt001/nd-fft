************************************************************************
*     transpose+bitreversal on second argument
*     [complex*16 src(q,bitrev(p))] -> [complex*16 dst(p,q)]
*     [complex*16 src(q,p)] -> [complex*16 dst(bitrev(p),q)]
************************************************************************
      subroutine transb(q,p,src,dst,bitrev)
      implicit none
      integer p,q,j,k
      integer bitrev(0:p-1)
      complex*16 dst(0:q*p-1),src(0:q*p-1)
*POPTION INDEP
      do j=0, p-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do k=0, q-1
            dst(k*p+j)=src(bitrev(j)*q+k)
         end do
      end do
      end

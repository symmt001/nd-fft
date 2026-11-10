************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k
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
            dst(k*p+j)=src(j*q+k)
         end do
      end do
      end

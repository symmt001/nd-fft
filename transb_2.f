************************************************************************
*     transpose + bit-reversal on the second argument
*     [complex*16 src(q,bitrev(p))] -> [complex*16 dst(p,q)]
*     [complex*16 src(q,p)] -> [complex*16 dst(bitrev(p),q)]
*     This routine is designed especially for the case
*     that both q and p equal to 2**n. But it works on the other case.
************************************************************************
      subroutine transb(q,p,src,dst,bitrev)
      implicit none
      integer p,q,j,k
      integer bitrev(0:p-1)
      complex*16 dst(0:q*p-1),src(0:q*p-1)
c     Avoiding Bank Conflict
      if( q.ge.p ) then
*POPTION INDEP
         do k=0, q-p
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1
               dst((k+j)*p+bitrev(j))=src(j*q+(k+j))
            end do
         end do
*POPTION INDEP
         do k=1, p-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
               dst((q-k+j)*p+bitrev(j))=src(j*q+(q-k+j))
               dst(j*p+bitrev(p-k+j))=src((p-k+j)*q+j)
            end do
         end do
      else
*POPTION INDEP
         do k=0, p-q
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, q-1
               dst(j*p+bitrev(k+j))=src((k+j)*q+j)
            end do
         end do
*POPTION INDEP
         do k=1, q-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
               dst((q-k+j)*p+bitrev(j))=src(j*q+(q-k+j))
               dst(j*p+bitrev(p-k+j))=src((p-k+j)*q+j)
            end do
         end do
      end if
      end 

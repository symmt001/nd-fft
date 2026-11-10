************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     This routine is designed especially for the case
*     that both q and p equal to 2**n. But it works on the other case.
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k
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
c$$$               dst((k+j)*p+j)=src(j*q+(k+j))
               dst(k*p+(p+1)*j)=src((q+1)*j+k)
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
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
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
c$$$               dst(j*p+(k+j))=src((k+j)*q+j)
               dst((p+1)*j+k)=src(k*q+(q+1)*j)
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
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
            end do
         end do
      end if
      end 

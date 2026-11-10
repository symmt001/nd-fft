************************************************************************
*     decimate once, only for the first decimation
************************************************************************
      subroutine dec1fm(m,q,z)
      real*8 z(0:*)
      real*8 wr,wi
      integer k,m,q,qm
      qm=q*m
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do k=0, qm-1, 2
         wr=z(qm+k)
         wi=z(qm+k+1)
         z(qm+k  )=z(k  )-wr
         z(qm+k+1)=z(k+1)-wi
         z(   k  )=z(k  )+wr
         z(   k+1)=z(k+1)+wi
      end do
      end 

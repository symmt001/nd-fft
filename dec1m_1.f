************************************************************************
*     decimate once
************************************************************************
      subroutine dec1m(m,q,r,z,phase)
      real*8 z(0:*),phase(0:*)
      real*8 c,s,wr,wi
      integer j,k,m,q,r,qm
      integer s0,s1
      qm=q*m
*POPTION INDEP
      do j=0, r-1, 2
         c=phase(j)
         s=phase(j+1)
         s0=qm*j
         s1=qm+s0
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s1+k)*c-z(s1+k+1)*s
            wi=z(s1+k)*s+z(s1+k+1)*c
            z(s1+k  )=z(s0+k  )-wr
            z(s1+k+1)=z(s0+k+1)-wi
            z(s0+k  )=z(s0+k  )+wr
            z(s0+k+1)=z(s0+k+1)+wi
         end do
      end do
      end 

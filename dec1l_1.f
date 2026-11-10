************************************************************************
*     decimate once
************************************************************************
      subroutine dec1l(r,iflag,z,phase)
      real*8 z(0:*),phase(0:*)
      real*8 c,s,wr,wi,iflag
      integer j,r
      integer s0,s1
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do j=0, r-1, 2
         c=phase(j)
         s=phase(j+1)
         s0=2*j
         s1=2+s0

         wr=z(s1)*c-z(s1+1)*s
         wi=z(s1)*s+z(s1+1)*c
         z(s1  )=z(s0  )-wr
         z(s1+1)=z(s0+1)-wi
         z(s0  )=z(s0  )+wr
         z(s0+1)=z(s0+1)+wi
      end do
      end 

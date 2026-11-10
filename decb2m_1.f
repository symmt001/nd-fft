************************************************************************
*     decimate twice with bit-reversal
************************************************************************
      subroutine decb2m(m,q,r,z,wk,phase)
      implicit none
      real*8 z(0:*),wk(0:*),phase(0:*)
      real*8 c1,s1,c2,s2,c3,s3,wr,wi
      real*8 wr00,wr01,wr10,wr11,wi00,wi01,wi10,wi11
      integer j,k,m,q,r,qm
      integer s00,s01,s10,s11,d00,d01,d10,d11
      qm=q*m
*POPTION INDEP
      do j=0, r-1, q
         c1=phase(2*j)
         s1=phase(2*j+1)
         c2=phase(j)
         s2=phase(j+1)
         c3=phase(j+r)
         s3=phase(j+r+1)
         s00=m*j*4
         s01=qm+s00
         s10=qm+s01
         s11=qm+s10
c     bit-reverse order
         d00=m*j
         d10=r*m+d00
         d01=r*m+d10
         d11=r*m+d01
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s10+k)*c1-z(s10+k+1)*s1
            wi=z(s10+k)*s1+z(s10+k+1)*c1
            wr00=z(s00+k  )+wr
            wi00=z(s00+k+1)+wi
            wr10=z(s00+k  )-wr
            wi10=z(s00+k+1)-wi
            wr=z(s11+k)*c1-z(s11+k+1)*s1
            wi=z(s11+k)*s1+z(s11+k+1)*c1
            wr01=z(s01+k  )+wr
            wi01=z(s01+k+1)+wi
            wr11=z(s01+k  )-wr
            wi11=z(s01+k+1)-wi

            wr=wr01*c2-wi01*s2
            wi=wr01*s2+wi01*c2
            wk(d00+k  )=wr00+wr
            wk(d00+k+1)=wi00+wi
            wk(d01+k  )=wr00-wr
            wk(d01+k+1)=wi00-wi
            wr=wr11*c3-wi11*s3
            wi=wr11*s3+wi11*c3
            wk(d10+k  )=wr10+wr
            wk(d10+k+1)=wi10+wi
            wk(d11+k  )=wr10-wr
            wk(d11+k+1)=wi10-wi
         end do
      end do
      end

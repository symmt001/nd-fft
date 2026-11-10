************************************************************************
*     decimate twice
************************************************************************
      subroutine dec2m(m,q,r,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*),wr,wi
      real*8 c1,s1,c2,s2,c3,s3
      real*8 wr00,wr01,wr10,wr11,wi00,wi01,wi10,wi11
      integer j,k,m,q,r,qm
      integer s00,s01,s10,s11
      qm=q*m
*POPTION INDEP
      do j=0, r-1, 2
         c1=phase(j)
         s1=phase(j+1)
         c2=phase(2*j)
         s2=phase(2*j+1)
         c3=phase(2*j+2)
         s3=phase(2*j+3)
         s00=qm*j*2
         s01=qm+s00
         s10=qm+s01
         s11=qm+s10
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
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
            z(s00+k  )=wr00+wr
            z(s00+k+1)=wi00+wi
            z(s01+k  )=wr00-wr
            z(s01+k+1)=wi00-wi
            wr=wr11*c3-wi11*s3
            wi=wr11*s3+wi11*c3
            z(s10+k  )=wr10+wr
            z(s10+k+1)=wi10+wi
            z(s11+k  )=wr10-wr
            z(s11+k+1)=wi10-wi
         end do
      end do
      end

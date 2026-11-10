************************************************************************
*     decimate trice with bit-reversal
************************************************************************
      subroutine decb3m(m,q,r,z,wk,phase)
      implicit none
      real*8 z(0:*),wk(0:*),phase(0:*)
      real*8 c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7,wr,wi
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer j,k,m,q,r,qm
      integer s000,s001,s010,s011,d000,d001,d010,d011
      integer s100,s101,s110,s111,d100,d101,d110,d111
      qm=q*m
*POPTION INDEP
      do j=0, r-1, q
         c1=phase(4*j)
         s1=phase(4*j+1)
         c2=phase(2*j)
         s2=phase(2*j+1)
         c3=phase(2*j+2*r)
         s3=phase(2*j+2*r+1)
         c4=phase(j)
         s4=phase(j+1)
         c5=phase(j+r)
         s5=phase(j+r+1)
         c6=phase(j+2*r)
         s6=phase(j+2*r+1)
         c7=phase(j+3*r)
         s7=phase(j+3*r+1)

         s000=m*j*8
         s001=qm+s000
         s010=qm+s001
         s011=qm+s010
         s100=qm+s011
         s101=qm+s100
         s110=qm+s101
         s111=qm+s110
c     bit-reverse order
         d000=m*j
         d100=r*m+d000
         d010=r*m+d100
         d110=r*m+d010
         d001=r*m+d110
         d101=r*m+d001
         d011=r*m+d101
         d111=r*m+d011
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s100+k)*c1-z(s100+k+1)*s1
            wi=z(s100+k)*s1+z(s100+k+1)*c1
            wr000=z(s000+k)  +wr
            wi000=z(s000+k+1)+wi
            wr100=z(s000+k)  -wr
            wi100=z(s000+k+1)-wi
            wr=z(s110+k)*c1-z(s110+k+1)*s1
            wi=z(s110+k)*s1+z(s110+k+1)*c1
            wr010=z(s010+k)  +wr
            wi010=z(s010+k+1)+wi
            wr110=z(s010+k)  -wr
            wi110=z(s010+k+1)-wi
            wr=z(s101+k)*c1-z(s101+k+1)*s1
            wi=z(s101+k)*s1+z(s101+k+1)*c1
            wr001=z(s001+k)  +wr
            wi001=z(s001+k+1)+wi
            wr101=z(s001+k)  -wr
            wi101=z(s001+k+1)-wi
            wr=z(s111+k)*c1-z(s111+k+1)*s1
            wi=z(s111+k)*s1+z(s111+k+1)*c1
            wr011=z(s011+k)  +wr
            wi011=z(s011+k+1)+wi
            wr111=z(s011+k)  -wr
            wi111=z(s011+k+1)-wi

            wr=wr010*c2-wi010*s2
            wi=wr010*s2+wi010*c2
            wr010=wr000-wr
            wi010=wi000-wi
            wr000=wr000+wr
            wi000=wi000+wi
            wr=wr110*c3-wi110*s3
            wi=wr110*s3+wi110*c3
            wr110=wr100-wr
            wi110=wi100-wi
            wr100=wr100+wr
            wi100=wi100+wi
            wr=wr011*c2-wi011*s2
            wi=wr011*s2+wi011*c2
            wr011=wr001-wr
            wi011=wi001-wi
            wr001=wr001+wr
            wi001=wi001+wi
            wr=wr111*c3-wi111*s3
            wi=wr111*s3+wi111*c3
            wr111=wr101-wr
            wi111=wi101-wi
            wr101=wr101+wr
            wi101=wi101+wi

            wr=wr001*c4-wi001*s4
            wi=wr001*s4+wi001*c4
            wk(d000+k)  =wr000+wr
            wk(d000+k+1)=wi000+wi
            wk(d001+k)  =wr000-wr
            wk(d001+k+1)=wi000-wi
            wr=wr011*c6-wi011*s6
            wi=wr011*s6+wi011*c6
            wk(d010+k)  =wr010+wr
            wk(d010+k+1)=wi010+wi
            wk(d011+k)  =wr010-wr
            wk(d011+k+1)=wi010-wi
            wr=wr101*c5-wi101*s5
            wi=wr101*s5+wi101*c5
            wk(d100+k)  =wr100+wr
            wk(d100+k+1)=wi100+wi
            wk(d101+k)  =wr100-wr
            wk(d101+k+1)=wi100-wi
            wr=wr111*c7-wi111*s7
            wi=wr111*s7+wi111*c7
            wk(d110+k)  =wr110+wr
            wk(d110+k+1)=wi110+wi
            wk(d111+k)  =wr110-wr
            wk(d111+k+1)=wi110-wi
         end do
      end do
      end

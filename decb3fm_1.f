************************************************************************
*     decimate first 3 times with bit-reversal
************************************************************************
      subroutine decb3fm(m,q,iflag,z,wk,phase)
      implicit none
      real*8 z(0:*),wk(0:*),phase(0:*)
      real*8 c5,wr,wi,iflag
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer k,m,q,qm
      integer s000,s001,s010,s011
      integer s100,s101,s110,s111
c     r=q
      qm=q*m
c     c5=dsqrt(2.d0)/2
      c5=phase(q)
         s000=0
         s001=qm+s000
         s010=qm+s001
         s011=qm+s010
         s100=qm+s011
         s101=qm+s100
         s110=qm+s101
         s111=qm+s110
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr000=z(s000+k)  +z(s100+k)
            wi000=z(s000+k+1)+z(s100+k+1)
            wr100=z(s000+k)  -z(s100+k)
            wi100=z(s000+k+1)-z(s100+k+1)
            wr001=z(s001+k)  +z(s101+k)
            wi001=z(s001+k+1)+z(s101+k+1)
            wr101=z(s001+k)  -z(s101+k)
            wi101=z(s001+k+1)-z(s101+k+1)
            wr010=z(s010+k)  +z(s110+k)
            wi010=z(s010+k+1)+z(s110+k+1)
            wr110=z(s010+k)  -z(s110+k)
            wi110=z(s010+k+1)-z(s110+k+1)
            wr011=z(s011+k)  +z(s111+k)
            wi011=z(s011+k+1)+z(s111+k+1)
            wr111=z(s011+k)  -z(s111+k)
            wi111=z(s011+k+1)-z(s111+k+1)

            wr=wr010
            wi=wi010
            wr010=wr000-wr
            wi010=wi000-wi
            wr000=wr000+wr
            wi000=wi000+wi
            wr=wr011
            wi=wi011
            wr011=wr001-wr
            wi011=wi001-wi
            wr001=wr001+wr
            wi001=wi001+wi
            wi= wr110*iflag
            wr=-wi110*iflag
            wr110=wr100-wr
            wi110=wi100-wi
            wr100=wr100+wr
            wi100=wi100+wi
            wi= wr111*iflag
            wr=-wi111*iflag
            wr111=wr101-wr
            wi111=wi101-wi
            wr101=wr101+wr
            wi101=wi101+wi

            wr=wr001
            wi=wi001
            wk(s000+k)  =wr000+wr
            wk(s000+k+1)=wi000+wi
            wk(s100+k)  =wr000-wr
            wk(s100+k+1)=wi000-wi
            wr=-wi011*iflag
            wi= wr011*iflag
            wk(s010+k)  =wr010+wr
            wk(s010+k+1)=wi010+wi
            wk(s110+k)  =wr010-wr
            wk(s110+k+1)=wi010-wi
            wr= (wr101-wi101*iflag)*c5
            wi= (wr101*iflag+wi101)*c5
            wk(s001+k)  =wr100+wr
            wk(s001+k+1)=wi100+wi
            wk(s101+k)  =wr100-wr
            wk(s101+k+1)=wi100-wi
            wi= (wr111*iflag-wi111)*c5
            wr=-(wr111+wi111*iflag)*c5
            wk(s011+k)  =wr110+wr
            wk(s011+k+1)=wi110+wi
            wk(s111+k)  =wr110-wr
            wk(s111+k+1)=wi110-wi
         end do
      end

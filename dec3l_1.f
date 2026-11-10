************************************************************************
*     decimate first trice
************************************************************************
      subroutine dec3l(r,iflag,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*),iflag
      real*8 c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7,wr,wi
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer j,r
      integer s000,s001,s010,s011
      integer s100,s101,s110,s111
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do j=0, r-1, 2
         c1=phase(j)
         s1=phase(j+1)
         c2=phase(2*j)
         s2=phase(2*j+1)
         c3=phase(2*j+2)
         s3=phase(2*j+3)
         c4=phase(4*j)
         s4=phase(4*j+1)
         c5=phase(4*j+2)
         s5=phase(4*j+3)
         c6=phase(4*j+4)
         s6=phase(4*j+5)
         c7=phase(4*j+6)
         s7=phase(4*j+7)

         s000=2*j*4
         s001=2+s000
         s010=2+s001
         s011=2+s010
         s100=2+s011
         s101=2+s100
         s110=2+s101
         s111=2+s110
         wr=z(s100)*c1-z(s100+1)*s1
         wi=z(s100)*s1+z(s100+1)*c1
         wr000=z(s000)  +wr
         wi000=z(s000+1)+wi
         wr100=z(s000)  -wr
         wi100=z(s000+1)-wi
         wr=z(s101)*c1-z(s101+1)*s1
         wi=z(s101)*s1+z(s101+1)*c1
         wr001=z(s001)  +wr
         wi001=z(s001+1)+wi
         wr101=z(s001)  -wr
         wi101=z(s001+1)-wi
         wr=z(s110)*c1-z(s110+1)*s1
         wi=z(s110)*s1+z(s110+1)*c1
         wr010=z(s010)  +wr
         wi010=z(s010+1)+wi
         wr110=z(s010)  -wr
         wi110=z(s010+1)-wi
         wr=z(s111)*c1-z(s111+1)*s1
         wi=z(s111)*s1+z(s111+1)*c1
         wr011=z(s011)  +wr
         wi011=z(s011+1)+wi
         wr111=z(s011)  -wr
         wi111=z(s011+1)-wi

         wr=wr010*c2-wi010*s2
         wi=wr010*s2+wi010*c2
         wr010=wr000-wr
         wi010=wi000-wi
         wr000=wr000+wr
         wi000=wi000+wi
         wr=wr011*c2-wi011*s2
         wi=wr011*s2+wi011*c2
         wr011=wr001-wr
         wi011=wi001-wi
         wr001=wr001+wr
         wi001=wi001+wi
         wr=wr110*c3-wi110*s3
         wi=wr110*s3+wi110*c3
         wr110=wr100-wr
         wi110=wi100-wi
         wr100=wr100+wr
         wi100=wi100+wi
         wr=wr111*c3-wi111*s3
         wi=wr111*s3+wi111*c3
         wr111=wr101-wr
         wi111=wi101-wi
         wr101=wr101+wr
         wi101=wi101+wi

         wr=wr001*c4-wi001*s4
         wi=wr001*s4+wi001*c4
         z(s000)  =wr000+wr
         z(s000+1)=wi000+wi
         z(s001)  =wr000-wr
         z(s001+1)=wi000-wi
         wr=wr011*c5-wi011*s5
         wi=wr011*s5+wi011*c5
         z(s010)  =wr010+wr
         z(s010+1)=wi010+wi
         z(s011)  =wr010-wr
         z(s011+1)=wi010-wi
         wr=wr101*c6-wi101*s6
         wi=wr101*s6+wi101*c6
         z(s100)  =wr100+wr
         z(s100+1)=wi100+wi
         z(s101)  =wr100-wr
         z(s101+1)=wi100-wi
         wr=wr111*c7-wi111*s7
         wi=wr111*s7+wi111*c7
         z(s110)  =wr110+wr
         z(s110+1)=wi110+wi
         z(s111)  =wr110-wr
         z(s111+1)=wi110-wi
      end do
      end

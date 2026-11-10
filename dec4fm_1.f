************************************************************************
*     decimate first 4 times -> It turns out to be useless.(HP-9000J282)
************************************************************************
      subroutine dec4fm(m,q,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*),wr,wi
      real*8 c6,cc,sc,iflag
      real*8 wr0000,wr0001,wr0010,wr0011,wi0000,wi0001,wi0010,wi0011
      real*8 wr0100,wr0101,wr0110,wr0111,wi0100,wi0101,wi0110,wi0111
      real*8 wr1000,wr1001,wr1010,wr1011,wi1000,wi1001,wi1010,wi1011
      real*8 wr1100,wr1101,wr1110,wr1111,wi1100,wi1101,wi1110,wi1111
      integer k,m,q,qm
      integer s0000,s0001,s0010,s0011
      integer s0100,s0101,s0110,s0111
      integer s1000,s1001,s1010,s1011
      integer s1100,s1101,s1110,s1111
      qm=q*m
      iflag=phase(3)
      c6   =phase(4)
      cc   =phase(8)
      sc   =phase(9)
c$$$      cd=-sc*iflag
c$$$      sd= cc*iflag
c$$$      ce= sc*iflag
c$$$      se= cc*iflag
c$$$      cf=-cc 
c$$$      sf= sc

      s0000=0
      s0001=qm
      s0010=qm+s0001
      s0011=qm+s0010
      s0100=qm+s0011
      s0101=qm+s0100
      s0110=qm+s0101
      s0111=qm+s0110
      s1000=qm+s0111
      s1001=qm+s1000
      s1010=qm+s1001
      s1011=qm+s1010
      s1100=qm+s1011
      s1101=qm+s1100
      s1110=qm+s1101
      s1111=qm+s1110
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
*VOPTION INDEP
*POPTION INDEP
      do k=0, qm-1, 2
         wr=z(s1000+k)
         wi=z(s1000+k+1)
         wr0000=z(s0000+k)  +wr
         wi0000=z(s0000+k+1)+wi
         wr1000=z(s0000+k)  -wr
         wi1000=z(s0000+k+1)-wi
         wr=z(s1001+k)
         wi=z(s1001+k+1)
         wr0001=z(s0001+k)  +wr
         wi0001=z(s0001+k+1)+wi
         wr1001=z(s0001+k)  -wr
         wi1001=z(s0001+k+1)-wi
         wr=z(s1010+k)
         wi=z(s1010+k+1)
         wr0010=z(s0010+k)  +wr
         wi0010=z(s0010+k+1)+wi
         wr1010=z(s0010+k)  -wr
         wi1010=z(s0010+k+1)-wi
         wr=z(s1011+k)
         wi=z(s1011+k+1)
         wr0011=z(s0011+k)  +wr
         wi0011=z(s0011+k+1)+wi
         wr1011=z(s0011+k)  -wr
         wi1011=z(s0011+k+1)-wi
         wr=z(s1100+k)
         wi=z(s1100+k+1)
         wr0100=z(s0100+k)  +wr
         wi0100=z(s0100+k+1)+wi
         wr1100=z(s0100+k)  -wr
         wi1100=z(s0100+k+1)-wi
         wr=z(s1101+k)
         wi=z(s1101+k+1)
         wr0101=z(s0101+k)  +wr
         wi0101=z(s0101+k+1)+wi
         wr1101=z(s0101+k)  -wr
         wi1101=z(s0101+k+1)-wi
         wr=z(s1110+k)
         wi=z(s1110+k+1)
         wr0110=z(s0110+k)  +wr
         wi0110=z(s0110+k+1)+wi
         wr1110=z(s0110+k)  -wr
         wi1110=z(s0110+k+1)-wi
         wr=z(s1111+k)
         wi=z(s1111+k+1)
         wr0111=z(s0111+k)  +wr
         wi0111=z(s0111+k+1)+wi
         wr1111=z(s0111+k)  -wr
         wi1111=z(s0111+k+1)-wi

         wr= wr0100
         wi= wi0100
         wr0100=wr0000-wr
         wi0100=wi0000-wi
         wr0000=wr0000+wr
         wi0000=wi0000+wi
         wr= wr0101
         wi= wi0101
         wr0101=wr0001-wr
         wi0101=wi0001-wi
         wr0001=wr0001+wr
         wi0001=wi0001+wi
         wr= wr0110
         wi= wi0110
         wr0110=wr0010-wr
         wi0110=wi0010-wi
         wr0010=wr0010+wr
         wi0010=wi0010+wi
         wr= wr0111
         wi= wi0111
         wr0111=wr0011-wr
         wi0111=wi0011-wi
         wr0011=wr0011+wr
         wi0011=wi0011+wi
         wi= wr1100*iflag
         wr=-wi1100*iflag
         wr1100=wr1000-wr
         wi1100=wi1000-wi
         wr1000=wr1000+wr
         wi1000=wi1000+wi
         wi= wr1101*iflag
         wr=-wi1101*iflag
         wr1101=wr1001-wr
         wi1101=wi1001-wi
         wr1001=wr1001+wr
         wi1001=wi1001+wi
         wi= wr1110*iflag
         wr=-wi1110*iflag
         wr1110=wr1010-wr
         wi1110=wi1010-wi
         wr1010=wr1010+wr
         wi1010=wi1010+wi
         wi= wr1111*iflag
         wr=-wi1111*iflag
         wr1111=wr1011-wr
         wi1111=wi1011-wi
         wr1011=wr1011+wr
         wi1011=wi1011+wi

         wr= wr0010
         wi= wi0010
         wr0010=wr0000-wr
         wi0010=wi0000-wi
         wr0000=wr0000+wr
         wi0000=wi0000+wi
         wr= wr0011
         wi= wi0011
         wr0011=wr0001-wr
         wi0011=wi0001-wi
         wr0001=wr0001+wr
         wi0001=wi0001+wi
         wi= wr0110*iflag
         wr=-wi0110*iflag
         wr0110=wr0100-wr
         wi0110=wi0100-wi
         wr0100=wr0100+wr
         wi0100=wi0100+wi
         wi= wr0111*iflag
         wr=-wi0111*iflag
         wr0111=wr0101-wr
         wi0111=wi0101-wi
         wr0101=wr0101+wr
         wi0101=wi0101+wi
         wr= (wr1010-wi1010*iflag)*c6
         wi= (wr1010*iflag+wi1010)*c6
         wr1010=wr1000-wr
         wi1010=wi1000-wi
         wr1000=wr1000+wr
         wi1000=wi1000+wi
         wr= (wr1011-wi1011*iflag)*c6
         wi= (wr1011*iflag+wi1011)*c6
         wr1011=wr1001-wr
         wi1011=wi1001-wi
         wr1001=wr1001+wr
         wi1001=wi1001+wi
         wi= (wr1110*iflag-wi1110)*c6
         wr=-(wr1110+wi1110*iflag)*c6
         wr1110=wr1100-wr
         wi1110=wi1100-wi
         wr1100=wr1100+wr
         wi1100=wi1100+wi
         wi= (wr1111*iflag-wi1111)*c6
         wr=-(wr1111+wi1111*iflag)*c6
         wr1111=wr1101-wr
         wi1111=wi1101-wi
         wr1101=wr1101+wr
         wi1101=wi1101+wi

         wr= wr0001
         wi= wi0001
         z(s0000+k)  =wr0000+wr
         z(s0000+k+1)=wi0000+wi
         z(s0001+k)  =wr0000-wr
         z(s0001+k+1)=wi0000-wi
         wi= wr0011*iflag
         wr=-wi0011*iflag
         z(s0010+k)  =wr0010+wr
         z(s0010+k+1)=wi0010+wi
         z(s0011+k)  =wr0010-wr
         z(s0011+k+1)=wi0010-wi
         wr= (wr0101-wi0101*iflag)*c6
         wi= (wr0101*iflag+wi0101)*c6
         z(s0100+k)  =wr0100+wr
         z(s0100+k+1)=wi0100+wi
         z(s0101+k)  =wr0100-wr
         z(s0101+k+1)=wi0100-wi
         wi= (wr0111*iflag-wi0111)*c6
         wr=-(wr0111+wi0111*iflag)*c6
         z(s0110+k)  =wr0110+wr
         z(s0110+k+1)=wi0110+wi
         z(s0111+k)  =wr0110-wr
         z(s0111+k+1)=wi0110-wi
         wr=wr1001*cc-wi1001*sc
         wi=wr1001*sc+wi1001*cc
         z(s1000+k)  =wr1000+wr
         z(s1000+k+1)=wi1000+wi
         z(s1001+k)  =wr1000-wr
         z(s1001+k+1)=wi1000-wi
         wr=-(wr1011*sc+wi1011*cc)*iflag
         wi= (wr1011*cc-wi1011*sc)*iflag
         z(s1010+k)  =wr1010+wr
         z(s1010+k+1)=wi1010+wi
         z(s1011+k)  =wr1010-wr
         z(s1011+k+1)=wi1010-wi
         wr=(wr1101*sc-wi1101*cc)*iflag
         wi=(wr1101*cc+wi1101*sc)*iflag
         z(s1100+k)  =wr1100+wr
         z(s1100+k+1)=wi1100+wi
         z(s1101+k)  =wr1100-wr
         z(s1101+k+1)=wi1100-wi
         wr=-(wr1111*cc+wi1111*sc)
         wi=  wr1111*sc-wi1111*cc
         z(s1110+k)  =wr1110+wr
         z(s1110+k+1)=wi1110+wi
         z(s1111+k)  =wr1110-wr
         z(s1111+k+1)=wi1110-wi
      end do
      end

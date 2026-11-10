************************************************************************
*     decimate 4 times with bit-reversal
************************************************************************
      subroutine decb4m(m,q,r,z,wk,phase)
      implicit none
      real*8 z(0:*),wk(0:*),phase(0:*)
      real*8 c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7,wr,wi
      real*8 c8,s8,c9,s9,ca,sa,cb,sb,cc,sc,cd,sd,ce,se,cf,sf
      real*8 wr0000,wr0001,wr0010,wr0011,wi0000,wi0001,wi0010,wi0011
      real*8 wr0100,wr0101,wr0110,wr0111,wi0100,wi0101,wi0110,wi0111
      real*8 wr1000,wr1001,wr1010,wr1011,wi1000,wi1001,wi1010,wi1011
      real*8 wr1100,wr1101,wr1110,wr1111,wi1100,wi1101,wi1110,wi1111
      integer j,k,m,q,r,qm
      integer s0000,s0001,s0010,s0011,d0000,d0001,d0010,d0011
      integer s0100,s0101,s0110,s0111,d0100,d0101,d0110,d0111
      integer s1000,s1001,s1010,s1011,d1000,d1001,d1010,d1011
      integer s1100,s1101,s1110,s1111,d1100,d1101,d1110,d1111
      qm=q*m
*POPTION INDEP
      do j=0, r-1, q
         c1=phase(8*j)
         s1=phase(8*j+1)
         c2=phase(4*j)
         s2=phase(4*j+1)
         c3=phase(4*j+4*r)
         s3=phase(4*j+4*r+1)
         c4=phase(2*j)
         s4=phase(2*j+1)
         c5=phase(2*j+2*r)
         s5=phase(2*j+2*r+1)
         c6=phase(2*j+4*r)
         s6=phase(2*j+4*r+1)
         c7=phase(2*j+6*r)
         s7=phase(2*j+6*r+1)
         c8=phase(j)
         s8=phase(j+1)
         c9=phase(j+r)
         s9=phase(j+r+1)
         ca=phase(j+2*r)
         sa=phase(j+2*r+1)
         cb=phase(j+3*r)
         sb=phase(j+3*r+1)
         cc=phase(j+4*r)
         sc=phase(j+4*r+1)
         cd=phase(j+5*r)
         sd=phase(j+5*r+1)
         ce=phase(j+6*r)
         se=phase(j+6*r+1)
         cf=phase(j+7*r)
         sf=phase(j+7*r+1)

         s0000=m*j*16
         s0001=qm+s0000
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
c     bit-reverse order
         d0000=m*j
         d1000=r*m+d0000
         d0100=r*m+d1000
         d1100=r*m+d0100
         d0010=r*m+d1100
         d1010=r*m+d0010
         d0110=r*m+d1010
         d1110=r*m+d0110
         d0001=r*m+d1110
         d1001=r*m+d0001
         d0101=r*m+d1001
         d1101=r*m+d0101
         d0011=r*m+d1101
         d1011=r*m+d0011
         d0111=r*m+d1011
         d1111=r*m+d0111
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s1000+k)*c1-z(s1000+k+1)*s1
            wi=z(s1000+k)*s1+z(s1000+k+1)*c1
            wr0000=z(s0000+k)  +wr
            wi0000=z(s0000+k+1)+wi
            wr1000=z(s0000+k)  -wr
            wi1000=z(s0000+k+1)-wi
            wr=z(s1100+k)*c1-z(s1100+k+1)*s1
            wi=z(s1100+k)*s1+z(s1100+k+1)*c1
            wr0100=z(s0100+k)  +wr
            wi0100=z(s0100+k+1)+wi
            wr1100=z(s0100+k)  -wr
            wi1100=z(s0100+k+1)-wi
            wr=z(s1010+k)*c1-z(s1010+k+1)*s1
            wi=z(s1010+k)*s1+z(s1010+k+1)*c1
            wr0010=z(s0010+k)  +wr
            wi0010=z(s0010+k+1)+wi
            wr1010=z(s0010+k)  -wr
            wi1010=z(s0010+k+1)-wi
            wr=z(s1110+k)*c1-z(s1110+k+1)*s1
            wi=z(s1110+k)*s1+z(s1110+k+1)*c1
            wr0110=z(s0110+k)  +wr
            wi0110=z(s0110+k+1)+wi
            wr1110=z(s0110+k)  -wr
            wi1110=z(s0110+k+1)-wi
            wr=z(s1001+k)*c1-z(s1001+k+1)*s1
            wi=z(s1001+k)*s1+z(s1001+k+1)*c1
            wr0001=z(s0001+k)  +wr
            wi0001=z(s0001+k+1)+wi
            wr1001=z(s0001+k)  -wr
            wi1001=z(s0001+k+1)-wi
            wr=z(s1101+k)*c1-z(s1101+k+1)*s1
            wi=z(s1101+k)*s1+z(s1101+k+1)*c1
            wr0101=z(s0101+k)  +wr
            wi0101=z(s0101+k+1)+wi
            wr1101=z(s0101+k)  -wr
            wi1101=z(s0101+k+1)-wi
            wr=z(s1011+k)*c1-z(s1011+k+1)*s1
            wi=z(s1011+k)*s1+z(s1011+k+1)*c1
            wr0011=z(s0011+k)  +wr
            wi0011=z(s0011+k+1)+wi
            wr1011=z(s0011+k)  -wr
            wi1011=z(s0011+k+1)-wi
            wr=z(s1111+k)*c1-z(s1111+k+1)*s1
            wi=z(s1111+k)*s1+z(s1111+k+1)*c1
            wr0111=z(s0111+k)  +wr
            wi0111=z(s0111+k+1)+wi
            wr1111=z(s0111+k)  -wr
            wi1111=z(s0111+k+1)-wi

            wr=wr0100*c2-wi0100*s2
            wi=wr0100*s2+wi0100*c2
            wr0100=wr0000-wr
            wi0100=wi0000-wi
            wr0000=wr0000+wr
            wi0000=wi0000+wi
            wr=wr1100*c3-wi1100*s3
            wi=wr1100*s3+wi1100*c3
            wr1100=wr1000-wr
            wi1100=wi1000-wi
            wr1000=wr1000+wr
            wi1000=wi1000+wi
            wr=wr0110*c2-wi0110*s2
            wi=wr0110*s2+wi0110*c2
            wr0110=wr0010-wr
            wi0110=wi0010-wi
            wr0010=wr0010+wr
            wi0010=wi0010+wi
            wr=wr1110*c3-wi1110*s3
            wi=wr1110*s3+wi1110*c3
            wr1110=wr1010-wr
            wi1110=wi1010-wi
            wr1010=wr1010+wr
            wi1010=wi1010+wi
            wr=wr0101*c2-wi0101*s2
            wi=wr0101*s2+wi0101*c2
            wr0101=wr0001-wr
            wi0101=wi0001-wi
            wr0001=wr0001+wr
            wi0001=wi0001+wi
            wr=wr1101*c3-wi1101*s3
            wi=wr1101*s3+wi1101*c3
            wr1101=wr1001-wr
            wi1101=wi1001-wi
            wr1001=wr1001+wr
            wi1001=wi1001+wi
            wr=wr0111*c2-wi0111*s2
            wi=wr0111*s2+wi0111*c2
            wr0111=wr0011-wr
            wi0111=wi0011-wi
            wr0011=wr0011+wr
            wi0011=wi0011+wi
            wr=wr1111*c3-wi1111*s3
            wi=wr1111*s3+wi1111*c3
            wr1111=wr1011-wr
            wi1111=wi1011-wi
            wr1011=wr1011+wr
            wi1011=wi1011+wi

            wr=wr0010*c4-wi0010*s4
            wi=wr0010*s4+wi0010*c4
            wr0010=wr0000-wr
            wi0010=wi0000-wi
            wr0000=wr0000+wr
            wi0000=wi0000+wi
            wr=wr1010*c5-wi1010*s5
            wi=wr1010*s5+wi1010*c5
            wr1010=wr1000-wr
            wi1010=wi1000-wi
            wr1000=wr1000+wr
            wi1000=wi1000+wi
            wr=wr0110*c6-wi0110*s6
            wi=wr0110*s6+wi0110*c6
            wr0110=wr0100-wr
            wi0110=wi0100-wi
            wr0100=wr0100+wr
            wi0100=wi0100+wi
            wr=wr1110*c7-wi1110*s7
            wi=wr1110*s7+wi1110*c7
            wr1110=wr1100-wr
            wi1110=wi1100-wi
            wr1100=wr1100+wr
            wi1100=wi1100+wi
            wr=wr0011*c4-wi0011*s4
            wi=wr0011*s4+wi0011*c4
            wr0011=wr0001-wr
            wi0011=wi0001-wi
            wr0001=wr0001+wr
            wi0001=wi0001+wi
            wr=wr1011*c5-wi1011*s5
            wi=wr1011*s5+wi1011*c5
            wr1011=wr1001-wr
            wi1011=wi1001-wi
            wr1001=wr1001+wr
            wi1001=wi1001+wi
            wr=wr0111*c6-wi0111*s6
            wi=wr0111*s6+wi0111*c6
            wr0111=wr0101-wr
            wi0111=wi0101-wi
            wr0101=wr0101+wr
            wi0101=wi0101+wi
            wr=wr1111*c7-wi1111*s7
            wi=wr1111*s7+wi1111*c7
            wr1111=wr1101-wr
            wi1111=wi1101-wi
            wr1101=wr1101+wr
            wi1101=wi1101+wi

            wr=wr0001*c8-wi0001*s8
            wi=wr0001*s8+wi0001*c8
            wk(d0000+k)  =wr0000+wr
            wk(d0000+k+1)=wi0000+wi
            wk(d0001+k)  =wr0000-wr
            wk(d0001+k+1)=wi0000-wi
            wr=wr0011*cc-wi0011*sc
            wi=wr0011*sc+wi0011*cc
            wk(d0010+k)  =wr0010+wr
            wk(d0010+k+1)=wi0010+wi
            wk(d0011+k)  =wr0010-wr
            wk(d0011+k+1)=wi0010-wi
            wr=wr0101*ca-wi0101*sa
            wi=wr0101*sa+wi0101*ca
            wk(d0100+k)  =wr0100+wr
            wk(d0100+k+1)=wi0100+wi
            wk(d0101+k)  =wr0100-wr
            wk(d0101+k+1)=wi0100-wi
            wr=wr0111*ce-wi0111*se
            wi=wr0111*se+wi0111*ce
            wk(d0110+k)  =wr0110+wr
            wk(d0110+k+1)=wi0110+wi
            wk(d0111+k)  =wr0110-wr
            wk(d0111+k+1)=wi0110-wi
            wr=wr1001*c9-wi1001*s9
            wi=wr1001*s9+wi1001*c9
            wk(d1000+k)  =wr1000+wr
            wk(d1000+k+1)=wi1000+wi
            wk(d1001+k)  =wr1000-wr
            wk(d1001+k+1)=wi1000-wi
            wr=wr1011*cd-wi1011*sd
            wi=wr1011*sd+wi1011*cd
            wk(d1010+k)  =wr1010+wr
            wk(d1010+k+1)=wi1010+wi
            wk(d1011+k)  =wr1010-wr
            wk(d1011+k+1)=wi1010-wi
            wr=wr1101*cb-wi1101*sb
            wi=wr1101*sb+wi1101*cb
            wk(d1100+k)  =wr1100+wr
            wk(d1100+k+1)=wi1100+wi
            wk(d1101+k)  =wr1100-wr
            wk(d1101+k+1)=wi1100-wi
            wr=wr1111*cf-wi1111*sf
            wi=wr1111*sf+wi1111*cf
            wk(d1110+k)  =wr1110+wr
            wk(d1110+k+1)=wi1110+wi
            wk(d1111+k)  =wr1110-wr
            wk(d1111+k+1)=wi1110-wi
         end do
      end do
      end

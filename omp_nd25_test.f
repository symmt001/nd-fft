************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2, fftprep3 & fftprep4
*     cwk:    Work Area (size N must satisfies N >= NMAX*nunroll)
*     bitrev: Bit Reversal Table Prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+...+n(nd))
*     Written by S. Yamamoto
*     
*     Comments:
*     *odir       directive to the optimizer on SX-3   (NEC)
*     *vdir       directive to the compiler  on SX-3   (NEC)
*     *VOPTION    directive to the compiler  on S-3800 (HITACHI)
*     *VOCL       directive to the compiler  on VPP    (FUJITSU)
************************************************************************
      subroutine fftnd(nd,nmax,n,z,cwk,phase,bitrev)
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      integer i,j,k,l,m,q,r,s,t,nunroll
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      integer WSIZE
c     128kbytes, (size of complex*16)=16
      parameter(WSIZE=128*1024/16)
      s=0
      do i=1, nd
         s=s+n(i)
      end do
      s=2**s
      r=1
c     First Axis
      i=1
      q=2**n(i)
      do k=0, s-1, q
         do l=0, q-1
            cwk(l)=z(k+l)
         end do
         call mfftnb(n(i),1,cwk,phase(0,i))
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do l=0, q-1
            z(k+l)=cwk(bitrev(l,i))
         end do
      end do
      r=q
c     Other
      do i=2, nd
         t=2**n(i)
         q=r*t
         nunroll=max(min(r,WSIZE/t),1)
         if(r.gt.nunroll)then
            do k=0, s-1, q
c$omp parallel do
               do j=k, k+r-1, nunroll
                  do m=0, t-1
                     do l=0, nunroll-1
                        cwk((j-k)*t+m*nunroll+l)=z(j+r*m+l)
                     end do
                  end do
                  call mfftnb(n(i),nunroll,cwk((j-k)*t),phase(0,i))
                  do m=0, t-1
                     do l=0, nunroll-1
                        z(j+r*m+l)=cwk((j-k)*t+bitrev(m,i)*nunroll+l)
                     end do
                  end do
               end do
            end do
         else
c$omp parallel do
            do k=0, s-1, q
               do l=0, q-1
                  cwk(k+l)=z(k+l)
               end do
               call mfftnb(n(i),r,cwk(k),phase(0,i))
               do m=0, t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
                  do l=0, r-1
                     z(k+r*m+l)=cwk(k+bitrev(m,i)*r+l)
                  end do
               end do
            end do
         end if
         r=q
      end do
      end
************************************************************************
*     multiple fft routine without bit-reversal
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors (bit-reversal order)
************************************************************************
      subroutine mfftnb(n,m,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*)
      integer n,m,q,r
      q=2**(n+1)
      r=2
c     first decimation
c  It turns out to be useless. (dec4f/HP-9000J282)
c$$$      if(q.ge.32)then
c$$$         q=q/16
c$$$         call dec4fm(m,q,z,phase)
c$$$         r=r*16
c$$$      else if(q.ge.16)then
      if(q.ge.16)then
         q=q/8
         call dec3fm(m,q,z,phase)
         r=r*8
c$$$      else if(q.ge.8)then
c$$$         q=q/4
c$$$         call dec2fm(m,q,z,phase)
c$$$         r=r*4
      end if
c     do while(q.gt.2)
 10   continue
c  It turns out to be useless. (dec5/HP-9000J282)
c$$$      if(q.ge.64)then
c$$$         q=q/32
c$$$         call dec5m(m,q,r,z,phase)
c$$$         r=r*32
      if(q.ge.32)then
         q=q/16
         call dec4m(m,q,r,z,phase)
         r=r*16
      else if(q.ge.16)then
         q=q/8
         call dec3m(m,q,r,z,phase)
         r=r*8
      else if(q.ge.8)then
         q=q/4
         call dec2m(m,q,r,z,phase)
         r=r*4
      else if(q.ge.4)then
         q=q/2
         call dec1m(m,q,r,z,phase)
         r=r*2
      end if
      if (q.gt.2) goto 10
c      end do
      end
************************************************************************
*     decimate first trice
************************************************************************
      subroutine dec3m(m,q,r,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*)
      real*8 c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7,wr,wi
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer j,k,m,q,r,qm
      integer s000,s001,s010,s011
      integer s100,s101,s110,s111
      qm=q*m
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

         s000=qm*j*4
         s001=qm+s000
         s010=qm+s001
         s011=qm+s010
         s100=qm+s011
         s101=qm+s100
         s110=qm+s101
         s111=qm+s110
*VOCL LOOP,NOVREC(Z)
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
            wr=z(s101+k)*c1-z(s101+k+1)*s1
            wi=z(s101+k)*s1+z(s101+k+1)*c1
            wr001=z(s001+k)  +wr
            wi001=z(s001+k+1)+wi
            wr101=z(s001+k)  -wr
            wi101=z(s001+k+1)-wi
            wr=z(s110+k)*c1-z(s110+k+1)*s1
            wi=z(s110+k)*s1+z(s110+k+1)*c1
            wr010=z(s010+k)  +wr
            wi010=z(s010+k+1)+wi
            wr110=z(s010+k)  -wr
            wi110=z(s010+k+1)-wi
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
            z(s000+k)  =wr000+wr
            z(s000+k+1)=wi000+wi
            z(s001+k)  =wr000-wr
            z(s001+k+1)=wi000-wi
            wr=wr011*c5-wi011*s5
            wi=wr011*s5+wi011*c5
            z(s010+k)  =wr010+wr
            z(s010+k+1)=wi010+wi
            z(s011+k)  =wr010-wr
            z(s011+k+1)=wi010-wi
            wr=wr101*c6-wi101*s6
            wi=wr101*s6+wi101*c6
            z(s100+k)  =wr100+wr
            z(s100+k+1)=wi100+wi
            z(s101+k)  =wr100-wr
            z(s101+k+1)=wi100-wi
            wr=wr111*c7-wi111*s7
            wi=wr111*s7+wi111*c7
            z(s110+k)  =wr110+wr
            z(s110+k+1)=wi110+wi
            z(s111+k)  =wr110-wr
            z(s111+k+1)=wi110-wi
         end do
      end do
      end
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
************************************************************************
*     decimate first trice
************************************************************************
      subroutine dec3fm(m,q,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*)
      real*8 c6,wr,wi,iflag
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer k,m,q,qm
      integer s000,s001,s010,s011
      integer s100,s101,s110,s111
      qm=q*m
      iflag=phase(3)
      c6   =phase(4)
      s000=0
      s001=qm
      s010=qm+s001
      s011=qm+s010
      s100=qm+s011
      s101=qm+s100
      s110=qm+s101
      s111=qm+s110
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do k=0, qm-1, 2
         wr=z(s100+k)
         wi=z(s100+k+1)
         wr000=z(s000+k)  +wr
         wi000=z(s000+k+1)+wi
         wr100=z(s000+k)  -wr
         wi100=z(s000+k+1)-wi
         wr=z(s101+k)
         wi=z(s101+k+1)
         wr001=z(s001+k)  +wr
         wi001=z(s001+k+1)+wi
         wr101=z(s001+k)  -wr
         wi101=z(s001+k+1)-wi
         wr=z(s110+k)
         wi=z(s110+k+1)
         wr010=z(s010+k)  +wr
         wi010=z(s010+k+1)+wi
         wr110=z(s010+k)  -wr
         wi110=z(s010+k+1)-wi
         wr=z(s111+k)
         wi=z(s111+k+1)
         wr011=z(s011+k)  +wr
         wi011=z(s011+k+1)+wi
         wr111=z(s011+k)  -wr
         wi111=z(s011+k+1)-wi
         
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
         z(s000+k)  =wr000+wr
         z(s000+k+1)=wi000+wi
         z(s001+k)  =wr000-wr
         z(s001+k+1)=wi000-wi
         wi= wr011*iflag
         wr=-wi011*iflag
         z(s010+k)  =wr010+wr
         z(s010+k+1)=wi010+wi
         z(s011+k)  =wr010-wr
         z(s011+k+1)=wi010-wi
         wr= (wr101-wi101*iflag)*c6
         wi= (wr101*iflag+wi101)*c6
         z(s100+k)  =wr100+wr
         z(s100+k+1)=wi100+wi
         z(s101+k)  =wr100-wr
         z(s101+k+1)=wi100-wi
         wi= (wr111*iflag-wi111)*c6
         wr=-(wr111+wi111*iflag)*c6
         z(s110+k)  =wr110+wr
         z(s110+k+1)=wi110+wi
         z(s111+k)  =wr110-wr
         z(s111+k+1)=wi110-wi
      end do
      end
************************************************************************
*     decimate 4 times
************************************************************************
      subroutine dec4m(m,q,r,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*),wr,wi
      real*8 c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7
      real*8 c8,s8,c9,s9,ca,sa,cb,sb,cc,sc,cd,sd,ce,se,cf,sf
      real*8 wr0000,wr0001,wr0010,wr0011,wi0000,wi0001,wi0010,wi0011
      real*8 wr0100,wr0101,wr0110,wr0111,wi0100,wi0101,wi0110,wi0111
      real*8 wr1000,wr1001,wr1010,wr1011,wi1000,wi1001,wi1010,wi1011
      real*8 wr1100,wr1101,wr1110,wr1111,wi1100,wi1101,wi1110,wi1111
      integer j,k,m,q,r,qm
      integer s0000,s0001,s0010,s0011
      integer s0100,s0101,s0110,s0111
      integer s1000,s1001,s1010,s1011
      integer s1100,s1101,s1110,s1111
      qm=q*m
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
         c8=phase(8*j)
         s8=phase(8*j+1)
         c9=phase(8*j+2)
         s9=phase(8*j+3)
         ca=phase(8*j+4)
         sa=phase(8*j+5)
         cb=phase(8*j+6)
         sb=phase(8*j+7)
         cc=phase(8*j+8)
         sc=phase(8*j+9)
         cd=phase(8*j+10)
         sd=phase(8*j+11)
         ce=phase(8*j+12)
         se=phase(8*j+13)
         cf=phase(8*j+14)
         sf=phase(8*j+15)

         s0000=qm*j*8
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
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s1000+k)*c1-z(s1000+k+1)*s1
            wi=z(s1000+k)*s1+z(s1000+k+1)*c1
            wr0000=z(s0000+k)  +wr
            wi0000=z(s0000+k+1)+wi
            wr1000=z(s0000+k)  -wr
            wi1000=z(s0000+k+1)-wi
            wr=z(s1001+k)*c1-z(s1001+k+1)*s1
            wi=z(s1001+k)*s1+z(s1001+k+1)*c1
            wr0001=z(s0001+k)  +wr
            wi0001=z(s0001+k+1)+wi
            wr1001=z(s0001+k)  -wr
            wi1001=z(s0001+k+1)-wi
            wr=z(s1010+k)*c1-z(s1010+k+1)*s1
            wi=z(s1010+k)*s1+z(s1010+k+1)*c1
            wr0010=z(s0010+k)  +wr
            wi0010=z(s0010+k+1)+wi
            wr1010=z(s0010+k)  -wr
            wi1010=z(s0010+k+1)-wi
            wr=z(s1011+k)*c1-z(s1011+k+1)*s1
            wi=z(s1011+k)*s1+z(s1011+k+1)*c1
            wr0011=z(s0011+k)  +wr
            wi0011=z(s0011+k+1)+wi
            wr1011=z(s0011+k)  -wr
            wi1011=z(s0011+k+1)-wi
            wr=z(s1100+k)*c1-z(s1100+k+1)*s1
            wi=z(s1100+k)*s1+z(s1100+k+1)*c1
            wr0100=z(s0100+k)  +wr
            wi0100=z(s0100+k+1)+wi
            wr1100=z(s0100+k)  -wr
            wi1100=z(s0100+k+1)-wi
            wr=z(s1101+k)*c1-z(s1101+k+1)*s1
            wi=z(s1101+k)*s1+z(s1101+k+1)*c1
            wr0101=z(s0101+k)  +wr
            wi0101=z(s0101+k+1)+wi
            wr1101=z(s0101+k)  -wr
            wi1101=z(s0101+k+1)-wi
            wr=z(s1110+k)*c1-z(s1110+k+1)*s1
            wi=z(s1110+k)*s1+z(s1110+k+1)*c1
            wr0110=z(s0110+k)  +wr
            wi0110=z(s0110+k+1)+wi
            wr1110=z(s0110+k)  -wr
            wi1110=z(s0110+k+1)-wi
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
            wr=wr0101*c2-wi0101*s2
            wi=wr0101*s2+wi0101*c2
            wr0101=wr0001-wr
            wi0101=wi0001-wi
            wr0001=wr0001+wr
            wi0001=wi0001+wi
            wr=wr0110*c2-wi0110*s2
            wi=wr0110*s2+wi0110*c2
            wr0110=wr0010-wr
            wi0110=wi0010-wi
            wr0010=wr0010+wr
            wi0010=wi0010+wi
            wr=wr0111*c2-wi0111*s2
            wi=wr0111*s2+wi0111*c2
            wr0111=wr0011-wr
            wi0111=wi0011-wi
            wr0011=wr0011+wr
            wi0011=wi0011+wi
            wr=wr1100*c3-wi1100*s3
            wi=wr1100*s3+wi1100*c3
            wr1100=wr1000-wr
            wi1100=wi1000-wi
            wr1000=wr1000+wr
            wi1000=wi1000+wi
            wr=wr1101*c3-wi1101*s3
            wi=wr1101*s3+wi1101*c3
            wr1101=wr1001-wr
            wi1101=wi1001-wi
            wr1001=wr1001+wr
            wi1001=wi1001+wi
            wr=wr1110*c3-wi1110*s3
            wi=wr1110*s3+wi1110*c3
            wr1110=wr1010-wr
            wi1110=wi1010-wi
            wr1010=wr1010+wr
            wi1010=wi1010+wi
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
            wr=wr0011*c4-wi0011*s4
            wi=wr0011*s4+wi0011*c4
            wr0011=wr0001-wr
            wi0011=wi0001-wi
            wr0001=wr0001+wr
            wi0001=wi0001+wi
            wr=wr0110*c5-wi0110*s5
            wi=wr0110*s5+wi0110*c5
            wr0110=wr0100-wr
            wi0110=wi0100-wi
            wr0100=wr0100+wr
            wi0100=wi0100+wi
            wr=wr0111*c5-wi0111*s5
            wi=wr0111*s5+wi0111*c5
            wr0111=wr0101-wr
            wi0111=wi0101-wi
            wr0101=wr0101+wr
            wi0101=wi0101+wi
            wr=wr1010*c6-wi1010*s6
            wi=wr1010*s6+wi1010*c6
            wr1010=wr1000-wr
            wi1010=wi1000-wi
            wr1000=wr1000+wr
            wi1000=wi1000+wi
            wr=wr1011*c6-wi1011*s6
            wi=wr1011*s6+wi1011*c6
            wr1011=wr1001-wr
            wi1011=wi1001-wi
            wr1001=wr1001+wr
            wi1001=wi1001+wi
            wr=wr1110*c7-wi1110*s7
            wi=wr1110*s7+wi1110*c7
            wr1110=wr1100-wr
            wi1110=wi1100-wi
            wr1100=wr1100+wr
            wi1100=wi1100+wi
            wr=wr1111*c7-wi1111*s7
            wi=wr1111*s7+wi1111*c7
            wr1111=wr1101-wr
            wi1111=wi1101-wi
            wr1101=wr1101+wr
            wi1101=wi1101+wi

            wr=wr0001*c8-wi0001*s8
            wi=wr0001*s8+wi0001*c8
            z(s0000+k)  =wr0000+wr
            z(s0000+k+1)=wi0000+wi
            z(s0001+k)  =wr0000-wr
            z(s0001+k+1)=wi0000-wi
            wr=wr0011*c9-wi0011*s9
            wi=wr0011*s9+wi0011*c9
            z(s0010+k)  =wr0010+wr
            z(s0010+k+1)=wi0010+wi
            z(s0011+k)  =wr0010-wr
            z(s0011+k+1)=wi0010-wi
            wr=wr0101*ca-wi0101*sa
            wi=wr0101*sa+wi0101*ca
            z(s0100+k)  =wr0100+wr
            z(s0100+k+1)=wi0100+wi
            z(s0101+k)  =wr0100-wr
            z(s0101+k+1)=wi0100-wi
            wr=wr0111*cb-wi0111*sb
            wi=wr0111*sb+wi0111*cb
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
            wr=wr1011*cd-wi1011*sd
            wi=wr1011*sd+wi1011*cd
            z(s1010+k)  =wr1010+wr
            z(s1010+k+1)=wi1010+wi
            z(s1011+k)  =wr1010-wr
            z(s1011+k+1)=wi1010-wi
            wr=wr1101*ce-wi1101*se
            wi=wr1101*se+wi1101*ce
            z(s1100+k)  =wr1100+wr
            z(s1100+k+1)=wi1100+wi
            z(s1101+k)  =wr1100-wr
            z(s1101+k+1)=wi1100-wi
            wr=wr1111*cf-wi1111*sf
            wi=wr1111*sf+wi1111*cf
            z(s1110+k)  =wr1110+wr
            z(s1110+k+1)=wi1110+wi
            z(s1111+k)  =wr1110-wr
            z(s1111+k+1)=wi1110-wi
         end do
      end do
      end
************************************************************************
*     preparation for fft
*     set bit reversal table
*     nd:    dimension n(nd)
*     nmax = 2**max(n)
*     bitrev(0:nmax-1,nd)
************************************************************************
      subroutine fftprep1(nd,nmax,n,bitrev)
*odirr NOALTCODE
      implicit none
      integer nd,nmax
      integer n(nd),bitrev(0:nmax-1,nd)
      integer i,j,k,l,p,q
      do i=1, nd
         k=2**n(i)
         do j=0, k-1
            bitrev(j,i)=0
         end do
         p=1
         q=k/2
         do k=1, n(i)
*     tuning for vector machine
            if(q.gt.p)then
               do l=1, 2*p-1, 2
*vdir noloopchg
*SOPTION UNROLL(4)
                  do j=0, q-1
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            else
               do j=0, q-1
*vdir noloopchg
*SOPTION UNROLL(4)
                  do l=1, 2*p-1, 2
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            end if
*     p*q=k/2
            p=p*2
            q=q/2
         end do
      end do
      end
************************************************************************
*     preparation for fft
*     set phase table
*     nd:    dimension n(nd)
*     nmax = 2**max(n)
*     phase(0:(nmax/2-1),nd)
************************************************************************
      subroutine fftprep2(nd,nmax,n,phase)
*odirr NOALTCODE
      implicit none
      integer i,j,k,m,nd,n(nd),nmax
      real*8  phase(0:nmax-1,nd)
      real*8  delta,PIF
c     PIF=PI/4
      PIF=atan(1.d0)
      m=1
c     Find max{n}
      do i=2, nd
         if(n(i).gt.n(m)) m=i
      end do
      i=m
c     Calculate phase table for the most large n(i).
      if( n(i)-3 .ge. 0 ) then
         k=2**(n(i)-3)
         delta=PIF/k
         do j=0,k
            phase(2*j  ,i)=cos(j*delta)
            phase(2*j+1,i)=sin(j*delta)
         end do
         m=k*2
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=k+1,m
            phase(2*j  ,i)=phase(2*(m-j)+1,i)
            phase(2*j+1,i)=phase(2*(m-j)  ,i)
         end do
         k=k*4
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=m+1,k-1
            phase(2*j  ,i)=-phase(2*(k-j)  ,i)
            phase(2*j+1,i)= phase(2*(k-j)+1,i)
         end do
      else if ( n(i) .ge. 1 ) then
         phase(0,i)=1.d0
         phase(1,i)=0.d0
           if ( n(i) .eq. 2 ) then
              phase(2,i)=0.d0
              phase(3,i)=1.d0
           end if
      end if
C     Copy phase table to the others.
      k=i
      do i=1, nd
         if(i.ne.k)then
            m=2**(n(k)-n(i))
            do j=0, 2**(n(i)-1)-1
               phase(2*j  ,i)=phase(2*j*m  ,k)
               phase(2*j+1,i)=phase(2*j*m+1,k)
            end do
         end if
      end do
      end
************************************************************************
*     preparation for fft
*     conjugate phase table
*     phase(0:(nmax/2-1),nd)
************************************************************************
      subroutine fftprep3(nd,nmax,phase)
*odirr NOALTCODE
      implicit none
      integer j,nd,nmax
      real*8 phase(nd*nmax)
*vdir noaltcode
      do j=2, nd*nmax, 2
         phase(j)=-phase(j)
      end do
      end
************************************************************************
*     preparation for fft
*     bitreversal phase table
************************************************************************
      subroutine fftprep4(nd,nmax,n,phase,bitrev)
      implicit none
      integer i,j,k,m,nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      complex*16 phase(0:nmax/2-1,nd),c
c      write(*,'(2F10.4)') (phase(p,1),p=0,15)
      do i=1, nd
         k=2**(n(i)-1)
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=0, k-1
            m=bitrev(j*2,i)
            if(m.gt.j) then
               c=phase(m,i)
               phase(m,i)=phase(j,i)
               phase(j,i)=c
            end if
         end do
      end do
c      write(*,'(I4,2F10.4)') (bitrev(i*2,1),phase(i,1),i=0,15)
      end
************************************************************************
*     post process for ifft
*     multiply 1/2**(n(1)+n(2)+...+n(nd))
*     dimension x(2**n(1),2**n(2),...,2**n(nd))
************************************************************************
      subroutine fftipost(nd,n,x)
*odirr NOALTCODE
      implicit none
      complex*16 x(*)
      real*8     f
      integer    j,k,nd,n(nd),nsum
      nsum=0
      do j=1, nd
         nsum=nsum+n(j)
      end do
      k=2**nsum
      f=1.d0/k
*vdir noaltcode
      do j=1,k
         x(j)=f*x(j)
      end do
      end

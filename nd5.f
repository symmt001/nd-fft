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
         call mfftnb(n(i),1,z(k),phase(0,i))
         call btrv(q,WSIZE,z(k),bitrev(0,i))
         do j=0, q-1
            cwk(k+j)=z(k+j)
         end do
      end do
      r=q
c     Other
      do i=2, nd
         t=2**n(i)
         q=r*t
         nunroll=max(min(r,WSIZE/t),1)
         do k=0, s-1, q
            if(r.gt.nunroll)then
               do j=k, k+r-1, nunroll
                  do m=0, t-1
                     do l=0, nunroll-1
                        cwk(m*nunroll+l)=z(j+r*m+l)
                     end do
                  end do
                  call mfftnb(n(i),nunroll,cwk,phase(0,i))
                  do m=0, t-1
                     do l=0, nunroll-1
                        z(j+r*m+l)=cwk(bitrev(m,i)*nunroll+l)
                     end do
                  end do
               end do
            else
               do l=0, q-1
                  cwk(l)=z(k+l)
               end do
               call mfftnb(n(i),r,cwk,phase(0,i))
               do m=0, t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
                  do l=0, r-1
                     z(k+r*m+l)=cwk(bitrev(m,i)*r+l)
                  end do
               end do
            end if
         end do
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
      integer n,m,p,q,r
      q=2**(n+1)
      r=2
      p=mod(n,4)
c     first decimation
      if(p.eq.0)then
         q=q/16
         call dec4fm(m,q,z,phase)
         r=r*16
      else if(p.eq.3)then
         q=q/8
         call dec3fm(m,q,z,phase)
         r=r*8
      else if(p.eq.2)then
         q=q/4
         call dec2fm(m,q,z,phase)
         r=r*4
      else if(p.eq.1 .and. n.gt.1)then
         q=q/32
         call dec5fm(m,q,z,phase)
         r=r*32
      else if(p.eq.1 .and. n.eq.1)then
         q=q/2
         call dec1fm(m,q,z)
         r=r*2
      else
         stop
      end if
c     main loop
 10   continue
         q=q/16
         call dec4m(m,q,r,z,phase)
         r=r*16
      if (q.gt.2) goto 10
      end
************************************************************************
*     rearranging data as bit-reversal order, in-place version
************************************************************************
      subroutine btrv(q,wsize,z,bitrev)
      implicit none
      integer i,j,k,l,q,t,p,wsize
      integer bitrev(0:q-1)
      complex*16 z(0:q-1),x
      if( q .le. wsize ) then
         do k=0, q-1
            l=bitrev(k)
            if(l.gt.k)then
               x=z(l)
               z(l)=z(k)
               z(k)=x
            end if
         end do
      else
         p=1
 10      continue
c     divide window into 2 bank (l=bitrev(k) and k)
         if( p*p*8 .le. wsize ) then
            p=p*2
            goto 10
         end if
         t=q/p
         do k=0, t-1, p
            l=bitrev(k)
            if(l.gt.k)then
               do j=0, p-1
                  do i=0, p-1
                     x=z(l+j+bitrev(i))
                     z(l+j+bitrev(i))=z(k+bitrev(j)+i)
                     z(k+bitrev(j)+i)=x
                  end do
               end do
            else if(l.eq.k) then
               do j=0, p-1
                  do i=0, j-1
                     x=z(k+j+bitrev(i))
                     z(k+j+bitrev(i))=z(k+bitrev(j)+i)
                     z(k+bitrev(j)+i)=x
                  end do
               end do
            end if
         end do
      end if
      end
************************************************************************
*     rearranging data as bit-reversal order, in-place version
************************************************************************
      subroutine btrvm(q,m,wsize,z,bitrev)
      implicit none
      integer j,k,l,m,q,r,wsize,nunroll
      integer bitrev(0:q-1)
      complex*16 z(0:q*m-1),x
      nunroll=max(min(wsize/2,m),1)
c      write(*,*) nunroll
      do j=0, q-1
         k=bitrev(j)
         if(k.gt.j)then
            do l=0, m-1, nunroll
*SOPTION UNROLL(2)
               do r=l, l+nunroll-1
                  x=z(j*m+r)
                  z(j*m+r)=z(k*m+r)
                  z(k*m+r)=x
               end do
            end do
         end if
      end do
      end
************************************************************************
*     decimate 5 times
************************************************************************
      subroutine dec5fm(m,q,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*),wr,wi
      real*8 c12,s12,c13,s13,iflag
      real*8 c14,s14,c15,s15,c16,s16,c17,s17
      real*8 c18,s18,c19,s19,c1a,s1a,c1b,s1b
      real*8 c1c,s1c,c1d,s1d,c1e,s1e,c1f,s1f
      real*8 wr00000,wr00001,wr00010,wr00011
      real*8 wi00000,wi00001,wi00010,wi00011
      real*8 wr00100,wr00101,wr00110,wr00111
      real*8 wi00100,wi00101,wi00110,wi00111
      real*8 wr01000,wr01001,wr01010,wr01011
      real*8 wi01000,wi01001,wi01010,wi01011
      real*8 wr01100,wr01101,wr01110,wr01111
      real*8 wi01100,wi01101,wi01110,wi01111
      real*8 wr10000,wr10001,wr10010,wr10011
      real*8 wi10000,wi10001,wi10010,wi10011
      real*8 wr10100,wr10101,wr10110,wr10111
      real*8 wi10100,wi10101,wi10110,wi10111
      real*8 wr11000,wr11001,wr11010,wr11011
      real*8 wi11000,wi11001,wi11010,wi11011
      real*8 wr11100,wr11101,wr11110,wr11111
      real*8 wi11100,wi11101,wi11110,wi11111
      integer k,m,q,qm
      integer s00000,s00001,s00010,s00011
      integer s00100,s00101,s00110,s00111
      integer s01000,s01001,s01010,s01011
      integer s01100,s01101,s01110,s01111
      integer s10000,s10001,s10010,s10011
      integer s10100,s10101,s10110,s10111
      integer s11000,s11001,s11010,s11011
      integer s11100,s11101,s11110,s11111
      qm=q*m
       iflag=phase(3)
         c12=phase(4)
         s12=phase(5)
         c13=phase(6)
         s13=phase(7)
         c14=phase(8)
         s14=phase(9)
         c15=phase(10)
         s15=phase(11)
         c16=phase(12)
         s16=phase(13)
         c17=phase(14)
         s17=phase(15)
         c18=phase(16)
         s18=phase(17)
         c19=phase(18)
         s19=phase(19)
         c1a=phase(20)  
         s1a=phase(21)
         c1b=phase(22)
         s1b=phase(23)
         c1c=phase(24)
         s1c=phase(25)
         c1d=phase(26)
         s1d=phase(27)
         c1e=phase(28)
         s1e=phase(29)
         c1f=phase(30)
         s1f=phase(31)

         s00000=0
         s00001=qm+s00000
         s00010=qm+s00001
         s00011=qm+s00010
         s00100=qm+s00011
         s00101=qm+s00100
         s00110=qm+s00101
         s00111=qm+s00110
         s01000=qm+s00111
         s01001=qm+s01000
         s01010=qm+s01001
         s01011=qm+s01010
         s01100=qm+s01011
         s01101=qm+s01100
         s01110=qm+s01101
         s01111=qm+s01110
         s10000=qm+s01111
         s10001=qm+s10000
         s10010=qm+s10001
         s10011=qm+s10010
         s10100=qm+s10011
         s10101=qm+s10100
         s10110=qm+s10101
         s10111=qm+s10110
         s11000=qm+s10111
         s11001=qm+s11000
         s11010=qm+s11001
         s11011=qm+s11010
         s11100=qm+s11011
         s11101=qm+s11100
         s11110=qm+s11101
         s11111=qm+s11110
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s10000+k)
            wi=z(s10000+k+1)
            wr00000=z(s00000+k)  +wr
            wi00000=z(s00000+k+1)+wi
            wr10000=z(s00000+k)  -wr
            wi10000=z(s00000+k+1)-wi
            wr=z(s10001+k)
            wi=z(s10001+k+1)
            wr00001=z(s00001+k)  +wr
            wi00001=z(s00001+k+1)+wi
            wr10001=z(s00001+k)  -wr
            wi10001=z(s00001+k+1)-wi
            wr=z(s10010+k)
            wi=z(s10010+k+1)
            wr00010=z(s00010+k)  +wr
            wi00010=z(s00010+k+1)+wi
            wr10010=z(s00010+k)  -wr
            wi10010=z(s00010+k+1)-wi
            wr=z(s10011+k)
            wi=z(s10011+k+1)
            wr00011=z(s00011+k)  +wr
            wi00011=z(s00011+k+1)+wi
            wr10011=z(s00011+k)  -wr
            wi10011=z(s00011+k+1)-wi
            wr=z(s10100+k)
            wi=z(s10100+k+1)
            wr00100=z(s00100+k)  +wr
            wi00100=z(s00100+k+1)+wi
            wr10100=z(s00100+k)  -wr
            wi10100=z(s00100+k+1)-wi
            wr=z(s10101+k)
            wi=z(s10101+k+1)
            wr00101=z(s00101+k)  +wr
            wi00101=z(s00101+k+1)+wi
            wr10101=z(s00101+k)  -wr
            wi10101=z(s00101+k+1)-wi
            wr=z(s10110+k)
            wi=z(s10110+k+1)
            wr00110=z(s00110+k)  +wr
            wi00110=z(s00110+k+1)+wi
            wr10110=z(s00110+k)  -wr
            wi10110=z(s00110+k+1)-wi
            wr=z(s10111+k)
            wi=z(s10111+k+1)
            wr00111=z(s00111+k)  +wr
            wi00111=z(s00111+k+1)+wi
            wr10111=z(s00111+k)  -wr
            wi10111=z(s00111+k+1)-wi
            wr=z(s11000+k)
            wi=z(s11000+k+1)
            wr01000=z(s01000+k)  +wr
            wi01000=z(s01000+k+1)+wi
            wr11000=z(s01000+k)  -wr
            wi11000=z(s01000+k+1)-wi
            wr=z(s11001+k)
            wi=z(s11001+k+1)
            wr01001=z(s01001+k)  +wr
            wi01001=z(s01001+k+1)+wi
            wr11001=z(s01001+k)  -wr
            wi11001=z(s01001+k+1)-wi
            wr=z(s11010+k)
            wi=z(s11010+k+1)
            wr01010=z(s01010+k)  +wr
            wi01010=z(s01010+k+1)+wi
            wr11010=z(s01010+k)  -wr
            wi11010=z(s01010+k+1)-wi
            wr=z(s11011+k)
            wi=z(s11011+k+1)
            wr01011=z(s01011+k)  +wr
            wi01011=z(s01011+k+1)+wi
            wr11011=z(s01011+k)  -wr
            wi11011=z(s01011+k+1)-wi
            wr=z(s11100+k)
            wi=z(s11100+k+1)
            wr01100=z(s01100+k)  +wr
            wi01100=z(s01100+k+1)+wi
            wr11100=z(s01100+k)  -wr
            wi11100=z(s01100+k+1)-wi
            wr=z(s11101+k)
            wi=z(s11101+k+1)
            wr01101=z(s01101+k)  +wr
            wi01101=z(s01101+k+1)+wi
            wr11101=z(s01101+k)  -wr
            wi11101=z(s01101+k+1)-wi
            wr=z(s11110+k)
            wi=z(s11110+k+1)
            wr01110=z(s01110+k)  +wr
            wi01110=z(s01110+k+1)+wi
            wr11110=z(s01110+k)  -wr
            wi11110=z(s01110+k+1)-wi
            wr=z(s11111+k)
            wi=z(s11111+k+1)
            wr01111=z(s01111+k)  +wr
            wi01111=z(s01111+k+1)+wi
            wr11111=z(s01111+k)  -wr
            wi11111=z(s01111+k+1)-wi

            wr=wr01000
            wi=wi01000
            wr01000=wr00000-wr
            wi01000=wi00000-wi
            wr00000=wr00000+wr
            wi00000=wi00000+wi
            wr=wr01001
            wi=wi01001
            wr01001=wr00001-wr
            wi01001=wi00001-wi
            wr00001=wr00001+wr
            wi00001=wi00001+wi
            wr=wr01010
            wi=wi01010
            wr01010=wr00010-wr
            wi01010=wi00010-wi
            wr00010=wr00010+wr
            wi00010=wi00010+wi
            wr=wr01011
            wi=wi01011
            wr01011=wr00011-wr
            wi01011=wi00011-wi
            wr00011=wr00011+wr
            wi00011=wi00011+wi
            wr=wr01100
            wi=wi01100
            wr01100=wr00100-wr
            wi01100=wi00100-wi
            wr00100=wr00100+wr
            wi00100=wi00100+wi
            wr=wr01101
            wi=wi01101
            wr01101=wr00101-wr
            wi01101=wi00101-wi
            wr00101=wr00101+wr
            wi00101=wi00101+wi
            wr=wr01110
            wi=wi01110
            wr01110=wr00110-wr
            wi01110=wi00110-wi
            wr00110=wr00110+wr
            wi00110=wi00110+wi
            wr=wr01111
            wi=wi01111
            wr01111=wr00111-wr
            wi01111=wi00111-wi
            wr00111=wr00111+wr
            wi00111=wi00111+wi
            wr=-wi11000*iflag
            wi= wr11000*iflag
            wr11000=wr10000-wr
            wi11000=wi10000-wi
            wr10000=wr10000+wr
            wi10000=wi10000+wi
            wr=-wi11001*iflag
            wi= wr11001*iflag
            wr11001=wr10001-wr
            wi11001=wi10001-wi
            wr10001=wr10001+wr
            wi10001=wi10001+wi
            wr=-wi11010*iflag
            wi= wr11010*iflag
            wr11010=wr10010-wr
            wi11010=wi10010-wi
            wr10010=wr10010+wr
            wi10010=wi10010+wi
            wr=-wi11011*iflag
            wi= wr11011*iflag
            wr11011=wr10011-wr
            wi11011=wi10011-wi
            wr10011=wr10011+wr
            wi10011=wi10011+wi
            wr=-wi11100*iflag
            wi= wr11100*iflag
            wr11100=wr10100-wr
            wi11100=wi10100-wi
            wr10100=wr10100+wr
            wi10100=wi10100+wi
            wr=-wi11101*iflag
            wi= wr11101*iflag
            wr11101=wr10101-wr
            wi11101=wi10101-wi
            wr10101=wr10101+wr
            wi10101=wi10101+wi
            wr=-wi11110*iflag
            wi= wr11110*iflag
            wr11110=wr10110-wr
            wi11110=wi10110-wi
            wr10110=wr10110+wr
            wi10110=wi10110+wi
            wr=-wi11111*iflag
            wi= wr11111*iflag
            wr11111=wr10111-wr
            wi11111=wi10111-wi
            wr10111=wr10111+wr
            wi10111=wi10111+wi

            wr=wr00100
            wi=wi00100
            wr00100=wr00000-wr
            wi00100=wi00000-wi
            wr00000=wr00000+wr
            wi00000=wi00000+wi
            wr=wr00101
            wi=wi00101
            wr00101=wr00001-wr
            wi00101=wi00001-wi
            wr00001=wr00001+wr
            wi00001=wi00001+wi
            wr=wr00110
            wi=wi00110
            wr00110=wr00010-wr
            wi00110=wi00010-wi
            wr00010=wr00010+wr
            wi00010=wi00010+wi
            wr=wr00111
            wi=wi00111
            wr00111=wr00011-wr
            wi00111=wi00011-wi
            wr00011=wr00011+wr
            wi00011=wi00011+wi
            wr=-wi01100*iflag
            wi= wr01100*iflag
            wr01100=wr01000-wr
            wi01100=wi01000-wi
            wr01000=wr01000+wr
            wi01000=wi01000+wi
            wr=-wi01101*iflag
            wi= wr01101*iflag
            wr01101=wr01001-wr
            wi01101=wi01001-wi
            wr01001=wr01001+wr
            wi01001=wi01001+wi
            wr=-wi01110*iflag
            wi= wr01110*iflag
            wr01110=wr01010-wr
            wi01110=wi01010-wi
            wr01010=wr01010+wr
            wi01010=wi01010+wi
            wr=-wi01111*iflag
            wi= wr01111*iflag
            wr01111=wr01011-wr
            wi01111=wi01011-wi
            wr01011=wr01011+wr
            wi01011=wi01011+wi
            wr=wr10100*c12-wi10100*s12
            wi=wr10100*s12+wi10100*c12
            wr10100=wr10000-wr
            wi10100=wi10000-wi
            wr10000=wr10000+wr
            wi10000=wi10000+wi
            wr=wr10101*c12-wi10101*s12
            wi=wr10101*s12+wi10101*c12
            wr10101=wr10001-wr
            wi10101=wi10001-wi
            wr10001=wr10001+wr
            wi10001=wi10001+wi
            wr=wr10110*c12-wi10110*s12
            wi=wr10110*s12+wi10110*c12
            wr10110=wr10010-wr
            wi10110=wi10010-wi
            wr10010=wr10010+wr
            wi10010=wi10010+wi
            wr=wr10111*c12-wi10111*s12
            wi=wr10111*s12+wi10111*c12
            wr10111=wr10011-wr
            wi10111=wi10011-wi
            wr10011=wr10011+wr
            wi10011=wi10011+wi
            wr=wr11100*c13-wi11100*s13
            wi=wr11100*s13+wi11100*c13
            wr11100=wr11000-wr
            wi11100=wi11000-wi
            wr11000=wr11000+wr
            wi11000=wi11000+wi
            wr=wr11101*c13-wi11101*s13
            wi=wr11101*s13+wi11101*c13
            wr11101=wr11001-wr
            wi11101=wi11001-wi
            wr11001=wr11001+wr
            wi11001=wi11001+wi
            wr=wr11110*c13-wi11110*s13
            wi=wr11110*s13+wi11110*c13
            wr11110=wr11010-wr
            wi11110=wi11010-wi
            wr11010=wr11010+wr
            wi11010=wi11010+wi
            wr=wr11111*c13-wi11111*s13
            wi=wr11111*s13+wi11111*c13
            wr11111=wr11011-wr
            wi11111=wi11011-wi
            wr11011=wr11011+wr
            wi11011=wi11011+wi

            wr=wr00010
            wi=wi00010
            wr00010=wr00000-wr
            wi00010=wi00000-wi
            wr00000=wr00000+wr
            wi00000=wi00000+wi
            wr=wr00011
            wi=wi00011
            wr00011=wr00001-wr
            wi00011=wi00001-wi
            wr00001=wr00001+wr
            wi00001=wi00001+wi
            wr=-wi00110*iflag
            wi= wr00110*iflag
            wr00110=wr00100-wr
            wi00110=wi00100-wi
            wr00100=wr00100+wr
            wi00100=wi00100+wi
            wr=-wi00111*iflag
            wi= wr00111*iflag
            wr00111=wr00101-wr
            wi00111=wi00101-wi
            wr00101=wr00101+wr
            wi00101=wi00101+wi
            wr=wr01010*c12-wi01010*s12
            wi=wr01010*s12+wi01010*c12
            wr01010=wr01000-wr
            wi01010=wi01000-wi
            wr01000=wr01000+wr
            wi01000=wi01000+wi
            wr=wr01011*c12-wi01011*s12
            wi=wr01011*s12+wi01011*c12
            wr01011=wr01001-wr
            wi01011=wi01001-wi
            wr01001=wr01001+wr
            wi01001=wi01001+wi
            wr=wr01110*c13-wi01110*s13
            wi=wr01110*s13+wi01110*c13
            wr01110=wr01100-wr
            wi01110=wi01100-wi
            wr01100=wr01100+wr
            wi01100=wi01100+wi
            wr=wr01111*c13-wi01111*s13
            wi=wr01111*s13+wi01111*c13
            wr01111=wr01101-wr
            wi01111=wi01101-wi
            wr01101=wr01101+wr
            wi01101=wi01101+wi
            wr=wr10010*c14-wi10010*s14
            wi=wr10010*s14+wi10010*c14
            wr10010=wr10000-wr
            wi10010=wi10000-wi
            wr10000=wr10000+wr
            wi10000=wi10000+wi
            wr=wr10011*c14-wi10011*s14
            wi=wr10011*s14+wi10011*c14
            wr10011=wr10001-wr
            wi10011=wi10001-wi
            wr10001=wr10001+wr
            wi10001=wi10001+wi
            wr=wr10110*c15-wi10110*s15
            wi=wr10110*s15+wi10110*c15
            wr10110=wr10100-wr
            wi10110=wi10100-wi
            wr10100=wr10100+wr
            wi10100=wi10100+wi
            wr=wr10111*c15-wi10111*s15
            wi=wr10111*s15+wi10111*c15
            wr10111=wr10101-wr
            wi10111=wi10101-wi
            wr10101=wr10101+wr
            wi10101=wi10101+wi
            wr=wr11010*c16-wi11010*s16
            wi=wr11010*s16+wi11010*c16
            wr11010=wr11000-wr
            wi11010=wi11000-wi
            wr11000=wr11000+wr
            wi11000=wi11000+wi
            wr=wr11011*c16-wi11011*s16
            wi=wr11011*s16+wi11011*c16
            wr11011=wr11001-wr
            wi11011=wi11001-wi
            wr11001=wr11001+wr
            wi11001=wi11001+wi
            wr=wr11110*c17-wi11110*s17
            wi=wr11110*s17+wi11110*c17
            wr11110=wr11100-wr
            wi11110=wi11100-wi
            wr11100=wr11100+wr
            wi11100=wi11100+wi
            wr=wr11111*c17-wi11111*s17
            wi=wr11111*s17+wi11111*c17
            wr11111=wr11101-wr
            wi11111=wi11101-wi
            wr11101=wr11101+wr
            wi11101=wi11101+wi

            wr=wr00001
            wi=wi00001
            z(s00000+k)  =wr00000+wr
            z(s00000+k+1)=wi00000+wi
            z(s00001+k)  =wr00000-wr
            z(s00001+k+1)=wi00000-wi
            wr=-wi00011*iflag
            wi= wr00011*iflag
            z(s00010+k)  =wr00010+wr
            z(s00010+k+1)=wi00010+wi
            z(s00011+k)  =wr00010-wr
            z(s00011+k+1)=wi00010-wi
            wr=wr00101*c12-wi00101*s12
            wi=wr00101*s12+wi00101*c12
            z(s00100+k)  =wr00100+wr
            z(s00100+k+1)=wi00100+wi
            z(s00101+k)  =wr00100-wr
            z(s00101+k+1)=wi00100-wi
            wr=wr00111*c13-wi00111*s13
            wi=wr00111*s13+wi00111*c13
            z(s00110+k)  =wr00110+wr
            z(s00110+k+1)=wi00110+wi
            z(s00111+k)  =wr00110-wr
            z(s00111+k+1)=wi00110-wi
            wr=wr01001*c14-wi01001*s14
            wi=wr01001*s14+wi01001*c14
            z(s01000+k)  =wr01000+wr
            z(s01000+k+1)=wi01000+wi
            z(s01001+k)  =wr01000-wr
            z(s01001+k+1)=wi01000-wi
            wr=wr01011*c15-wi01011*s15
            wi=wr01011*s15+wi01011*c15
            z(s01010+k)  =wr01010+wr
            z(s01010+k+1)=wi01010+wi
            z(s01011+k)  =wr01010-wr
            z(s01011+k+1)=wi01010-wi
            wr=wr01101*c16-wi01101*s16
            wi=wr01101*s16+wi01101*c16
            z(s01100+k)  =wr01100+wr
            z(s01100+k+1)=wi01100+wi
            z(s01101+k)  =wr01100-wr
            z(s01101+k+1)=wi01100-wi
            wr=wr01111*c17-wi01111*s17
            wi=wr01111*s17+wi01111*c17
            z(s01110+k)  =wr01110+wr
            z(s01110+k+1)=wi01110+wi
            z(s01111+k)  =wr01110-wr
            z(s01111+k+1)=wi01110-wi
            wr=wr10001*c18-wi10001*s18
            wi=wr10001*s18+wi10001*c18
            z(s10000+k)  =wr10000+wr
            z(s10000+k+1)=wi10000+wi
            z(s10001+k)  =wr10000-wr
            z(s10001+k+1)=wi10000-wi
            wr=wr10011*c19-wi10011*s19
            wi=wr10011*s19+wi10011*c19
            z(s10010+k)  =wr10010+wr
            z(s10010+k+1)=wi10010+wi
            z(s10011+k)  =wr10010-wr
            z(s10011+k+1)=wi10010-wi
            wr=wr10101*c1a-wi10101*s1a
            wi=wr10101*s1a+wi10101*c1a
            z(s10100+k)  =wr10100+wr
            z(s10100+k+1)=wi10100+wi
            z(s10101+k)  =wr10100-wr
            z(s10101+k+1)=wi10100-wi
            wr=wr10111*c1b-wi10111*s1b
            wi=wr10111*s1b+wi10111*c1b
            z(s10110+k)  =wr10110+wr
            z(s10110+k+1)=wi10110+wi
            z(s10111+k)  =wr10110-wr
            z(s10111+k+1)=wi10110-wi
            wr=wr11001*c1c-wi11001*s1c
            wi=wr11001*s1c+wi11001*c1c
            z(s11000+k)  =wr11000+wr
            z(s11000+k+1)=wi11000+wi
            z(s11001+k)  =wr11000-wr
            z(s11001+k+1)=wi11000-wi
            wr=wr11011*c1d-wi11011*s1d
            wi=wr11011*s1d+wi11011*c1d
            z(s11010+k)  =wr11010+wr
            z(s11010+k+1)=wi11010+wi
            z(s11011+k)  =wr11010-wr
            z(s11011+k+1)=wi11010-wi
            wr=wr11101*c1e-wi11101*s1e
            wi=wr11101*s1e+wi11101*c1e
            z(s11100+k)  =wr11100+wr
            z(s11100+k+1)=wi11100+wi
            z(s11101+k)  =wr11100-wr
            z(s11101+k+1)=wi11100-wi
            wr=wr11111*c1f-wi11111*s1f
            wi=wr11111*s1f+wi11111*c1f
            z(s11110+k)  =wr11110+wr
            z(s11110+k+1)=wi11110+wi
            z(s11111+k)  =wr11110-wr
            z(s11111+k+1)=wi11110-wi
         end do
      end
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
*     decimate twice, only for the first decimation
************************************************************************
      subroutine dec2fm(m,q,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*)
      real*8 wr,wi,iflag
      real*8 wr00,wr01,wr10,wr11,wi00,wi01,wi10,wi11
      integer k,m,q,qm
      integer s00,s01,s10,s11
      iflag=phase(3)
      qm=q*m
      s00=0
      s01=qm+s00
      s10=qm+s01
      s11=qm+s10
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do k=0, qm-1, 2
         wr=z(s10+k)
         wi=z(s10+k+1)
         wr00=z(s00+k  )+wr
         wi00=z(s00+k+1)+wi
         wr10=z(s00+k  )-wr
         wi10=z(s00+k+1)-wi
         wr=z(s11+k)
         wi=z(s11+k+1)
         wr01=z(s01+k  )+wr
         wi01=z(s01+k+1)+wi
         wr11=z(s01+k  )-wr
         wi11=z(s01+k+1)-wi

         z(s00+k  )=wr00+wr01
         z(s00+k+1)=wi00+wi01
         z(s01+k  )=wr00-wr01
         z(s01+k+1)=wi00-wi01
         wi= wr11*iflag
         wr=-wi11*iflag
         z(s10+k  )=wr10+wr
         z(s10+k+1)=wi10+wi
         z(s11+k  )=wr10-wr
         z(s11+k+1)=wi10-wi
      end do
      end
************************************************************************
*     decimate once, only for the first decimation
************************************************************************
      subroutine dec1fm(m,q,z)
      real*8 z(0:*)
      real*8 wr,wi
      integer k,m,q,qm
      qm=q*m
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do k=0, qm-1, 2
         wr=z(qm+k)
         wi=z(qm+k+1)
         z(qm+k  )=z(k  )-wr
         z(qm+k+1)=z(k+1)-wi
         z(   k  )=z(k  )+wr
         z(   k+1)=z(k+1)+wi
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

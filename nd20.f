************************************************************************
*     N-dimensional fft routine
*     Multiple Call of Decimation-In-Time Method 
*     z:  Input & Output (serialized x(i,j,k))
*     n:  dimension x(2**n(1),2**n(2),2**n(3))
*     phase: phase table prepared by fftprep2 & fftprep3
*     wk: work area
*     bitrev: bit reversal table prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+n(3))
*     Written by S. Yamamoto
************************************************************************
      subroutine fftnd(nd,nmax,n,z,wk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      real*8 z(*),wk(*),phase(nmax,nd)
      integer i,p,q,r,iside
      iside=1
      r=0
      do i=1, nd
         r=r+n(i)
      end do
      r=2**r
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         call mfftw(n(i),q,z,wk,phase(1,i),iside)
         if(iside.eq.1)then
            call transp(q,p,z,wk)
         else
            call transp(q,p,wk,z)
         end if
         iside=-iside
      end do
      if(iside.ne.1)then
         do i=1, 2*r
            z(i)=wk(i)
         end do
      end if
      end
************************************************************************
*     multiple fft routine
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 wk(m,2**n): work space
*     complex*16 phase(2**(n-1)) : phase factors
************************************************************************
      subroutine mfftw(n,m,z,wk,phase,iside)
      implicit none
      real*8 z(0:*),wk(0:*),phase(0:*),iflag
      integer n,m,q,r,iside
      r=2**(n+1)
      q=r
      iflag=phase(r/4+1)
c     first decimation
         if(q.ge.16)then
            q=q/8
            if(iside.eq.1)then
               call decb3fm(m,q,iflag,z,wk,phase)
            else
               call decb3fm(m,q,iflag,wk,z,phase)
            end if
            iside=-iside
         end if
c      do while(q.gt.2)
 10   continue
c     Decimate 4 times -> less performance on SX-3
c$$$         if(q.ge.32)then
c$$$            q=q/16
c$$$            if(iside.eq.1)then
c$$$               call decb4m(m,q,r/16,z,wk,phase)
c$$$            else
c$$$               call decb4m(m,q,r/16,wk,z,phase)
c$$$            end if
c$$$            iside=-iside
c$$$         else if(q.ge.16)then
         if(q.ge.16)then
            q=q/8
            if(iside.eq.1)then
               call decb3m(m,q,r/8,z,wk,phase)
            else
               call decb3m(m,q,r/8,wk,z,phase)
            end if
            iside=-iside
         else if(q.ge.8)then
            q=q/4
            if(iside.eq.1)then
               call decb2m(m,q,r/4,z,wk,phase)
            else
               call decb2m(m,q,r/4,wk,z,phase)
            end if
            iside=-iside
         else if(q.ge.4)then
            q=q/2
            if(iside.eq.1)then
               call decb1m(m,q,r/2,z,wk,phase)
            else
               call decb1m(m,q,r/2,wk,z,phase)
            end if
            iside=-iside
         end if
      if (q.gt.2) goto 10
c      end do
      end
************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     This routine is designed especially for the case
*     that both q and p equal to 2**n. But it works on the other case.
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k
      complex*16 dst(0:q*p-1),src(0:q*p-1)
c     Avoiding Bank Conflict
      if( q.ge.p ) then
*POPTION INDEP
         do k=0, q-p
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1
c$$$               dst((k+j)*p+j)=src(j*q+(k+j))
               dst(k*p+(p+1)*j)=src((q+1)*j+k)
            end do
         end do
*POPTION INDEP
         do k=1, p-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
            end do
         end do
      else
*POPTION INDEP
         do k=0, p-q
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, q-1
c$$$               dst(j*p+(k+j))=src((k+j)*q+j)
               dst((p+1)*j+k)=src(k*q+(q+1)*j)
            end do
         end do
*POPTION INDEP
         do k=1, q-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
            end do
         end do
      end if
      end 
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
************************************************************************
*     decimate once with bit-reversal
************************************************************************
      subroutine decb1m(m,q,r,z,wk,phase)
      real*8 z(0:*),wk(0:*),phase(0:*)
      real*8 c,s,wr,wi
      integer j,k,m,q,r,qm
      integer s0,s1,d0,d1
      qm=q*m
*POPTION INDEP
      do j=0, r-1, q
         c=phase(j)
         s=phase(j+1)
         s0=m*j*2
         s1=qm+s0
         d0=m*j
         d1=r*m+d0
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1, 2
            wr=z(s1+k)*c-z(s1+k+1)*s
            wi=z(s1+k)*s+z(s1+k+1)*c
            wk(d0+k  )=z(s0+k  )+wr
            wk(d0+k+1)=z(s0+k+1)+wi
            wk(d1+k  )=z(s0+k  )-wr
            wk(d1+k+1)=z(s0+k+1)-wi
         end do
      end do
      end 
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
*     calculating displacement
            bitrev(j,i)=-j
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
*     dummy for compatibility problem
************************************************************************
      subroutine fftprep4(nd,nmax,n,phase,bitrev)
      implicit none
      integer nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      complex*16 phase(0:nmax/2-1,nd)
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

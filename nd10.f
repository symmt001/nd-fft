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
c     Fortran90
      recursive subroutine fftnd(d,nmax,n,z,cwk,phase,bitrev)
c      subroutine fftnd(d,nmax,n,z,cwk,phase,bitrev)
      implicit none
      integer nmax,d,n(d),bitrev(0:nmax-1,d)
      integer j,l,m,q,s,t,nunroll
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,d)
      integer WSIZE
c     WSIZE equals to the half of window size
      parameter(WSIZE=128*1024/16)
      t=2**n(d)
      if(d.eq.1)then
         call mfftnb(n(d),1,z,phase(0,d))
         call btrv(t,WSIZE,z,bitrev(0,d))
         return
      end if
      s=0
      do j=1, d-1
         s=s+n(j)
      end do
      q=2**s
      s=t*q
      if(s.gt.WSIZE)then
         if(t.le.WSIZE)then
            nunroll=WSIZE/t
c     loop transform for efficient use of cache
            do j=0, q-1, nunroll
               do m=0, t-1
c     for efficient use of cache line (in other word burst transfer)
                  do l=0, nunroll-1
                     cwk(m*nunroll+l)=z(j+m*q+l)
                  end do
               end do
               call mfftnb(n(d),nunroll,cwk,phase(0,d))
               do m=0, t-1
                  do l=0, nunroll-1
                     z(j+m*q+l)=cwk(bitrev(m,d)*nunroll+l)
                  end do
               end do
            end do
         else
c     **in progress** to be more efficient
            do j=0, q-1
               do m=0, t-1
                  cwk(m)=z(j+m*q)
               end do
               call mfftnb(n(d),1,cwk,phase(0,d))
               do m=0, t-1
                  z(j+m*q)=cwk(bitrev(m,d))
               end do
            end do
         end if
      else
         call mfftnb(n(d),q,z,phase(0,d))
         call btrvm(t,q,WSIZE,z,bitrev(0,d))
      end if
      do j=s-q, 0, -q
         call fftnd(d-1,nmax,n,z(j),cwk,phase,bitrev)
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
      if(q.ge.16)then
         q=q/8
         call dec3fm(m,q,z,phase)
         r=r*8
      end if
c     do while(q.gt.2)
 10   continue
c      if(q.ge.16)then
c         q=q/8
c         call dec3m(m,q,r,z,phase)
c         r=r*8
c      else if(q.ge.8)then
      if(q.ge.8)then
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

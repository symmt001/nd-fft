************************************************************************
*     fft test
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,ITE,NTOT
      real*8  EPS, usrtime, u_start, u_end, t, q
      parameter(ND=3,ITE=2)
c      parameter(ND=3,ITE=10)
c      parameter(ND=3,ITE=100)
c      parameter(ND=3,ITE=1000)
c      parameter(NPX=4,NPY=4,NPZ=4)
c      parameter(NPX=5,NPY=5,NPZ=5)
c      parameter(NPX=6,NPY=6,NPZ=6)
      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
      parameter(NTOT=NX*NY*NZ)
c      parameter(NPM=max(NPX,NPY,NPZ),NM=2**NPM)
      parameter(NPM=NPZ,NM=2**NPM)
      parameter(EPS=1.d-13)
      complex*16 phase(0:NM/2-1,ND),phx,phy,phz,cwk(NX,NY,NZ)
      real*8     xr(0:NX-1,0:NY-1,0:NZ-1),xi(0:NX-1,0:NY-1,0:NZ-1)
      real*8     rndm(ND),emax,y
      integer    bitrev(NM,ND),n(ND),p(ND),nemax(ND)
      integer    i,j,k,l
      data       n/NPX,NPY,NPZ/
      common     xr,xi,cwk

      call RINISP(2147483647)
      call rndoid(3,rndm)

*     set bit reversal table
      call fftprep1(ND,NM,n,bitrev)
*     set phase table
      call fftprep2(ND,NM,n,phase)
      do i=1, 3
         p(i)=int(rndm(i)*(2**n(i)))
      end do
      do k=0, NZ-1
         l=mod(k*p(3),NZ)
         if(l.ge.NZ/2)then
            phz=-phase(l-NZ/2,3)
         else
            phz= phase(l,3)
         end if
         do j=0, NY-1
            l=mod(j*p(2),NY)
            if(l.ge.NY/2)then
               phy=-phase(l-NY/2,2)
            else
               phy= phase(l,2)
            end if
            do i=0, NX-1
c$$$  x(i+1)=cdexp(iu*PI2/dble(NMAX)(i*p(1)+j*p(2)+k*p(3)))
               l=mod(i*p(1),NX)
               if(l.ge.NX/2)then
                  phx=-phase(l-NX/2,1)
               else
                  phx= phase(l,1)
               end if
               xr(i,j,k)=dble (phx*phy*phz)
               xi(i,j,k)=dimag(phx*phy*phz)
            end do
         end do
      end do
*     call fftprep1(ND,NM,n,bitrev)
*     call fftprep2(ND,NM,n,phase)
*     (phase() = phase(bitrev()) , if it is needed)
      call fftprep4(ND,NM,n,phase,bitrev)
      write(*,*) 'Testing...'
      u_start=usrtime()
      do i=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NM,phase)
         call fftnd(ND,NM,n,xr,xi,cwk,phase,bitrev)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,NM,phase)
         call fftnd(ND,NM,n,xr,xi,cwk,phase,bitrev)
*     x is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times x
         call fftipost(ND,n,xr,xi)
      end do
*     phase = conjg(phase), for DFT
c      call fftprep3(ND,NM,phase)
      call fftnd(ND,NM,n,xr,xi,cwk,phase,bitrev)
      u_end=usrtime()
      call fftipost(ND,n,xr,xi)
c     IDFT
      do i=1, 3
         if (p(i).ne.0) p(i)=2**n(i)-p(i)
      end do
      emax =0.d0
      xr(p(1),p(2),p(3))=xr(p(1),p(2),p(3))-1.d0
      do k=0, 2**n(3)-1
         do j=0, 2**n(2)-1
            do i=0, 2**n(1)-1
               y=sqrt(xr(i,j,k)*xr(i,j,k)+xi(i,j,k)*xi(i,j,k))
               if( y .gt. emax) then
                  nemax(1)=i
                  nemax(2)=j
                  nemax(3)=k
                  emax=y
               end if
            end do
         end do
      end do
      t=(u_end-u_start)/(2*ITE+1)
      q=5*NTOT*log(dble(NTOT))/log(2.d0)
      write(*,200) NX,NY,NZ,2*ITE+1,t,q/t/1.d6
      if(emax.gt.EPS) write(*,201) emax,nemax,p
 200  format(1P,'NX=',I4,', NY=',I4,', NZ=',I4,', M=',I4,
     $     ', TIME/M=',E9.2,', MFLOPS=',E9.2)
 201  format(1P,'MaxError=',E9.2,' ,Place:',3I4,' ,Incident:',3I4)
      end
************************************************************************      
*     GENERATE RANDOM NUMBERS FOLLOWING UNIFORM DISTRIBUTION
*     rndm \in (0,1]
************************************************************************      
      subroutine rndoid(NRANDOM,rndm)
!$HP$check_overflow integer off
      implicit none
      integer NRANDOM,nmax,irand,i,k
      real*8 rndm(*)
      common/rndmsd/k,nmax
      irand(i)=iand(i*1812433253,nmax)
      do 10 i=1, NRANDOM
         k=irand(k)
         rndm(i)=k/dble(nmax)
 10   continue
      end
************************************************************************
*     INITIALIZE RANDOM NUMBERS
************************************************************************
      subroutine rinisp(iseed)
      implicit none
      integer iseed,nmax,k
      common/rndmsd/k,nmax
c      nmax=2**31-1
      nmax=2**30-1
      nmax=nmax*2+1
      k=iseed
      end
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
      subroutine fftnd(nd,nmax,n,xr,xi,wk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      real*8 xr(*),xi(*),wk(*),phase(nmax,nd)
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
         call mfftw(n(i),q,xr,xi,wk(1),wk(r+1),phase(1,i),iside)
         if(iside.eq.1)then
            call transp(q,p,xr,xi,wk(1),wk(r+1))
         else
            call transp(q,p,wk(1),wk(r+1),xr,xi)
         end if
         iside=-iside
      end do
      if(iside.ne.1)then
         do i=1, r
            xr(i)=wk(i)
            xi(i)=wk(r+i)
         end do
      end if
      end
************************************************************************
*     multiple fft routine
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors
************************************************************************
      subroutine mfftw(n,m,xr,xi,wr,wi,phase,iside)
      implicit none
      real*8 xr(0:*),wr(0:*),xi(0:*),wi(0:*),phase(0:*),iflag
      integer n,m,q,r,iside
      r=2**(n+1)
      q=r
      iflag=phase(r/4+1)
c     first decimation
         if(q.ge.16)then
            q=q/8
            if(iside.eq.1)then
               call decb3fm(m,q,iflag,xr,xi,wr,wi,phase)
            else
               call decb3fm(m,q,iflag,wr,wi,xr,xi,phase)
            end if
            iside=-iside
         end if
c      do while(q.gt.2)
 10   continue
         if(q.ge.16)then
            q=q/8
            if(iside.eq.1)then
               call decb3m(m,q,r/8,xr,xi,wr,wi,phase)
            else
               call decb3m(m,q,r/8,wr,wi,xr,xi,phase)
            end if
            iside=-iside
         else if(q.ge.8)then
            q=q/4
            if(iside.eq.1)then
               call decb2m(m,q,r/4,xr,xi,wr,wi,phase)
            else
               call decb2m(m,q,r/4,wr,wi,xr,xi,phase)
            end if
            iside=-iside
         else if(q.ge.4)then
            q=q/2
            if(iside.eq.1)then
               call decb1m(m,q,r/2,xr,xi,wr,wi,phase)
            else
               call decb1m(m,q,r/2,wr,wi,xr,xi,phase)
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
      subroutine transp(q,p,sr,si,dr,di)
      implicit none
      integer p,q,j,k
      real*8 dr(0:q*p-1),sr(0:q*p-1)
      real*8 di(0:q*p-1),si(0:q*p-1)
c     Avoiding Bank Conflict
      if( q.ge.p ) then
         do k=0, q-p
*VOCL LOOP,NOVREC(SR,SI,DR,DI)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1
               dr(k*p+(p+1)*j)=sr((q+1)*j+k)
               di(k*p+(p+1)*j)=si((q+1)*j+k)
            end do
         end do
         do k=1, p-1
*VOCL LOOP,NOVREC(SR,SI,DR,DI)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
               dr((q-k)*p+(p+1)*j)=sr((q-k)  +(q+1)*j)
               di((q-k)*p+(p+1)*j)=si((q-k)  +(q+1)*j)
               dr((p-k)  +(p+1)*j)=sr((p-k)*q+(q+1)*j)
               di((p-k)  +(p+1)*j)=si((p-k)*q+(q+1)*j)
            end do
         end do
      else
         do k=0, p-q
*VOCL LOOP,NOVREC(SR,SI,DR,DI)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, q-1
               dr((p+1)*j+k)=sr(k*q+(q+1)*j)
               di((p+1)*j+k)=si(k*q+(q+1)*j)
            end do
         end do
         do k=1, q-1
*VOCL LOOP,NOVREC(SR,SI,DR,DI)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
               dr((p-k)  +(p+1)*j)=sr((p-k)*q+(q+1)*j)
               di((p-k)  +(p+1)*j)=si((p-k)*q+(q+1)*j)
               dr((q-k)*p+(p+1)*j)=sr((q-k)  +(q+1)*j)
               di((q-k)*p+(p+1)*j)=si((q-k)  +(q+1)*j)
            end do
         end do
      end if
      end 
************************************************************************
*     decimate trice with bit-reversal
************************************************************************
      subroutine decb3m(m,q,r,xr,xi,wkr,wki,phase)
      implicit none
      real*8 xr(0:*),xi(0:*),wkr(0:*),wki(0:*),phase(0:*)
      real*8 c1,s1,c2,s2,c3,s3,c4,s4,c5,s5,c6,s6,c7,s7,wr,wi
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer j,k,m,q,r,qm
      integer s000,s001,s010,s011,d000,d001,d010,d011
      integer s100,s101,s110,s111,d100,d101,d110,d111
      qm=q*m/2
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

         s000=m*j*4
         s001=qm+s000
         s010=qm+s001
         s011=qm+s010
         s100=qm+s011
         s101=qm+s100
         s110=qm+s101
         s111=qm+s110
c     bit-reverse order
         d000=m*j/2
         d100=r*m/2+d000
         d010=r*m/2+d100
         d110=r*m/2+d010
         d001=r*m/2+d110
         d101=r*m/2+d001
         d011=r*m/2+d101
         d111=r*m/2+d011
*VOCL LOOP,NOVREC(WKR,WKI,XR,XI)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1
            wr=xr(s100+k)*c1-xi(s100+k)*s1
            wi=xr(s100+k)*s1+xi(s100+k)*c1

            wr000=xr(s000+k)+wr
            wi000=xi(s000+k)+wi
            wr100=xr(s000+k)-wr
            wi100=xi(s000+k)-wi
            wr=xr(s110+k)*c1-xi(s110+k)*s1
            wi=xr(s110+k)*s1+xi(s110+k)*c1
            wr010=xr(s010+k)+wr
            wi010=xi(s010+k)+wi
            wr110=xr(s010+k)-wr
            wi110=xi(s010+k)-wi
            wr=xr(s101+k)*c1-xi(s101+k)*s1
            wi=xr(s101+k)*s1+xi(s101+k)*c1
            wr001=xr(s001+k)+wr
            wi001=xi(s001+k)+wi
            wr101=xr(s001+k)-wr
            wi101=xi(s001+k)-wi
            wr=xr(s111+k)*c1-xi(s111+k)*s1
            wi=xr(s111+k)*s1+xi(s111+k)*c1
            wr011=xr(s011+k)+wr
            wi011=xi(s011+k)+wi
            wr111=xr(s011+k)-wr
            wi111=xi(s011+k)-wi

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
            wkr(d000+k)=wr000+wr
            wki(d000+k)=wi000+wi
            wkr(d001+k)=wr000-wr
            wki(d001+k)=wi000-wi
            wr=wr011*c6-wi011*s6
            wi=wr011*s6+wi011*c6
            wkr(d010+k)=wr010+wr
            wki(d010+k)=wi010+wi
            wkr(d011+k)=wr010-wr
            wki(d011+k)=wi010-wi
            wr=wr101*c5-wi101*s5
            wi=wr101*s5+wi101*c5
            wkr(d100+k)=wr100+wr
            wki(d100+k)=wi100+wi
            wkr(d101+k)=wr100-wr
            wki(d101+k)=wi100-wi
            wr=wr111*c7-wi111*s7
            wi=wr111*s7+wi111*c7
            wkr(d110+k)=wr110+wr
            wki(d110+k)=wi110+wi
            wkr(d111+k)=wr110-wr
            wki(d111+k)=wi110-wi
         end do
      end do
      end
************************************************************************
*     decimate twice with bit-reversal
************************************************************************
      subroutine decb2m(m,q,r,xr,xi,wkr,wki,phase)
      implicit none
      real*8 xr(0:*),wkr(0:*),phase(0:*)
      real*8 xi(0:*),wki(0:*)
      real*8 c1,s1,c2,s2,c3,s3,wr,wi
      real*8 wr00,wr01,wr10,wr11,wi00,wi01,wi10,wi11
      integer j,k,m,q,r,qm
      integer s00,s01,s10,s11,d00,d01,d10,d11
      qm=q*m/2
      do j=0, r-1, q
         c1=phase(2*j)
         s1=phase(2*j+1)
         c2=phase(j)
         s2=phase(j+1)
         c3=phase(j+r)
         s3=phase(j+r+1)
         s00=m*j*2
         s01=qm+s00
         s10=qm+s01
         s11=qm+s10
c     bit-reverse order
         d00=m*j/2
         d10=r*m/2+d00
         d01=r*m/2+d10
         d11=r*m/2+d01
*VOCL LOOP,NOVREC(WKR,WKI,XR,XI)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1
            wr=xr(s10+k)*c1-xi(s10+k)*s1
            wi=xr(s10+k)*s1+xi(s10+k)*c1
            wr00=xr(s00+k)+wr
            wi00=xi(s00+k)+wi
            wr10=xr(s00+k)-wr
            wi10=xi(s00+k)-wi
            wr=xr(s11+k)*c1-xi(s11+k)*s1
            wi=xr(s11+k)*s1+xi(s11+k)*c1
            wr01=xr(s01+k)+wr
            wi01=xi(s01+k)+wi
            wr11=xr(s01+k)-wr
            wi11=xi(s01+k)-wi

            wr=wr01*c2-wi01*s2
            wi=wr01*s2+wi01*c2
            wkr(d00+k)=wr00+wr
            wki(d00+k)=wi00+wi
            wkr(d01+k)=wr00-wr
            wki(d01+k)=wi00-wi
            wr=wr11*c3-wi11*s3
            wi=wr11*s3+wi11*c3
            wkr(d10+k)=wr10+wr
            wki(d10+k)=wi10+wi
            wkr(d11+k)=wr10-wr
            wki(d11+k)=wi10-wi
         end do
      end do
      end
************************************************************************
*     decimate once with bit-reversal
************************************************************************
      subroutine decb1m(m,q,r,xr,xi,wkr,wki,phase)
      real*8 xr(0:*),wkr(0:*),phase(0:*)
      real*8 xi(0:*),wki(0:*)
      real*8 c,s,wr,wi
      integer j,k,m,q,r,qm
      integer s0,s1,d0,d1
      qm=q*m/2
      do j=0, r-1, q
         c=phase(j)
         s=phase(j+1)
         s0=m*j
         s1=qm+s0
         d0=m*j/2
         d1=r*m/2+d0
*VOCL LOOP,NOVREC(WKR,WKI,XR,XI)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1
            wr=xr(s1+k)*c-xi(s1+k)*s
            wi=xr(s1+k)*s+xi(s1+k)*c
            wkr(d0+k)=xr(s0+k)+wr
            wki(d0+k)=xi(s0+k)+wi
            wkr(d1+k)=xr(s0+k)-wr
            wki(d1+k)=xi(s0+k)-wi
         end do
      end do
      end 
************************************************************************
*     decimate first 3 times with bit-reversal
************************************************************************
      subroutine decb3fm(m,q,iflag,xr,xi,wkr,wki,phase)
      implicit none
      real*8 xr(0:*),wkr(0:*),phase(0:*)
      real*8 xi(0:*),wki(0:*)
      real*8 c5,wr,wi,iflag
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      integer k,m,q,qm
      integer s000,s001,s010,s011
      integer s100,s101,s110,s111
c     r=q
      qm=q*m/2
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
*VOCL LOOP,NOVREC(WKR,WKI,XR,XI)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do k=0, qm-1
            wr000=xr(s000+k)+xr(s100+k)
            wi000=xi(s000+k)+xi(s100+k)
            wr100=xr(s000+k)-xr(s100+k)
            wi100=xi(s000+k)-xi(s100+k)
            wr001=xr(s001+k)+xr(s101+k)
            wi001=xi(s001+k)+xi(s101+k)
            wr101=xr(s001+k)-xr(s101+k)
            wi101=xi(s001+k)-xi(s101+k)
            wr010=xr(s010+k)+xr(s110+k)
            wi010=xi(s010+k)+xi(s110+k)
            wr110=xr(s010+k)-xr(s110+k)
            wi110=xi(s010+k)-xi(s110+k)
            wr011=xr(s011+k)+xr(s111+k)
            wi011=xi(s011+k)+xi(s111+k)
            wr111=xr(s011+k)-xr(s111+k)
            wi111=xi(s011+k)-xi(s111+k)

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
            wkr(s000+k)=wr000+wr
            wki(s000+k)=wi000+wi
            wkr(s100+k)=wr000-wr
            wki(s100+k)=wi000-wi
            wr=-wi011*iflag
            wi= wr011*iflag
            wkr(s010+k)=wr010+wr
            wki(s010+k)=wi010+wi
            wkr(s110+k)=wr010-wr
            wki(s110+k)=wi010-wi
            wr= (wr101-wi101*iflag)*c5
            wi= (wr101*iflag+wi101)*c5
            wkr(s001+k)=wr100+wr
            wki(s001+k)=wi100+wi
            wkr(s101+k)=wr100-wr
            wki(s101+k)=wi100-wi
            wi= (wr111*iflag-wi111)*c5
            wr=-(wr111+wi111*iflag)*c5
            wkr(s011+k)=wr110+wr
            wki(s011+k)=wi110+wi
            wkr(s111+k)=wr110-wr
            wki(s111+k)=wi110-wi
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
                  do j=0, q-1
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            else
               do j=0, q-1
*vdir noloopchg
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
      subroutine fftipost(nd,n,xr,xi)
*odirr NOALTCODE
      implicit none
      real*8     f,xr(*),xi(*)
      integer    j,k,nd,n(nd),nsum
      nsum=0
      do j=1, nd
         nsum=nsum+n(j)
      end do
      k=2**nsum
      f=1.d0/k
*vdir noaltcode
      do j=1,k
         xr(j)=f*xr(j)
         xi(j)=f*xi(j)
      end do
      end

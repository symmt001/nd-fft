************************************************************************
*     fft test
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,ITE,NTOT
      real*8  EPS, usrtime, u_start, u_end, t, q
c      parameter(ND=3,ITE=0)
      parameter(ND=3,ITE=10)
c      parameter(ND=3,ITE=100)
c      parameter(ND=3,ITE=1000)
c      parameter(NPX=4,NPY=4,NPZ=4)
c      parameter(NPX=5,NPY=5,NPZ=5)
      parameter(NPX=6,NPY=6,NPZ=6)
c      parameter(NPX=7,NPY=7,NPZ=7)
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
               xr(i,j,k)=dble(phx*phy*phz)
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
      subroutine fftprep1(nd,nmax,n,bitrev)
      implicit none
      integer nd,n(nd),nmax,bitrev(nmax,nd)
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
************************************************************************
*     N-dimensional fft routine
*     Multiple Call of JFC
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
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      real*8  xr(*),xi(*),wk(*),phase(0:nmax-1,nd)
      integer i,iw,icon,NMSSL
      parameter(NMSSL=10)
      integer nssl(NMSSL),isn(NMSSL)
      if(nd.gt.NMSSL)stop'nd must be smaller than or equal to NMSSL'
      iw=1
      do i=1, nd
         nssl(i)=2**n(i)
         isn(i)=-int(phase(nssl(i)/2+1,i))
         iw=iw*nssl(i)
      end do
      icon=0
      call DVMCFT(xr,xi,nssl,nd,isn,wk,2*iw,icon)
      if (icon.ne.0) then
         write(*,*) iw,isn
         write(*,*) 'ERROR: DVMCFT CODE=',icon
         stop
      end if
      end

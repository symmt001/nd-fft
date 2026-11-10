************************************************************************
*     fft test
************************************************************************
      program ffttest_MPP
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none

      integer ITE
      parameter(ITE=4)

      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,MZ
      parameter(ND=3)
c      parameter(NPX=3,NPY=3,NPZ=3)
c      parameter(NPX=4,NPY=4,NPZ=4)
c      parameter(NPX=5,NPY=5,NPZ=5)
c      parameter(NPX=6,NPY=6,NPZ=6)
c      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NPX=8,NPY=8,NPZ=8)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
c      parameter(NPM=max(NPX,NPY,NPZ),NM=2**NPM)
      parameter(NPM=NPZ,NM=2**NPM)

c     NP : Number of Processors
      integer NP,myid
      parameter(NP=8)

      complex(kind=8),dimension(0:NX-1,0:NY-1,0:NZ/NP-1)::z

      integer NTOT,ierr
      integer sc_start, sc_end, c_rate
      real*8  EPS, t, q

      parameter(NTOT=NX*NY*NZ)
      parameter(EPS=1.d-13)
      complex(kind=8)::  phase(0:NM/2-1,ND),phx,phy,phz
      real(kind=8)   ::  rndm(ND),emax,y
      integer    bitrev(NM,ND),n(ND),p(ND),nemax(ND)
      integer    i,j,k,l

      integer LSTPU(NP),LOM(NP),IENV(4)
      integer IOPT1(2),IOPT2(2),IWKSIZE
      real(kind=8) TB(0:NM/4+1)

*     MATRIX/MPP INIT

      LSTPU(1)=-1 ; LOM(1)=-1

      IWKSIZE=max((NX+1)*(NY+1)*NZ/NP,
     $     (NY+1)*(NZ+1)*((NX-1)/NP+1))*32
      call HMATINIT(IWKSIZE,LSTPU,NP,ierr)

      call HKXPARA(IENV)
      myid=IENV(1)
      if(NP.ne.IENV(2)) then
         write(*,*) 'inconsistent NP'
         goto 999
      end if
      MZ=NZ/np

      n(1)=NPX
      n(2)=NPY
      n(3)=NPZ

      do i=1, ND
         call random_number(rndm(i))
         p(i)=int(rndm(i)*(2**n(i)))
      end do
      

*     set bit reversal table
      call fftprep1(ND,NM,n,bitrev)
*     set phase table
      call fftprep2(ND,NM,n,phase)
      
      do k=0, MZ-1
         l=mod((k+myid*MZ)*p(3),NZ)
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
c$$$  z(i)=cdexp(iu*PI2/dble(NMAX)(i*p(1)+j*p(2)+k*p(3)))
               l=mod(i*p(1),NX)
               if(l.ge.NX/2)then
                  phx=-phase(l-NX/2,1)
               else
                  phx= phase(l,1)
               end if
               z(i,j,k)=phx*phy*phz
            end do
         end do
      end do
c      goto 999
*     call fftprep1(ND,NM,n,bitrev)
*     call fftprep2(ND,NM,n,phase)
*     (phase() = phase(bitrev()) , if it is needed)
      call fftprep4(ND,NM,n,phase,bitrev)

c     MATRIX/MPP PHASE TABLE INIT
c IOPT1(1):(-2,IDFT/N)(-1,DFT/N)(1,DFT)(2,IDFT)
c IOPT1(2):(0,set TBL only)(1,Trans+TBL)(2,Trans/preset TBL)
c IOPT2(1):(1,no check)(2,check)
c IOPT2(2):(1,no info)(2,info)

      IOPT1=(/1,0/) ; IOPT2=(/1,1/)

      call HZFT6MPP(LSTPU,NP,NX,NY,NZ,Z,NX,NY,LOM,
     $     TB,IOPT1,IOPT2,ierr)

      IOPT1(2)=2

      call SYSTEM_CLOCK(sc_start)

      do i=1, ITE
*     DFT
      IOPT1(1)=2
      call HZFT6MPP(LSTPU,NP,NX,NY,NZ,Z,NX,NY,LOM,
     $     TB,IOPT1,IOPT2,ierr)
*     IDFT
      IOPT1(1)=1
      call HZFT6MPP(LSTPU,NP,NX,NY,NZ,Z,NX,NY,LOM,
     $     TB,IOPT1,IOPT2,ierr)
      z=z/NTOT
      end do
*     for DFT
      IOPT1(1)=2
      call HZFT6MPP(LSTPU,NP,NX,NY,NZ,Z,NX,NY,LOM,
     $     TB,IOPT1,IOPT2,ierr)
      z=z/NTOT

      call SYSTEM_CLOCK(sc_end,c_rate)

c     IDFT
      do i=1, 3
         if (p(i).ne.0) p(i)=2**n(i)-p(i)
      end do
      l=p(3)/MZ
      if( l.eq.myid ) then
         l=mod(p(3),MZ)
         z(p(1),p(2),l)=z(p(1),p(2),l)-(1.d0,0.d0)
      end if
      emax =0.d0
      do k=0, MZ-1
         do j=0, NY-1
            do i=0, NX-1
               y=abs(z(i,j,k))
               if( y .gt. emax) then
                  nemax(1)=i
                  nemax(2)=j
                  nemax(3)=k
                  emax=y
               end if
            end do
         end do
      end do

c     mili sec unit
      t=dble(sc_end-sc_start)/c_rate
      t=t/(2*ITE+1)
      q=5*NTOT*log(dble(NTOT))/(log(2.d0)*1.d6)
      write(*,202) 'REAL ',t,q/t

      if(emax.gt.EPS) write(*,201) emax,myid,nemax,p
 200  format(1P,'NX=',I4,', NY=',I4,', NZ=',I4,', M=',I4,', #PRC=',I4)
 201  format(1P,'MaxError=',E9.2,' ,Place:',4I4,' ,Incident:',3I4)
 202  format(1P,A5,'TIME/M=',E9.2,', MFLOPS=',E9.2)

 999  continue
      call HMATEXIT(ierr)
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

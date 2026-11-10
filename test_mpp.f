************************************************************************
*     fft test
************************************************************************
      program ffttest_MPP
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none

      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,MZ
      parameter(ND=3)
      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
      parameter(NPM=NPZ,NM=2**NPM)

c     NP : Number of Processors
      integer NP,myid
      parameter(NP=2**1)

      complex(kind=8),dimension(0:NX-1,0:NY-1,0:NZ/NP-1)::z

      integer NTOT,ierr
      integer sc_start, sc_end, c_rate
      real*8  EPS, t, q

      parameter(NTOT=NX*NY*NZ)
      parameter(EPS=1.d-13)
      complex(kind=8)::  phx,phy,phz
      real(kind=8)   ::  rndm(ND),emax,y
      integer    n(ND),p(ND),nemax(ND)
      integer    i,j,k,l

      integer LSTPU(NP),LOM(NP),IENV(4)
      integer IOPT1(2),IOPT2(2),IWKSIZE
      real*8 TB(0:NM/4+1)

*     MATRIX/MPP INIT

      LSTPU=(/(i,i=0,NP-1)/) ; LOM=NZ/NP
c      write(*,*) LSTPU
c      write(*,*) LOM
      IWKSIZE=max((NX+1)*(NY+1)*NZ/NP,
     $     (NY+1)*(NZ+1)*((NX-1)/NP+1))*32
      call HMATINIT(IWKSIZE,LSTPU,NP,ierr)

      call HKXPARA(IENV)
      myid=IENV(1)

      if(ienv(2).ne.np) then
         write(*,*) 'inconsistent NP'
         goto 999
      end if

      n=(/NPX,NPY,NPZ/)

      j=n(1)
      do i=1, ND
         call random_number(rndm(i))
         p(i)=int(rndm(i)*(2**n(i)))
         j=max(j,n(i))
      end do
      
      MZ=NZ/np

c     MATRIX/MPP PHASE TABLE INIT
c IOPT1(1):(-2,IDFT/N)(-1,DFT/N)(1,DFT)(2,IDFT)
c IOPT1(2):(0,set TBL only)(1,Trans+TBL)(2,Trans/preset TBL)
c IOPT2(1):(1,no check)(2,check)
c IOPT2(2):(1,no info)(2,info)

      IOPT1=(/1,0/) ; IOPT2=(/2,2/)

      call HZFT6MPP(LSTPU,NP,NX,NY,NZ,Z,NX,NY,LOM,
     $     TB,IOPT1,IOPT2,ierr)

      IOPT1(2)=2
      q=atan(1.d0)*8.d0
      do k=0, MZ-1
         l=(k+myid*MZ)*p(3)
         t=q*l/dble(NZ)
         phz=dcmplx(cos(t),sin(t))
         do j=0, NY-1
            l=j*p(2)
            t=q*l/dble(NY)
            phy=dcmplx(cos(t),sin(t))
            do i=0, NX-1
               l=i*p(1)
               t=q*l/dble(NX)
               phx=dcmplx(cos(t),sin(t))
               z(i,j,k)=phx*phy*phz
            end do
         end do
      end do

      call SYSTEM_CLOCK(sc_start)

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
      q=5*NTOT*log(dble(NTOT))/(log(2.d0)*1.d6)
      write(*,202) 'REAL ',t,q/t

      if(emax.gt.EPS) write(*,201) emax,myid,nemax,p
 200  format(1P,'NX=',I4,', NY=',I4,', NZ=',I4,', M=',I4,', #PRC=',I4)
 201  format(1P,'MaxError=',E9.2,' ,Place:',4I4,' ,Incident:',3I4)
 202  format(1P,A5,'TIME/M=',E9.2,', MFLOPS=',E9.2)

 999  continue
      call HMATEXIT(ierr)
      end

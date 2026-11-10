************************************************************************
*     fft test
************************************************************************
      program ffttest_mpi
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none

      include 'mpif.h'

      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,ITE,NTOT,ROOT
      integer sc_start, sc_end, c_rate ,MZ, NPMIN, NPMAX
      real*8  EPS, u_start, u_end, t, q, usrtime
c      parameter(ND=3,ITE=10)
      parameter(ND=3,ITE=10)
c      parameter(NPX=3,NPY=3,NPZ=3)
c      parameter(NPX=5,NPY=5,NPZ=5)
c      parameter(NPX=6,NPY=6,NPZ=6)
c      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NPMIN=1,NPMAX=2**3)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
      parameter(NTOT=NX*NY*NZ)
c      parameter(NPM=max(NPX,NPY,NPZ),NM=2**NPM)
      parameter(NPM=NPZ,NM=2**NPM)
      parameter(EPS=1.d-13,ROOT=0)
      complex(kind=8)::  phase(0:NM/2-1,ND),phx,phy,phz
      real(kind=8)   ::  rndm(ND),emax,y
      real(kind=8),dimension(NPMAX) :: tbuf
      integer    bitrev(NM,ND),n(ND),p(ND),nemax(ND)
      integer    i,j,k,l

C     MPI related variables
      integer ierr,np,myid 

c     FFT DATA STORAGE
c
c     Hitachi SR8000, SR2201 -rdma option
c      can reduce communication overhead drastically. But,
c      that mechanism is available only for static storage.
c
      complex(kind=8),dimension(0:NX-1,0:NY-1,0:NZ/NPMIN-1)::x,cwk
      common x,cwk
c      complex(kind=8),allocatable,dimension(:,:,:)::x,cwk

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)

      MZ=NZ/np
c      allocate(x(0:NX-1,0:NY-1,0:MZ-1),cwk(NX,NY,MZ))

      n(1)=NPX
      n(2)=NPY
      n(3)=NPZ
      y=log(2.0)
      l=int(log(real(np))/y+0.1)
      k=int(exp(y*l)+0.1)

      j=n(1)
      do i=1, ND
         call random_number(rndm(i))
         p(i)=int(rndm(i)*(2**n(i)))
         j=max(j,n(i))
      end do
      
      if(k .ne. np .or. np.gt.2**j) then
         if(myid.eq.ROOT)
     $        write(*,*) 'inconsistent number of processors'
         goto 999
      end if


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
c$$$  x(i+1)=cdexp(iu*PI2/dble(NMAX)(i*p(1)+j*p(2)+k*p(3)))
               l=mod(i*p(1),NX)
               if(l.ge.NX/2)then
                  phx=-phase(l-NX/2,1)
               else
                  phx= phase(l,1)
               end if
               x(i,j,k)=phx*phy*phz
            end do
         end do
      end do
c      goto 999
*     call fftprep1(ND,NM,n,bitrev)
*     call fftprep2(ND,NM,n,phase)
*     (phase() = phase(bitrev()) , if it is needed)
      call fftprep4(ND,NM,n,phase,bitrev)

      call MPI_Barrier(MPI_COMM_WORLD,ierr)

      call SYSTEM_CLOCK(sc_start)
      u_start=usrtime()

      do i=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NM,phase)
         call fftnd_mpi(ND,NM,n,x,cwk,phase,bitrev,myid,np)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,NM,phase)
         call fftnd_mpi(ND,NM,n,x,cwk,phase,bitrev,myid,np)
*     x is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times x
         call fftipost_mpi(ND,n,x,np)
      end do
*     phase = conjg(phase), for DFT
c      call fftprep3(ND,NM,phase)
      call fftnd_mpi(ND,NM,n,x,cwk,phase,bitrev,myid,np)
      call fftipost_mpi(ND,n,x,np)

      u_end=usrtime()
      call SYSTEM_CLOCK(sc_end,c_rate)

c     IDFT
      do i=1, 3
         if (p(i).ne.0) p(i)=2**n(i)-p(i)
      end do
      l=p(3)/MZ
      if( l.eq.myid ) then
         l=mod(p(3),MZ)
         x(p(1),p(2),l)=x(p(1),p(2),l)-(1.d0,0.d0)
      end if
      emax =0.d0
      do k=0, MZ-1
         do j=0, NY-1
            do i=0, NX-1
               y=abs(x(i,j,k))
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
      t=(u_end-u_start)/(2*ITE+1)
      call MPI_Gather(t,1,MPI_REAL8, tbuf,1,MPI_REAL8,
     $     ROOT, MPI_COMM_WORLD, ierr)
      if(myid.eq.ROOT)then
         write(*,200) NX,NY,NZ,2*ITE+1,np
         t=sum(tbuf(1:np))
         q=5*NTOT*log(dble(NTOT))/(log(2.d0)*1.d6)
         write(*,202) 'CPU  ',t,q/t
      end if

c     mili sec unit
      t=dble(sc_end-sc_start)/c_rate
      call MPI_Gather(t,1,MPI_REAL8, tbuf,1,MPI_REAL8,
     $     ROOT, MPI_COMM_WORLD, ierr)
      if(myid.eq.ROOT)then
         t=maxval(tbuf(1:np))/(2*ITE+1)
         q=5*NTOT*log(dble(NTOT))/(log(2.d0)*1.d6)
         write(*,202) 'REAL ',t,q/t
      end if

      if(emax.gt.EPS) write(*,201) emax,myid,nemax,p
 200  format(1P,'NX=',I4,', NY=',I4,', NZ=',I4,', M=',I4,', #PRC=',I4)
 201  format(1P,'MaxError=',E9.2,' ,Place:',4I4,' ,Incident:',3I4)
 202  format(1P,A5,'TIME/M=',E9.2,', MFLOPS=',E9.2)

 999  continue
      call MPI_FINALIZE(ierr)
      end

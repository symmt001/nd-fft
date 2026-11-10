************************************************************************
*     fft test
************************************************************************
      program ffttest_rdma
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none

      integer ITE
      parameter(ITE=0)

      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,MZ
      parameter(ND=3)
c      parameter(NPX=3,NPY=3,NPZ=3)
c      parameter(NPX=5,NPY=5,NPZ=5)
c      parameter(NPX=6,NPY=6,NPZ=6)
      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
c      parameter(NPM=max(NPX,NPY,NPZ),NM=2**NPM)
      parameter(NPM=NPZ,NM=2**NPM)

      integer NTOT
      integer sc_start, sc_end, c_rate
      real*8  EPS, u_start, u_end, t, q, usrtime

      parameter(NTOT=NX*NY*NZ)
      parameter(EPS=1.d-13)
      real(kind=8)   ::  rndm(ND),emax,y
      complex(kind=8)::  phase(0:NM/2-1,ND),phx,phy,phz

C     -RDMA SECTION-
      include 'rdma_h.f'
      complex(kind=8),dimension(0:NX-1,0:NY-1,0:NZ/MINNP-1)::z,w
C     -RDMA SECTION-
      real(kind=8),dimension(0:2*LBUF-1)::wk
      equivalence (w,wk)

      integer    bitrev(NM,ND),n(ND),p(ND),nemax(ND)
      integer    i,j,k,m

*     inode=/myid,np,-reserved-,-reserved-/
      ierr=$RGETNOD(inode)
      if(ierr.ne.0) stop 'Failed to get NODE ID'
*     get objects ID
      ierr=$RPARAM(1,iobj,IKEY)
      if(ierr.ne.0) stop 'Failed to get RDMA ID'

*     NP: # OF PROCESSORS
*     INIT Q,T,L,LCHUNK
*     T: OUTERMOST DIM
*     Q: (TOTAL # OF DATA)/T
*     L: T/NP
*     LCHUNK: Q/NP (LENGTH OF CHUNK)

      j=0
      do i=1, d-1
         j=j+n(i)
      end do
      q=2**j
      t=2**n(d)
      lchunk=q/np
      l=t/np

*     Blocking Receive
      do i=0, l-1

         do j=0, np-1

            m=i*np+j
            k=j*l+i

            ierr=$RCREATE(iobj,z(m*lchunk),16*LCHUNK,m,0,desc(m))
            if( ierr.ne.0 ) stop 'failed to create field'
            ierr=$RCREATE(iobj,w(k*lchunk),16*LCHUNK,t+k,0,desc(t+k))
            if( ierr.ne.0 ) stop 'failed to create field'

         end do

      end do

      
      write(*,*) 'np=',np,',  myid=',myid

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

      MZ=NZ/np


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
      if(myid.eq.ROOT) then
         write(*,*) 'Testing...'
      end if


      call SYSTEM_CLOCK(sc_start)
      u_start=usrtime()

      do i=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NM,phase)
         call fftnd_rdma(ND,NM,n,z,w,phase,bitrev)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,NM,phase)
         call fftnd_rdma(ND,NM,n,z,w,phase,bitrev)
*     z is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times z
         call fftipost_rdma(ND,n,z,np)
      end do
*     phase = conjg(phase), for DFT
c      call fftprep3(ND,NM,phase)
      call fftnd_rdma(ND,NM,n,z,w,phase,bitrev)
      call fftipost_rdma(ND,n,z,np)

      u_end=usrtime()
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
      t=(u_end-u_start)/(2*ITE+1)

      do i=0, np-1
         if(i.ne.myid) then
            do j=1, 2
               if( ierr.eq.0 ) stop 'failed to get right'
            end do
         end if
      end do

      if(myid.eq.ROOT)then
         wk(0)=t
         do i=1, np-1
*     $RREAD second parameter = wait(mili sec)
            call $RREAD(desc(2),-1,ierr)
            if( ierr.ne.1 ) stop 'failed to receive'
         end do
      else
         j=0
         ierr=$RRIGHT(ROOT,IKEY,j,-1,desc(j))
         call $RSETMSG(msg,8,WK)
         call $RWRITE(msg,desc(j),8*myid,ierr)
      end if

      if(myid.eq.ROOT)then
         write(*,200) NX,NY,NZ,2*ITE+1,np
         t=sum(wk(0:np-1))
         q=5*NTOT*log(dble(NTOT))/(log(2.d0)*1.d6)
         write(*,202) 'CPU  ',t,q/t
      end if

c     mili sec unit
      t=dble(sc_end-sc_start)/c_rate

      if(myid.eq.ROOT)then
         wk(0)=t
         do i=1, np-1
*     $RREAD second parameter = wait(mili sec)
            call $RREAD(desc(2),-1,ierr)
            if( ierr.ne.1 ) stop 'failed to receive'
         end do
      else
         call $RSETMSG(msg,8,WK)
c                      8*myid -> offset
         call $RWRITE(msg,rdesc(2,ROOT),8*myid,ierr)
      end if

      if(myid.eq.ROOT)then
         t=maxval(wk(0:np-1))/(2*ITE+1)
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
************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2, fftprep3 & fftprep4
*     w:    Work Area (size N must satisfies N >= NMAX*nunroll)
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
      subroutine fftnd_rdma(d,nmax,n,z,w,phase,bitrev)
      implicit none

      include 'rdma_h.f'

      integer nmax,d
      integer,dimension(d)::n
      integer,dimension(0:nmax-1,d)::bitrev
      complex(kind=8),dimension(0:*)::z,w
      complex(kind=8),dimension(0:nmax/2-1,d)::phase
      complex(kind=8),allocatable,dimension(:)::u
      integer i,j,jj,k,m,latency
      integer ierr


      if(d.le.1 .or. np.gt.t) then
         write(*,*) 'incompatible parameter'
         stop
      end if

      allocate(u(0:q-1))
      
      latency=8
      latency=min(l,latency)

      do i=0, l-1

         do j=0, np-1

            jj=ieor(myid,j)
            m=i*np+jj
            k=jj*l+i

            call MPI_Ssend_init
     $           (z(m*lchunk),lchunk,MPI_DOUBLE_COMPLEX,
     $           jj,i,COMM,REQS(i*np+j),ierr)
            call MPI_Recv_init
     $           (w(k*lchunk),lchunk,MPI_DOUBLE_COMPLEX,
     $           jj,i,COMM,REQR(i*np+j),ierr)

         end do

         call fftnd(d-1,nmax,n,z(i*q),u,phase,bitrev)

         call MPI_Startall(np,REQS(i*np),ierr)
         call MPI_Startall(np,REQR(i*np),ierr)

         if(i.ge.latency)then
            call MPI_Waitall (np,REQS((i-latency)*np),STAT,ierr)
            call MPI_Waitall (np,REQR((i-latency)*np),STAT,ierr)
         end if

      end do

      do i=l-latency, l-1
         call MPI_Waitall (np,REQS(i*np),STAT,ierr)
         call MPI_Waitall (np,REQR(i*np),STAT,ierr)
      end do

      call mfftnb(n(d),lchunk,w,phase(0,d))

      latency=10
      latency=min(l,latency)

      do i=0, l-1

         do j=0, np-1

            jj=ieor(myid,j)
            k=bitrev(jj*l+i,d)
            m=i*np+jj
            call MPI_Ssend_init
     $           (w(k*lchunk),lchunk,MPI_DOUBLE_COMPLEX,
     $           jj,i,COMM,REQS(i*np+j),ierr)
            call MPI_Recv_init
     $           (z(m*lchunk),lchunk,MPI_DOUBLE_COMPLEX,
     $           jj,i,COMM,REQR(i*np+j),ierr)

         end do

         call MPI_Startall(np,REQS(i*np),ierr)
         call MPI_Startall(np,REQR(i*np),ierr)

         if(i.ge.latency)then
            call MPI_Waitall (np,REQS((i-latency)*np),STAT,ierr)
            call MPI_Waitall (np,REQR((i-latency)*np),STAT,ierr)
         end if

      end do

      do i=l-latency, l-1
         call MPI_Waitall (np,REQS(i*np),STAT,ierr)
         call MPI_Waitall (np,REQR(i*np),STAT,ierr)
      end do

      deallocate(u)

      end
************************************************************************
*     post process for ifft
*     multiply 1/2**(n(1)+n(2)+...+n(nd))
*     dimension x(2**n(1),2**n(2),...,2**n(nd))
************************************************************************
      subroutine fftipost_rdma(nd,n,x,np)
*odirr NOALTCODE
      implicit none
      complex*16 x(*)
      real*8     f
      integer    j,k,nd,n(nd),nsum,np
      nsum=0
      do j=1, nd
         nsum=nsum+n(j)
      end do
      k=2**nsum
      f=1.d0/k
*vdir noaltcode
      do j=1,k/np
         x(j)=f*x(j)
      end do
      end

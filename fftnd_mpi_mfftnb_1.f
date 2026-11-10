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
      subroutine fftnd_mpi(d,nmax,n,z,w,phase,bitrev,myid,np)
      implicit none

      include 'mpif.h'

      integer nmax,d,myid,np
      integer,dimension(d)::n
      integer,dimension(0:nmax-1,d)::bitrev
      complex(kind=8),dimension(0:*)::z,w
      complex(kind=8),dimension(0:nmax/2-1,d)::phase
      complex(kind=8),allocatable,dimension(:)::u
      integer,allocatable,dimension(:)::REQR,REQS,STAT,COMM
      integer i,j,jj,k,m,l,q,t,nchunk,nw,latency
      integer ierr

      j=0
      do i=1, d-1
         j=j+n(i)
      end do
      q=2**j
      t=2**n(d)
      nchunk=q/np
      nw=nchunk*t
      l=t/np

      if(d.le.1 .or. np.gt.t) then
         write(*,*) 'incompatible parameter'
         stop
      end if

      allocate(REQR(0:t-1),REQS(0:t-1))
      allocate(STAT(0:MPI_STATUS_SIZE*t-1))
      allocate(u(0:q-1),COMM(0:np-1))
      
      COMM(0)=MPI_COMM_WORLD
c      do i=1, np-1
c         call MPI_Comm_dup(COMM(0),COMM(i),ierr)
c      end do

      latency=8
      latency=min(l,latency)

      do i=0, l-1

         do j=0, np-1

            jj=ieor(myid,j)
            m=i*np+jj
            k=jj*l+i

            call MPI_Ssend_init
     $           (z(m*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           jj,i,COMM,REQS(i*np+j),ierr)
            call MPI_Recv_init
     $           (w(k*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
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

      call mfftnb(n(d),nchunk,w,phase(0,d))

      latency=10
      latency=min(l,latency)

      do i=0, l-1

         do j=0, np-1

            jj=ieor(myid,j)
            k=bitrev(jj*l+i,d)
            m=i*np+jj
            call MPI_Ssend_init
     $           (w(k*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           jj,i,COMM,REQS(i*np+j),ierr)
            call MPI_Recv_init
     $           (z(m*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
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

c      do i=1, np-1
c         call MPI_Comm_free(COMM(i),ierr)
c      end do

      deallocate(u,COMM,REQR,REQS,STAT)

      end
************************************************************************
*     post process for ifft
*     multiply 1/2**(n(1)+n(2)+...+n(nd))
*     dimension x(2**n(1),2**n(2),...,2**n(nd))
************************************************************************
      subroutine fftipost_mpi(nd,n,x,np)
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

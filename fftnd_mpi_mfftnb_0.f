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
      integer,allocatable,dimension(:)::REQR,REQS,STAT
      integer i,j,k,m,l,q,t,nchunk,nw
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

      allocate(REQR(0:t-1),REQS(0:t-1),
     $     STAT(MPI_STATUS_SIZE*t),u(0:q-1))
      do i=0, l-1

         do j=0, myid-1
            m=i*np+j
            k=j*l+i
            call MPI_Ssend_init
     $           (z(m*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           j,i,MPI_COMM_WORLD,REQS(m-i),ierr)
            call MPI_Recv_init
     $           (w(k*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           j,i,MPI_COMM_WORLD,REQR(m-i),ierr)
         end do

         do j=myid+1, np-1
            m=i*np+j
            k=j*l+i
            call MPI_Ssend_init
     $           (z(m*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           j,i,MPI_COMM_WORLD,REQS(m-i-1),ierr)
            call MPI_Recv_init
     $           (w(k*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           j,i,MPI_COMM_WORLD,REQR(m-i-1),ierr)
         end do

         call fftnd(d-1,nmax,n,z(i*q),u,phase,bitrev)

         call MPI_Startall(np-1,REQS(i*(np-1)),ierr)
         call MPI_Startall(np-1,REQR(i*(np-1)),ierr)

         m=i*np+myid
         k=myid*l+i
         do j=0, nchunk-1
            w(k*nchunk+j)=z(m*nchunk+j)
         end do
      end do

      call MPI_Waitall(t-l,REQR,STAT,ierr)
      call MPI_Waitall(t-l,REQS,STAT,ierr)

      call mfftnb(n(d),nchunk,w,phase(0,d))

      do i=0, l-1

         do j=0, np-1

            k=bitrev(j*l+i,d)
            m=i*np+j
            call MPI_Ssend_init
     $           (w(k*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           j,i,MPI_COMM_WORLD,REQS(m),ierr)
            call MPI_Recv_init
     $           (z(m*nchunk),nchunk,MPI_DOUBLE_COMPLEX,
     $           j,i,MPI_COMM_WORLD,REQR(m),ierr)

         end do

         call MPI_Startall(np,REQS(i*np),ierr)
         call MPI_Startall(np,REQR(i*np),ierr)

      end do

      call MPI_Waitall(t,REQR,STAT,ierr)
      call MPI_Waitall(t,REQS,STAT,ierr)

      deallocate(REQR,REQS,STAT,u)
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

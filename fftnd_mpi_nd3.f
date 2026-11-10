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
      integer i,j,jj,k,m,l,q,t,lchunk,latency,ib,nb,lb,ir,npb
      integer ierr

      j=0
      do i=1, d-1
         j=j+n(i)
      end do
      q=2**j
      t=2**n(d)
      lchunk=q/np
      l=t/np

      if(d.le.1 .or. np.gt.t) then
         write(*,*) 'incompatible parameter'
         stop
      end if

c     tunable parameters
c
      nb=2**0
      latency=2
c
      nb=min(lchunk,nb)
      lb=lchunk/nb
      latency=min(l,latency)
      npb=np*nb

      allocate(REQR(0:t*nb-1),REQS(0:t*nb-1))
      allocate(STAT(MPI_STATUS_SIZE*max(t,npb)))
      allocate(u(0:q-1),COMM(0:np-1))
      
      COMM(0)=MPI_COMM_WORLD
c      do i=1, np-1
c         call MPI_Comm_dup(COMM(0),COMM(i),ierr)
c      end do


      do i=0, l-1

         do j=0, np-1

            jj=ieor(myid,j)

            do ib=0, nb-1

               m=i*npb+jj*nb+ib
               k=ib*t+jj*l+i
               ir=i*npb+ib

               call MPI_Ssend_init
     $              (z(m*lb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQS(ir+j*nb),ierr)
               call MPI_Recv_init
     $              (w(k*lb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQR(ir+j*nb),ierr)

               end do

         end do

         call fftnd(d-1,nmax,n,z(i*q),u,phase,bitrev)

         call MPI_Startall(npb,REQS(i*npb),ierr)
         call MPI_Startall(npb,REQR(i*npb),ierr)

         if(i.ge.latency)then
            call MPI_Waitall (npb,REQS((i-latency)*npb),STAT,ierr)
            call MPI_Waitall (npb,REQR((i-latency)*npb),STAT,ierr)
         end if

      end do

      do i=l-latency, l-1
         call MPI_Waitall (npb,REQS(i*npb),STAT,ierr)
         call MPI_Waitall (npb,REQR(i*npb),STAT,ierr)
      end do

c     tunable parameter
c
      latency=1
      latency=min(nb,latency)
c

      do ib=0, nb-1

         do i=0, l-1

            ir=ib*t+i

            do j=0, np-1

               jj=ieor(myid,j)
               k=ib*t+bitrev(jj*l+i,d)
               m=i*npb+jj*nb+ib

               call MPI_Ssend_init
     $              (w(k*lb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQS(ir+j*l),ierr)
               call MPI_Recv_init
     $              (z(m*lb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQR(ir+j*l),ierr)

            end do

         end do

         call mfftnb(n(d),lb,w(ib*t*lb),phase(0,d))

         call MPI_Startall(t,REQS(ib*t),ierr)
         call MPI_Startall(t,REQR(ib*t),ierr)

         if(ib.ge.latency)then
            call MPI_Waitall (t,REQS((ib-latency)*t),STAT,ierr)
            call MPI_Waitall (t,REQR((ib-latency)*t),STAT,ierr)
         end if

      end do

      do ib=nb-latency, nb-1
         call MPI_Waitall (t,REQS(ib*t),STAT,ierr)
         call MPI_Waitall (t,REQR(ib*t),STAT,ierr)
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

***
* Multi dimensional FFT module using zfft1d (MKL)
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
      integer nd,n(nd),nmax,bitrev(nmax,nd)
      complex(kind=8):: cdummy
      integer i,IWDIM,IWLEN
      complex(kind=8), pointer                   ::cw (:,:)
      complex(kind=8), allocatable, target, save ::cwb(:,:)
      common /ZFFTWRK/cw,IWDIM,IWLEN
      IWDIM=nd
      IWLEN=0
      do i=1, nd
         IWLEN=max(n(i),IWLEN)
      end do
      IWLEN=2**IWLEN
      if( allocated(cwb) ) then
         deallocate(cwb,STAT=i)
         if( i .ne. 0 ) stop 'Failed to deallocate cwb'
      end if
      allocate(cwb((IWLEN/2)*3,IWDIM),STAT=i)
      if( i .ne. 0 ) stop 'Failed to allocate cwb'
      cw=>cwb
      do i=1, nd
         call zfft1d(cdummy,2**n(i),0,cw(1,i))
      end do
      end
************************************************************************
*     N-dimensional fft routine
*     Multiple Call of JFC
*     z:  Input & Output (serialized x(i,j,k))
*     n:  dimension x(2**n(1),2**n(2),2**n(3))
*     phase: phase table prepared by fftprep2 & fftprep3
*     cwk: work area
*     bitrev: bit reversal table prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+n(3))
*     Written by S. Yamamoto
************************************************************************
      subroutine fftnd(nd,nmax,n,z,cwk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      integer i,j,p,q,r,idir,iside

      integer IWDIM,IWLEN
      complex(kind=8), pointer::cw(:,:)
      common /ZFFTWRK/cw,IWDIM,IWLEN

      iside=1
      r=0
      do i=1, nd
         r=r+n(i)
      end do
      r=2**r
      do i=1, nd
         p=2**n(i)
         q=r/p
         idir=int(dimag(phase(p/4,i)))
         if(iside.eq.1)then
            do j=0, q-1
               call zfft1d(z(j*p),p,idir,cw(1,i))
            end do
            if(q.ne.1)then
               call transp(p,q,z,cwk)
               iside=-iside
            end if
         else
            do j=0, q-1
               call zfft1d(cwk(j*p),p,idir,cw(1,i))
            end do
            if(q.ne.1)then
               call transp(p,q,cwk,z)
               iside=-iside
            end if
         end if
      end do
      if(iside.ne.1)then
         if(idir.eq.1)then
            do i=0, r-1
               z(i)=cwk(i)*r
            end do
         else
            do i=0, r-1
               z(i)=cwk(i)
            end do
         end if
      else
         if(idir.eq.1)then
            do i=0, r-1
               z(i)=z(i)*r
            end do
         end if
      end if
      end

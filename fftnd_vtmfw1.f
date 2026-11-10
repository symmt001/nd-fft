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
      subroutine fftnd(nd,nmax,n,z,wk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      real*8 z(*),wk(*),phase(nmax,nd)
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
         call mfftw(n(i),q,z,wk,phase(1,i),iside)
         if(iside.eq.1)then
            call transp(q,p,z,wk)
         else
            call transp(q,p,wk,z)
         end if
         iside=-iside
      end do
      if(iside.ne.1)then
         do i=1, 2*r
            z(i)=wk(i)
         end do
      end if
      end

************************************************************************
*     N-dimensional fft routine
*     Multiple Call of Decimation-In-Time Method 
*     z:  Input & Output (serialized z(i,j,k))
*     n:  dimension z(2**n(1),2**n(2),2**n(3))
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
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      complex*16 z(0:*),cwk(0:*),phase(nmax/2,nd)
      integer i,m,p,q,r,iside
      iside=1
      m=0
      do i=1, nd
         m=m+n(i)
      end do
      r=2**m
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         if(iside.eq.1)then
            call mfft(n(i),q,z,phase(1,i),bitrev(1,i))
            call transp(q,p,z,cwk)
         else
            call mfft(n(i),q,cwk,phase(1,i),bitrev(1,i))
            call transp(q,p,cwk,z)
         end if
         iside=-iside
      end do
      if(iside.ne.1)then
         do i=0, r-1
            z(i)=cwk(i)
         end do
      end if
      end

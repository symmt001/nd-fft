************************************************************************
*     N-dimensional fft routine
*     Multiple Call of Decimation-In-Time Method 
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
         if(iside.eq.1)then
            call mfftnb(n(i),q,z,phase(0,i))
            call transb(q,p,z,cwk,bitrev(0,i))
         else
            call mfftnb(n(i),q,cwk,phase(0,i))
            call transb(q,p,cwk,z,bitrev(0,i))
         end if
         iside=-iside
      end do
      if(iside.ne.1) then
         do i=0, r-1
            z(i)=cwk(i)
         end do
      end if
      end

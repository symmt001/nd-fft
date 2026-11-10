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
      integer i,j,k,m,p,q,r
      m=0
      do i=1, nd
         m=m+n(i)
      end do
      r=2**m
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         call mfftnb(n(i),q,z,phase(0,i))
         do j=0, p-1
            do k=0, q-1
               cwk(j*q+k)=z(bitrev(j,i)*q+k)
c     The next line is also O.K.
c              cwk(bitrev(j,i)*q+k)=z(j*q+k)
            end do
         end do
         call transp(q,p,cwk,z)
      end do
      end

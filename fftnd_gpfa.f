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
      integer nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      integer p,nsum

      integer ID, NN(3)
      common /GPFAWRK/ID,NN

      if(nd.ne.3) stop '3-dim only'
      nsum=0
      do p=1, nd
         NN(p)=2**n(p)
         nsum=nsum+n(p)
      end do
      ID=NN(1)
      end
************************************************************************
*     3-dimensional fft routine
*     Comparison against my own routine.
************************************************************************
      subroutine fftnd(nd,nmax,n,x,cwk,phase,bitrev)
      implicit none
      integer nmax,nd
      complex*16 x(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      integer bitrev(0:nmax-1,nd),n(nd)

      integer IS

      integer ID, NN(3)
      common /GPFAWRK/ID,NN

      if( dimag( phase(NN(1)/4,1) ) .gt. 0 ) then
         IS=-1
      else
         IS=1
      end if
      call GPF3D(x,ID,NN,IS)
      end

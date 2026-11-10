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
      integer nd,n(nd),nsum,nmax,p,bitrev(0:nmax-1,nd)
      complex*16 X

      integer MAXN
      parameter(MAXN=256)
      integer ISIG,IOPT,IORD,IERR, ID, NL,NM,NN,IWORK(MAXN**3)
      real*8  WL(4*MAXN+14),WM(4*MAXN*(MAXN+1)+14),WN(4*MAXN+14)
      common/MFFTWORK/ID, IOPT, IORD, NL,NM,NN, WL,WM,WN, IWORK

      if(nd.ne.3) stop '3-dim only'

      nsum=0
      do p=1, nd
         nsum=nsum+n(p)
      end do
      if (nsum.gt.30) stop 'Errror in FFTPREP1: too large n'

      ID=2**n(1)
      NL=ID
      NM=2**n(2)
      NN=2**n(3)

      IOPT=1
      ISIG=0
      IORD=1
      call C3FFT(X,ID,NL,NM,NN,WL,WM,WN,IOPT,ISIG,IORD,IWORK,IERR)
      if(IERR.ne.0) stop 'error in mfft : fftprep1'
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

      integer MAXN
      parameter(MAXN=256)
      integer ISIG,IOPT,IORD,IERR, ID, NL,NM,NN,IWORK(MAXN**3)
      real*8  WL(4*MAXN+14),WM(4*MAXN*(MAXN+1)+14),WN(4*MAXN+14)
      common/MFFTWORK/ID, IOPT, IORD, NL,NM,NN, WL,WM,WN, IWORK

      if( dimag( phase(NL/4,1) ) .gt. 0 ) then
         ISIG=1
      else
         ISIG=-1
      end if
      call C3FFT(x,ID,NL,NM,NN,WL,WM,WN,IOPT,ISIG,IORD,IWORK,IERR)
      if(IERR.ne.0) stop 'error in mfft'
      end

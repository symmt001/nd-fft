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
      integer i,nsum

      real*8  trigs((256+1+1)*2,3)
      common/GPFA2WRK/trigs
      if(nd.ne.3) stop '3-dim only'
      nsum=0
      do i=1, nd
         nsum=nsum+n(i)
         call SETGPFA(trigs(1,i),2**n(i))
      end do
      end
************************************************************************
*     3-dimensional fft routine
*     Comparison against my own routine.
************************************************************************
      subroutine fftnd(nd,nmax,n,z,w,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      real*8  z(0:*),w(0:*),phase(0:nmax-1,nd)
      integer i,m,p,q,r,iside
      integer IS

      real*8  trigs((128+1+1)*2,3)
      common/GPFA2WRK/trigs

      iside=1
      m=0
      do i=1, nd
         m=m+n(i)
      end do
      r=2**m
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         IS=int(phase(p/2+1,i))
         if(iside.eq.1)then
            call GPFA(z(0),z(1),trigs(1,i),2*q,2,p,q,IS)
            call transp(q,p,z,w)
         else
            call GPFA(w(0),w(1),trigs(1,i),2*q,2,p,q,IS)
            call transp(q,p,w,z)
         end if
         iside=-iside
      end do
      if(iside.ne.1)then
         do i=0, 2*r
            z(i)=w(i)
         end do
      end if
      end

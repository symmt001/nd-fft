***
* Multi dimensional FFT module for Hitachi SR-80000
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
      integer nd,n(nd),nmax,bitrev(nmax,nd),kd
      parameter(kd=kind(1.d0))
      complex(kind=kd):: cdummy
      integer i,ier,iopt(2),NWKW
      complex(kind=kd),pointer ::wk1(:),wk2(:)
      complex(kind=kd),allocatable,target,save::cwk1(:),cwk2(:)
      common /ZFFTWRK/wk1,wk2,NWKW
      if(nd.ne.3) stop'parameter error: hzft6m'
      NWKW=1
      if( allocated(cwk1) ) then
         deallocate(cwk1,STAT=i)
         if( i .ne. 0 ) stop 'Failed to deallocate cwk1'
      end if
      if( allocated(cwk2) ) then
         deallocate(cwk2,STAT=i)
         if( i .ne. 0 ) stop 'Failed to deallocate cwk2'
      end if
      allocate(cwk1(2**max(n(1),n(2),n(3))),STAT=i)
      if( i .ne. 0 ) stop 'Failed to allocate cwk1'
      allocate(cwk2(2**(n(1)+n(2)+n(3))
     $     +NWKW*2**(max(n(1),n(2))+n(3))),STAT=i)
      if( i .ne. 0 ) stop 'Failed to allocate cwk2'
      wk1=>cwk1
      wk2=>cwk2
      iopt(1)=1 ; iopt(2)=0
      call HZFT6M(cdummy,2**n(1),2**n(2),2**n(1),2**n(2),2**n(3),
     $     iopt,wk1,wk2,NWKW,ier)
      if(ier.ne.0) stop 'error in init HZFT6M'
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
      integer idir,kd
      parameter(kd=kind(1.d0))
      integer iopt(2),ier,NWKW
      complex(kind=kd),pointer ::wk1(:),wk2(:)
      common /ZFFTWRK/wk1,wk2,NWKW

      idir=int(dimag(phase(2**(n(1)-2),1)))
      iopt(1)=(idir+1)/2+1
      iopt(2)=2 
      call HZFT6M(z,2**n(1),2**n(2),2**n(1),2**n(2),2**n(3),
     $     iopt,wk1,wk2,NWKW,ier)
      end

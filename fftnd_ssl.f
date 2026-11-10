      subroutine fftprep1(nd,nmax,n,bitrev)
      implicit none
      integer nd,n(nd),nmax,bitrev(nmax,nd)
      end
************************************************************************
*     N-dimensional fft routine
*     Multiple Call of JFC
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
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      real*8  z(*),wk(*),phase(0:nmax-1,nd)
      integer i,iw,icon,NMSSL
      parameter(NMSSL=10)
      integer nssl(NMSSL),isn(NMSSL)
      if(nd.gt.NMSSL)stop'nd must be smaller than or equal to NMSSL'
      iw=1
      do i=1, nd
         nssl(i)=2**n(i)
         isn(i)=-int(phase(nssl(i)/2+1,i))
         iw=iw*nssl(i)
      end do
      do i=1, iw
         wk(i)   =z(2*i-1)
         wk(iw+i)=z(2*i)
      end do
      icon=0
      call DVMCFT(wk(1),wk(iw+1),nssl,nd,isn,z,2*iw,icon)
      if (icon.ne.0) then
         write(*,*) iw,isn
         write(*,*) 'ERROR: DVMCFT CODE=',icon
         stop
      end if
      do i=1, iw
         z(2*i-1)=wk(i)
         z(2*i)  =wk(iw+i)
      end do
      end

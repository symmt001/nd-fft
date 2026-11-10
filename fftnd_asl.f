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
      integer i,j,k,m

      integer IWPMAX, IWNMAX, IWDIM
c     Set suitable work area size by your own hand.
      parameter( IWPMAX=10, IWNMAX=2**IWPMAX ,IWDIM=3 )
      complex*16 cw(IWPMAX*IWNMAX/2,IWDIM)
      integer iw(2*IWNMAX+1,IWDIM)
      common /ASL_WORK/cw,iw
      if(nd.gt.IWDIM) stop 'nd cannot be larger than IWDIM'
      m=0
      do i=1, nd
         m=m+n(i)
      end do
      if (m.gt.30) stop 'Errror in FFTPREP1: too large n'
      m=2**m
      do i=1, nd
         k=2**n(i)
         j=0
         call JFCSSI(k,m/k,iw(1,i),cw(1,i),j)
         if (j.ne.0) then
            write(*,*) 'ERROR: JFCSSI CODE=',j
            stop 
         end if
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
      integer i,p,q,r,idir,ierr,iside

      integer IWPMAX, IWNMAX, IWDIM
c     Set suitable work area size by your own hand.
      parameter( IWPMAX=10, IWNMAX=2**IWPMAX ,IWDIM=3 )
      complex*16 cw(IWPMAX*IWNMAX/2,IWDIM)
      integer iw(2*IWNMAX+1,IWDIM)
      common /ASL_WORK/cw,iw

      iside=1
      r=0
      do i=1, nd
         r=r+n(i)
      end do
      r=2**r
      ierr=0
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         idir=-int(dimag(phase(p/4,i)))
         if(iside.eq.1)then
            call JFCSSF(p,q,z,q,1,idir,iw(1,i),cw(1,i),cwk,ierr)
            if (ierr.ne.0) then
               write(*,*) p,q,idir
               write(*,*) 'ERROR: JFCSSF CODE=',ierr
               stop 
            end if
            call transp(q,p,z,cwk)
         else
            call JFCSSF(p,q,cwk,q,1,idir,iw(1,i),cw(1,i),z,ierr)
            if (ierr.ne.0) then
               write(*,*) p,q,idir
               write(*,*) 'ERROR: JFCSSF CODE=',ierr
               stop 
            end if
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

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
      integer nd,n(nd),nmax,bitrev(nmax,nd)
      complex(kind=8),allocatable:: cdummy(:)
      integer i,IWDIM,IWLEN,IWP,ier,iopt(2)
      integer, pointer :: iwk(:,:)
      integer, allocatable, target, save::ciwk(:,:)
      complex(kind=8),pointer ::wk1(:,:),wk2(:,:)
      complex(kind=8),allocatable,target,save::cwk1(:,:),cwk2(:,:)
      common /ZFFTWRK/iwk,wk1,wk2,IWDIM,IWLEN,IWP
      IWDIM=nd
      IWP=0
      do i=1, nd
         IWP=max(n(i),IWP)
      end do
      IWLEN=2**IWP
      if( allocated(cwk1) ) then
         deallocate(cwk1,STAT=i)
         if( i .ne. 0 ) stop 'Failed to deallocate cwk1'
      end if
      if( allocated(cwk2) ) then
         deallocate(cwk2,STAT=i)
         if( i .ne. 0 ) stop 'Failed to deallocate cwk2'
      end if
      if( allocated(ciwk) ) then
         deallocate(ciwk,STAT=i)
         if( i .ne. 0 ) stop 'Failed to deallocate ciwk'
      end if
      allocate(cwk1(IWLEN*3*(IWP/2-1)/2+1,IWDIM),STAT=i)
      if( i .ne. 0 ) stop 'Failed to allocate cwk1'
      allocate(cwk2(IWLEN,IWDIM),STAT=i)
      if( i .ne. 0 ) stop 'Failed to allocate cwk1'
      allocate(ciwk(IWLEN*((IWP+1)/2-2)/4+4,IWDIM),STAT=i)
      if( i .ne. 0 ) stop 'Failed to allocate ciwk'
      wk1=>cwk1
      wk2=>cwk2
      iwk=>ciwk
      allocate(cdummy(2*IWLEN))
      cdummy=(0.d0,0.d0)
      iopt(1)=1 ; iopt(2)=1
      do i=1, nd
         call HZFT4M(cdummy,2**n(i),n(i),iopt,cdummy(IWLEN+1),
     .        wk1(1,i),wk2(1,i),iwk(1,i),ier)
         if(ier.ne.0) stop 'error in init HZFT4M'
      end do
      do i=1, IWLEN
         if(cdummy(i) .ne. (0.d0,0.d0)) stop 'error'
      end do
      deallocate(cdummy)
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
      integer i,j,p,q,r,idir,iside

      integer IWDIM,IWLEN,IWP,iopt(2),ier
      integer, pointer :: iwk(:,:)
      complex(kind=8),pointer ::wk1(:,:),wk2(:,:)
      common /ZFFTWRK/iwk,wk1,wk2,IWDIM,IWLEN,IWP

      iside=1
      r=0
      do i=1, nd
         r=r+n(i)
      end do
      r=2**r
      do i=1, nd
         p=2**n(i)
         q=r/p
         idir=int(dimag(phase(p/4,i)))
         iopt(1)=(idir+1)/2+1
         iopt(2)=2 
         if(iside.eq.1)then
            do j=0, q-1
               call HZFT4M(z(j*p),2**n(i),n(i),iopt,cwk(j*p),
     .              wk1(1,i),wk2(1,i),iwk(1,i),ier)
            end do
            if(q.ne.1)then
               call transp(p,q,cwk,z)
            else
               iside=-iside
            end if
         else
            do j=0, q-1
               call HZFT4M(cwk(j*p),2**n(i),n(i),iopt,z(j*p),
     .              wk1(1,i),wk2(1,i),iwk(1,i),ier)
            end do
            if(q.ne.1)then
               call transp(p,q,z,cwk)
            else
               iside=-iside
            end if
         end if
      end do
      if(iside.ne.1)then
         if(idir.eq.1)then
            do i=0, r-1
               z(i)=cwk(i)*r
            end do
         else
            do i=0, r-1
               z(i)=cwk(i)
            end do
         end if
      else
         if(idir.eq.1)then
            do i=0, r-1
               z(i)=z(i)*r
            end do
         end if
      end if
      end

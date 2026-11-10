************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     x:      Input & Output (serialized x(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2 & fftprep3
*     wk:    Work Area
*     bitrev: Bit Reversal Table Prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+...+n(nd))
*     Written by S. Yamamoto
*     
*     Comments:
*     *odir       directive to the optimizer on SX-3   (NEC)
*     *vdir       directive to the compiler  on SX-3   (NEC)
*     *VOPTION    directive to the compiler  on S-3800 (HITACHI)
*     *VOCL       directive to the compiler  on VPP    (FUJITSU)
************************************************************************
      subroutine fftnd(nd,nmax,n,x,wk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd
      integer bitrev(0:nmax-1,nd),n(nd)
      real*8 x(0:*),wk(0:*),phase(0:nmax-1,nd)
      integer axis,i,j,k,l,m,p,q,iside
      real*8 x0,x1,phr,phi
      iside=1
      m=0
      do axis=1, nd
         m=m+n(axis)
      end do
      m=2**m
      do axis=1, nd
         l=2**n(axis)
         q=m/l
         p=l
*     bit reversal
         if(iside.eq.1)then
            call rearr(l,q,x,wk,bitrev(0,axis))
         else
            call rearr(l,q,wk,x,bitrev(0,axis))
         end if
         iside=-iside
*     Loop Unrolling with Stride 2
         do i=1, n(axis)
* actually, next do loop is *POPTION INDEP, but
* inserting POPTION directive causes discouraging result
            do k=0, l-1, p
               phr=phase(k,axis)
               phi=phase(k+1,axis)
               if(iside.eq.1)then
*VOCL LOOP,NOVREC(WK,X)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1, 2
                     x0=x(2*j+2)*phr-x(2*j+3)*phi
                     x1=x(2*j+2)*phi+x(2*j+3)*phr
                     wk(j)    =x(2*j)  +x0
                     wk(j+1)  =x(2*j+1)+x1
                     wk(j+m)  =x(2*j)  -x0
                     wk(j+m+1)=x(2*j+1)-x1
                  end do
               else
*VOCL LOOP,NOVREC(WK,X)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1, 2
                     x0=wk(2*j+2)*phr-wk(2*j+3)*phi
                     x1=wk(2*j+2)*phi+wk(2*j+3)*phr
                     x(j)    =wk(2*j)  +x0
                     x(j+1)  =wk(2*j+1)+x1
                     x(j+m)  =wk(2*j)  -x0
                     x(j+m+1)=wk(2*j+1)-x1
                  end do
               end if
            end do
            iside=-iside
            p=p/2
         end do
      end do
      if(iside.ne.1)then
         do i=0, 2*m-1
            x(i)=wk(i)
         end do
      end if
      end
************************************************************************
*     rearrange complex data as follows
*     src(bitrev(p)+p,q)->dst(p,q)
*     Caution! p must be power of 2.
*     Caution! Not compatible with rearr_0.f
*     This routine must be used with fftprep1_1.f
************************************************************************
      subroutine rearr(p,q,src,dst,bitrev)
      implicit none
      integer p,q,i
      integer bitrev(0:p-1)
      complex*16 src(0:p*q-1),dst(0:p*q-1)
c     maximizing vector length for vector machines
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*VOPTION INDEP
*POPTION INDEP
      do i=0, p*q-1
         dst(i)=src(bitrev(iand(i,p-1))+i)
      end do
      end
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
      integer nd,nmax
      integer n(nd),bitrev(0:nmax-1,nd)
      integer i,j,k,l,p,q
      do i=1, nd
         k=2**n(i)
         do j=0, k-1
*     calculating displacement
            bitrev(j,i)=-j
         end do
         p=1
         q=k/2
         do k=1, n(i)
*     tuning for vector machine
            if(q.gt.p)then
               do l=1, 2*p-1, 2
*vdir noloopchg
*SOPTION UNROLL(4)
                  do j=0, q-1
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            else
               do j=0, q-1
*vdir noloopchg
*SOPTION UNROLL(4)
                  do l=1, 2*p-1, 2
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            end if
*     p*q=k/2
            p=p*2
            q=q/2
         end do
      end do
      end
************************************************************************
*     preparation for fft
*     set phase table
*     nd:    dimension n(nd)
*     nmax = 2**max(n)
*     phase(0:(nmax/2-1),nd)
************************************************************************
      subroutine fftprep2(nd,nmax,n,phase)
*odirr NOALTCODE
      implicit none
      integer i,j,k,m,nd,n(nd),nmax
      real*8  phase(0:nmax-1,nd)
      real*8  delta,PIF
c     PIF=PI/4
      PIF=atan(1.d0)
      m=1
c     Find max{n}
      do i=2, nd
         if(n(i).gt.n(m)) m=i
      end do
      i=m
c     Calculate phase table for the most large n(i).
      if( n(i)-3 .ge. 0 ) then
         k=2**(n(i)-3)
         delta=PIF/k
         do j=0,k
            phase(2*j  ,i)=cos(j*delta)
            phase(2*j+1,i)=sin(j*delta)
         end do
         m=k*2
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=k+1,m
            phase(2*j  ,i)=phase(2*(m-j)+1,i)
            phase(2*j+1,i)=phase(2*(m-j)  ,i)
         end do
         k=k*4
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=m+1,k-1
            phase(2*j  ,i)=-phase(2*(k-j)  ,i)
            phase(2*j+1,i)= phase(2*(k-j)+1,i)
         end do
      else if ( n(i) .ge. 1 ) then
         phase(0,i)=1.d0
         phase(1,i)=0.d0
           if ( n(i) .eq. 2 ) then
              phase(2,i)=0.d0
              phase(3,i)=1.d0
           end if
      end if
C     Copy phase table to the others.
      k=i
      do i=1, nd
         if(i.ne.k)then
            m=2**(n(k)-n(i))
            do j=0, 2**(n(i)-1)-1
               phase(2*j  ,i)=phase(2*j*m  ,k)
               phase(2*j+1,i)=phase(2*j*m+1,k)
            end do
         end if
      end do
      end
************************************************************************
*     preparation for fft
*     conjugate phase table
*     phase(0:(nmax/2-1),nd)
************************************************************************
      subroutine fftprep3(nd,nmax,phase)
*odirr NOALTCODE
      implicit none
      integer j,nd,nmax
      real*8 phase(nd*nmax)
*vdir noaltcode
      do j=2, nd*nmax, 2
         phase(j)=-phase(j)
      end do
      end
************************************************************************
*     dummy for compatibility problem
************************************************************************
      subroutine fftprep4(nd,nmax,n,phase,bitrev)
      implicit none
      integer nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      complex*16 phase(0:nmax/2-1,nd)
      end
************************************************************************
*     post process for ifft
*     multiply 1/2**(n(1)+n(2)+...+n(nd))
*     dimension x(2**n(1),2**n(2),...,2**n(nd))
************************************************************************
      subroutine fftipost(nd,n,x)
*odirr NOALTCODE
      implicit none
      complex*16 x(*)
      real*8     f
      integer    j,k,nd,n(nd),nsum
      nsum=0
      do j=1, nd
         nsum=nsum+n(j)
      end do
      k=2**nsum
      f=1.d0/k
*vdir noaltcode
      do j=1,k
         x(j)=f*x(j)
      end do
      end

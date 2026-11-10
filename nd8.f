************************************************************************
*     N-dimensional fft routine
*     Multiple Call of Decimation-In-Time Method 
*     z:  Input & Output (serialized z(i,j,k))
*     n:  dimension z(2**n(1),2**n(2),2**n(3))
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
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      complex*16 z(0:*),cwk(0:*),phase(nmax/2,nd)
      integer i,m,p,q,r,iside
      iside=1
      m=0
      do i=1, nd
         m=m+n(i)
      end do
      r=2**m
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         if(iside.eq.1)then
            call mfft(n(i),q,z,phase(1,i),bitrev(1,i))
            call transp(q,p,z,cwk)
         else
            call mfft(n(i),q,cwk,phase(1,i),bitrev(1,i))
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
************************************************************************
*     Multiple FFT
************************************************************************
      subroutine mfft(n,m,z,phase,bitrev)
      implicit none
      integer i,j,j2,j3,n,p,pm,q,m
      complex*16 z(0:m*(2**n)-1),phase(0:2**n/2-1),x,y
      integer bitrev(0:2**n-1)
      q=2**n
*     bit reversal
      do j=0,q-1
         i=bitrev(j)
         if(i.gt.0) then
            i=i*m
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
            do j2=j*m, (j+1)*m-1
               x=z(i+j2)
               z(i+j2)=z(j2)
               z(j2)=x
            end do
         end if
      end do
      p=1
      q=q/2
      do i=1, n
         pm=p*m
         do j2=0, 2**n-1, 2*p
            do j=0, p-1
               x=phase(j*q)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
               do j3=(j2+j)*m, (j2+j+1)*m-1
                  y=z(pm+j3)*x
                  z(pm+j3)=z(j3)-y
                  z(   j3)=z(j3)+y
               end do
            end do
         end do
         p=p*2
         q=q/2
      end do
      end
************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     This routine is designed especially for the case
*     that both q and p equal to 2**n. But it works on the other case.
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k
      complex*16 dst(0:q*p-1),src(0:q*p-1)
c     Avoiding Bank Conflict
      if( q.ge.p ) then
*POPTION INDEP
         do k=0, q-p
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1
c$$$               dst((k+j)*p+j)=src(j*q+(k+j))
               dst(k*p+(p+1)*j)=src((q+1)*j+k)
            end do
         end do
*POPTION INDEP
         do k=1, p-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
            end do
         end do
      else
*POPTION INDEP
         do k=0, p-q
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, q-1
c$$$               dst(j*p+(k+j))=src((k+j)*q+j)
               dst((p+1)*j+k)=src(k*q+(q+1)*j)
            end do
         end do
*POPTION INDEP
         do k=1, q-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
            end do
         end do
      end if
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

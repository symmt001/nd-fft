************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension z(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2 & fftprep3
*     cwk:    Work Area
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
      subroutine fftnd(nd,nmax,n,z,cwk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd),x,y
      integer bitrev(0:nmax-1,nd),n(nd),axis,i,j,j2,k,l,q,r
      j=0
      do 10 axis=1, nd
         j=j+n(axis)
 10   continue
      k=2**(j-1)
      do 20 axis=1, nd
         l=2**(n(axis)-1)
         q=k/l
*     bit reversal
         i=2*l-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
         do 30 j=0, 2*k-1
            cwk(j)=z(j+bitrev(iand(j,i),axis))
 30      continue
         r=l
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do 120 j=0, r*q-1
            z(  j)=cwk(2*j)+cwk(2*j+1)
            z(k+j)=cwk(2*j)-cwk(2*j+1)
 120      continue
         r=r/2
*     Loop Unrolling with Stride 2
*VOCL LOOP,NOVDOPT
*vdir noaltcode
         do 40 i=1, (n(axis)-1)/2
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*POPTION INDEP
            do 50 j2=0, l-1, r
               x=phase(j2,axis)
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
               do 60 j=j2*q, (j2+r)*q-1
c$$$                  y=z(2*j+1)*x
c$$$                  cwk(  j)=z(2*j)+y
c$$$                  cwk(k+j)=z(2*j)-y
c     better on SR8000
                  cwk(  j)=z(2*j)+z(2*j+1)*x
                  cwk(k+j)=z(2*j)-z(2*j+1)*x
 60            continue
 50         continue
            r=r/2
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*POPTION INDEP
            do 70 j2=0, l-1, r
               x=phase(j2,axis)
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
               do 80 j=j2*q, (j2+r)*q-1
c$$$                  y=cwk(2*j+1)*x
c$$$                  z(  j)=cwk(2*j)+y
c$$$                  z(k+j)=cwk(2*j)-y
                  z(  j)=cwk(2*j)+cwk(2*j+1)*x
                  z(k+j)=cwk(2*j)-cwk(2*j+1)*x
 80            continue
 70         continue
            r=r/2
 40      continue
         if (mod(n(axis),2).eq.0) then
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*POPTION INDEP
            do 100 j2=0, l-1
               x=phase(j2,axis)
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
               do 110 j=j2*q, (j2+1)*q-1
c$$$                  y=z(2*j+1)*x
c$$$                  cwk(  j)=z(2*j)+y
c$$$                  cwk(k+j)=z(2*j)-y
                  cwk(  j)=z(2*j)+z(2*j+1)*x
                  cwk(k+j)=z(2*j)-z(2*j+1)*x
 110           continue
 100        continue
*VOCL LOOP,NOVDOPT
*vdir noaltcode
            do 90 j=0, 2*k-1
               z(j)=cwk(j)
 90         continue
         else
         end if
 20   continue
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

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
      integer    nmax,nd
      integer    bitrev(0:nmax-1,nd),n(nd)
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd),ph
      integer    i,j,k,l,m,p,q,axis,iside
c      complex*16 x0,x1
*     "iside" indicates on which site the data is located.
      iside=1
      m=0
      do axis=1, nd
         m=m+n(axis)
      end do
      m=2**(m-1)
      do axis=1, nd
         l=2**(n(axis)-1)
         p=l
         q=m/l
*     bit-reversal
         if(iside.eq.1)then
            call rearr(2*l,q,z,cwk,bitrev(0,axis))
         else
            call rearr(2*l,q,cwk,z,bitrev(0,axis))
         end if
*     exchange z and cwk
         iside=-iside
         do i=1, n(axis)
* actually, next do loop is *POPTION INDEP, but
* inserting POPTION directive causes discouraging result
            do k=0, l-1, p
               ph=phase(k,axis)
*     Doing radix 2 DIT & transpose at the sametime
               if(iside.eq.1)then
CDIR$ IVDEP
CDIR$ VECTOR ALWAYS
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1
c$$$                     x0=z(2*j)
c$$$                     x1=z(2*j+1)*ph
c$$$                     cwk(  j)=x0+x1
c$$$                     cwk(m+j)=x0-x1
c     better on SR-8000
                     cwk(  j)=z(2*j)+z(2*j+1)*ph
                     cwk(m+j)=z(2*j)-z(2*j+1)*ph
                  end do
               else
CDIR$ IVDEP
CDIR$ VECTOR ALWAYS
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1
c$$$                     x0=cwk(2*j)
c$$$                     x1=cwk(2*j+1)*ph
c$$$                     z(  j)=x0+x1
c$$$                     z(m+j)=x0-x1
c     better on SR-8000
                     z(  j)=cwk(2*j)+cwk(2*j+1)*ph
                     z(m+j)=cwk(2*j)-cwk(2*j+1)*ph
                  end do
               end if
            end do
            p=p/2
*     exchange z and cwk
            iside=-iside
         end do
      end do
      if (iside.ne.1) then
CDIR$ VECTOR ALWAYS
         do i=0, 2*m-1
            z(i)=cwk(i)
         end do
      end if
      end
************************************************************************
*     rearrange complex data as follows
*     src(bitrev(p),q)->dst(p,q)
*     Caution! Not compatible with rearr_1.f
*     This routine must be used with fftprep1_0.f
************************************************************************
      subroutine rearr(p,q,src,dst,bitrev)
      implicit none
      integer p,q,i,j
      integer bitrev(0:p-1)
      complex*16 src(0:p-1,q),dst(0:p-1,q)
c$$$      integer p2,p3,p4,p5,p6,p7,p8,k
c$$$      if (p.lt.8) then
c     Original Scheme
*POPTION INDEP
         do j=1, q
CDIR$ IVDEP
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*VOPTION INDEP
*POPTION INDEP
            do i=0, p-1
               dst(i,j)=src(bitrev(i),j)
            end do
         end do
* Not Good
c$$$      else
c$$$         p8=p/8
c$$$         p4=p8*2
c$$$         p2=p4*2
c$$$         p3=p2+p4
c$$$         p5=p2+p8
c$$$         p6=p4+p8
c$$$         p7=p3+p8
c$$$c unrolling
c$$$*POPTION INDEP
c$$$*SOPTION NOUNROLL
c$$$         do j=1, q
c$$$*VOCL LOOP,NOVREC(SRC,DST)
c$$$*VOCL LOOP,NOVDOPT
c$$$*vdir noassume
c$$$*vdir noaltcode
c$$$*VOPTION INDEP
c$$$*POPTION INDEP
c$$$            do i=0, p8-1
c$$$               k=bitrev(i)
c$$$               dst(i,j)=src(k,j)
c$$$               dst(p2+i,j)=src(k+1,j)
c$$$               dst(p4+i,j)=src(k+2,j)
c$$$               dst(p3+i,j)=src(k+3,j)
c$$$               dst(p8+i,j)=src(k+4,j)
c$$$               dst(p5+i,j)=src(k+5,j)
c$$$               dst(p6+i,j)=src(k+6,j)
c$$$               dst(p7+i,j)=src(k+7,j)
c$$$            end do
c$$$         end do
c$$$      end if
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
            bitrev(j,i)=0
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

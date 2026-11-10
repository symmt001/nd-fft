************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension z(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2, fftprep3 & fftprep4
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
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      integer i,q,r,s,t
      integer WSIZE,nline
c     a naive estimation
c     primary cache(byte) < WSIZE < secondary cache(byte)
      parameter(WSIZE=128*1024/16)
      s=0
      do i=1, nd
         s=s+n(i)
      end do
      s=2**s
      r=1
      do i=1, nd
         t=2**n(i)
         q=r*t
         if(r.gt.1)then
            nline=min(r,int(WSIZE/t))
            nline=max(r,1)
            call sv_sub1(n(i),q,r,s,t,nline,
     $           z,cwk,phase(0,i),bitrev(0,i))
         else
            nline=min(s/t,int(WSIZE/t))
            nline=max(r,1)
            call sv_sub2(n(i),s,t,nline,
     $           z,cwk,phase(0,i),bitrev(0,i))
         end if
         r=q
      end do
      end
      subroutine sv_sub1(n,q,r,s,t,nunroll,z,cwk,phase,bitrev)
      implicit none
      integer j,k,l,m,n,q,r,s,t,bitrev(0:*)
      integer nunroll
      complex*16 z(0:*),cwk(0:*),phase(0:*)
      do k=0, s-1, q
         do j=k, k+r-1, nunroll
            do m=0, t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do l=0, nunroll-1
                  cwk(m*nunroll+l)=z(j+m*r+l)
               end do
            end do
            call mfftnb(n,nunroll,cwk,phase)
            do m=0, t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do l=0, nunroll-1
c                  z(j+m*r+l)=cwk(bitrev(m)*nunroll+l)
                  z(j+bitrev(m)*r+l)=cwk(m*nunroll+l)
               end do
            end do
         end do
      end do
      end
      subroutine sv_sub2(n,s,t,nunroll,z,cwk,phase,bitrev)
      implicit none
      integer j,k,m,n,s,t,bitrev(0:*)
      integer nunroll
      complex*16 z(0:*),cwk(0:*),phase(0:*)
      do k=0, s-1, nunroll*t
         do m=0, nunroll-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, t-1
               cwk(m+j*nunroll)=z(k+m*t+j)
            end do
         end do
         call mfftnb(n,nunroll,cwk,phase)
         do j=0,t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do m=0, nunroll-1
               z(k+m*t+bitrev(j))=cwk(j*nunroll+m)
            end do
         end do
      end do
      end
************************************************************************
*     multiple fft routine without bit-reversal
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors (bit-reversal order)
************************************************************************
      subroutine mfftnb(n,m,z,phase)
*odirr NOALTCODE
      implicit none
      real*8 z(0:*),phase(0:*),iflag
      real*8 ur,ui,vr,vi,xr,xi,yr,yi,wr,wi,s1,c1,s2,c2
      integer i,j,k,n,m,p,q,q2,q3
      p=2
      q=(2**(n-1))*m
      if(n.ge.2) iflag=phase(3)
*     Loop Unrolling, Stride=2
      do i=1, n-1, 2
         q2=q+q
         q3=q2+q
*     Loop Unrolling, Stride=2
         do j=0, p-1, 2
            c1=phase(j)
            s1=phase(j+1)
            c2=phase(2*j)
            s2=phase(2*j+1)
*     phase(2*j+2)=-phase(2*j+1)*phase(3)=-s2*phase(3)
*     phase(2*j+3)= phase(2*j)  *phase(3)= c2*phase(3)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=q2*j, q2*j+q-1, 2
               wr=z(q2+k)*c1-z(q2+k+1)*s1
               wi=z(q2+k)*s1+z(q2+k+1)*c1
               xr=z(  k)  +wr
               ur=z(  k)  -wr
               xi=z(  k+1)+wi
               ui=z(  k+1)-wi
*
               wr=z(q3+k)*c1-z(q3+k+1)*s1
               wi=z(q3+k)*s1+z(q3+k+1)*c1
               yr=z(q+k)  +wr
               vr=z(q+k)  -wr
               yi=z(q+k+1)+wi
               vi=z(q+k+1)-wi
*
               wr       =yr*c2-yi*s2
               wi       =yr*s2+yi*c2
               z(  k)   =xr+wr
               z(q+k)   =xr-wr
               z(  k+1) =xi+wi
               z(q+k+1) =xi-wi
*
               wr       =(vr*s2+vi*c2)*iflag
               wi       =(vr*c2-vi*s2)*iflag
               z(q2+k)  =ur-wr
               z(q3+k)  =ur+wr
               z(q2+k+1)=ui+wi
               z(q3+k+1)=ui-wi
            end do
         end do
         p=p*4
         q=q/4
      end do
      if(i.eq.n)then
         q=m*2
         q2=q+q
         q3=q2+q
         do j=0, 2**n-1, 4
            c1=phase(j)
            s1=phase(j+1)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=j*q, j*q+q-1, 2
               xr=z(   k)
               xi=z(   k+1)
c
               wr=z(q +k)
               wi=z(q +k+1)
               ur=wr*c1-wi*s1
               ui=wr*s1+wi*c1
               yr=z(q2+k)
               yi=z(q2+k+1)
c
               wr=z(q3+k)
               wi=z(q3+k+1)
               vr=( wr*s1+wi*c1)*iflag
               vi=(-wr*c1+wi*s1)*iflag
               z(   k)  =xr+ur
               z(   k+1)=xi+ui
               z(q +k)  =xr-ur
               z(q +k+1)=xi-ui
               z(q2+k)  =yr-vr
               z(q2+k+1)=yi-vi
               z(q3+k)  =yr+vr
               z(q3+k+1)=yi+vi
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
*     preparation for fft
*     bitreversal phase table
************************************************************************
      subroutine fftprep4(nd,nmax,n,phase,bitrev)
      implicit none
      integer i,j,k,m,nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      complex*16 phase(0:nmax/2-1,nd),c
c      write(*,'(2F10.4)') (phase(p,1),p=0,15)
      do i=1, nd
         k=2**(n(i)-1)
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=0, k-1
            m=bitrev(j*2,i)
            if(m.gt.j) then
               c=phase(m,i)
               phase(m,i)=phase(j,i)
               phase(j,i)=c
            end if
         end do
      end do
c      write(*,'(I4,2F10.4)') (bitrev(i*2,1),phase(i,1),i=0,15)
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

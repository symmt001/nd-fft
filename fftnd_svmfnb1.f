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

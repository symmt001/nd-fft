************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2, fftprep3 & fftprep4
*     cwk:    Work Area (size N must satisfies N >= NMAX*nunroll)
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
c     Fortran90
      recursive subroutine fftnd(d,nmax,n,z,cwk,phase,bitrev)
c      subroutine fftnd(d,nmax,n,z,cwk,phase,bitrev)
      implicit none
      integer nmax,d,n(d),bitrev(0:nmax-1,d)
      integer j,l,m,q,s,t,nunroll
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,d)
      integer WSIZE
c     WSIZE equals to the half of window size
      parameter(WSIZE=128*1024/16)
      t=2**n(d)
      if(d.eq.1)then
         call mfftnb(n(d),1,z,phase(0,d))
         call btrv(t,WSIZE,z,bitrev(0,d))
         return
      end if
      s=0
      do j=1, d-1
         s=s+n(j)
      end do
      q=2**s
      s=t*q
      do j=0, s-1, q
         call fftnd(d-1,nmax,n,z(j),cwk,phase,bitrev)
      end do
      if(s.gt.WSIZE)then
         if(t.le.WSIZE)then
            nunroll=WSIZE/t
c     loop transform for efficient use of cache
            do j=0, q-1, nunroll
               do m=0, t-1
c     for efficient use of cache line (in other word burst transfer)
                  do l=0, nunroll-1
                     cwk(m*nunroll+l)=z(j+m*q+l)
                  end do
               end do
               call mfftnb(n(d),nunroll,cwk,phase(0,d))
               do m=0, t-1
                  do l=0, nunroll-1
                     z(j+m*q+l)=cwk(bitrev(m,d)*nunroll+l)
                  end do
               end do
            end do
         else
c     **in progress** to be more efficient
            do j=0, q-1
               do m=0, t-1
                  cwk(m)=z(j+m*q)
               end do
               call mfftnb(n(d),1,cwk,phase(0,d))
               do m=0, t-1
                  z(j+m*q)=cwk(bitrev(m,d))
               end do
            end do
         end if
      else
         call mfftnb(n(d),q,z,phase(0,d))
         call btrvm(t,q,WSIZE,z,bitrev(0,d))
      end if
      end

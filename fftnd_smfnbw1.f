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
      subroutine fftnd(nd,nmax,n,z,cwk,phase,bitrev)
      implicit none
      integer nmax,nd,n(nd),bitrev(0:nmax-1,nd)
      integer i,j,k,l,m,q,r,s,t,nunroll
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      integer WSIZE
c     WSIZE equals to the half of window size
c     128kbytes, (size of complex*16)=16
      parameter(WSIZE=128*1024/16)
      s=0
      do i=1, nd
         s=s+n(i)
      end do
      s=2**s
      r=1
c     First Axis
      i=1
      q=2**n(i)
      if(q.le.WSIZE)then
         do k=0, s-1, q
            do j=0, q-1
               cwk(j)=z(k+j)
            end do
            call mfftnb(n(i),1,cwk,phase(0,i))
            do j=0, q-1
               z(k+j)=cwk(bitrev(j,i))
            end do
         end do
      else
         do k=0, s-1, q
            call mfftnb(n(i),1,z(k),phase(0,i))
            call btrv(q,WSIZE,z(k),bitrev(0,i))
         end do
      end if
      r=q
c     Other
      do i=2, nd
         t=2**n(i)
         q=r*t
         if(q.gt.WSIZE)then
            if(t.le.WSIZE)then
c     for efficient use of cache line
               nunroll=min(r,WSIZE/t)
               do k=0, s-1, q
c     for efficient use of cache line
                  do j=k, k+r-1, nunroll
                     do m=0, t-1
                        do l=0, nunroll-1
                           cwk(m*nunroll+l)=z(j+r*m+l)
                        end do
                     end do
                     call mfftnb(n(i),nunroll,cwk,phase(0,i))
                     do m=0, t-1
                        do l=0, nunroll-1
                           z(j+r*m+l)=cwk(bitrev(m,i)*nunroll+l)
                        end do
                     end do
                  end do
c
c$$$                  do j=k, k+r-1
c$$$                     do m=0, t-1
c$$$                        cwk(m)=z(j+r*m)
c$$$                     end do
c$$$                     call mfftnb(n(i),1,cwk,phase(0,i))
c$$$                     do m=0, t-1
c$$$                        z(j+r*m)=cwk(bitrev(m,i))
c$$$                     end do
c$$$                  end do
c                  
               end do
            else
c     need to be more efficient
               do k=0, s-1, q
                  do j=k, k+r-1
                     do m=0, t-1
                        cwk(m)=z(j+r*m)
                     end do
                     call mfftnb(n(i),1,cwk,phase(0,i))
                     do m=0, t-1
                        z(j+r*m)=cwk(bitrev(m,i))
                     end do
                  end do
               end do
            end if
         else
            do k=0, s-1, q
               do l=0, q-1
                  cwk(l)=z(k+l)
               end do
               call mfftnb(n(i),r,cwk,phase(0,i))
               do m=0, t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
                  do l=0, r-1
                     z(k+r*m+l)=cwk(bitrev(m,i)*r+l)
                  end do
               end do
            end do
         end if
         r=q
      end do
      end

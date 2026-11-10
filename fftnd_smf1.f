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
      parameter(nunroll=2**5)
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      s=0
      do i=1, nd
         s=s+n(i)
      end do
      s=2**s
      r=1
      do i=1, nd
         t=2**n(i)
         q=r*t
         do k=0, s-1, q
            if(r.ge.nunroll)then
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
                  call mfft(n(i),nunroll,cwk,phase(0,i),bitrev(0,i))
                  do m=0, t-1
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
                     do l=0, nunroll-1
                        z(j+m*r+l)=cwk(m*nunroll+l)
                     end do
                  end do
               end do
            else
               call mfft(n(i),r,z(k),phase(0,i),bitrev(0,i))
            end if
         end do
         r=q
      end do
      end

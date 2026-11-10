************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
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
      complex*16 z(0:*),cwk(0:*),phase(nmax/2,nd)
      integer i,j,k,m,q,r,s,t
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
            if(r.ge.8)then
               do j=k, k+r-1, 8
                  do m=0, t-1
                     cwk(m)    =z(j+m*r)
                     cwk(t+m)  =z(j+m*r+1)
                     cwk(2*t+m)=z(j+m*r+2)
                     cwk(3*t+m)=z(j+m*r+3)
                     cwk(4*t+m)=z(j+m*r+4)
                     cwk(5*t+m)=z(j+m*r+5)
                     cwk(6*t+m)=z(j+m*r+6)
                     cwk(7*t+m)=z(j+m*r+7)
                  end do
                  call fftnb(n(i),cwk(0),  phase(1,i))
                  call fftnb(n(i),cwk(t),  phase(1,i))
                  call fftnb(n(i),cwk(2*t),phase(1,i))
                  call fftnb(n(i),cwk(3*t),phase(1,i))
                  call fftnb(n(i),cwk(4*t),phase(1,i))
                  call fftnb(n(i),cwk(5*t),phase(1,i))
                  call fftnb(n(i),cwk(6*t),phase(1,i))
                  call fftnb(n(i),cwk(7*t),phase(1,i))
                  do m=0, t-1
                     z(j+m*r)  =cwk(    bitrev(m,i))
                     z(j+m*r+1)=cwk(  t+bitrev(m,i))
                     z(j+m*r+2)=cwk(2*t+bitrev(m,i))
                     z(j+m*r+3)=cwk(3*t+bitrev(m,i))
                     z(j+m*r+4)=cwk(4*t+bitrev(m,i))
                     z(j+m*r+5)=cwk(5*t+bitrev(m,i))
                     z(j+m*r+6)=cwk(6*t+bitrev(m,i))
                     z(j+m*r+7)=cwk(7*t+bitrev(m,i))
                  end do
               end do
            else if(r.ge.4)then
               do j=k, k+r-1, 4
                  do m=0, t-1
                     cwk(m)    =z(j+m*r)
                     cwk(t+m)  =z(j+m*r+1)
                     cwk(2*t+m)=z(j+m*r+2)
                     cwk(3*t+m)=z(j+m*r+3)
                  end do
                  call fftnb(n(i),cwk(0),  phase(1,i))
                  call fftnb(n(i),cwk(t),  phase(1,i))
                  call fftnb(n(i),cwk(2*t),phase(1,i))
                  call fftnb(n(i),cwk(3*t),phase(1,i))
                  do m=0, t-1
                     z(j+m*r)  =cwk(    bitrev(m,i))
                     z(j+m*r+1)=cwk(  t+bitrev(m,i))
                     z(j+m*r+2)=cwk(2*t+bitrev(m,i))
                     z(j+m*r+3)=cwk(3*t+bitrev(m,i))
                  end do
               end do
            else if(r.ge.2)then
               do j=k, k+r-1, 2
                  do m=0, t-1
                     cwk(m)  =z(j+m*r)
                     cwk(t+m)=z(j+m*r+1)
                  end do
                  call fftnb(n(i),cwk(0),phase(1,i))
                  call fftnb(n(i),cwk(t),phase(1,i))
                  do m=0, t-1
                     z(j+m*r)  =cwk(  bitrev(m,i))
                     z(j+m*r+1)=cwk(t+bitrev(m,i))
                  end do
               end do
            else
               do j=k, k+r-1
                  do m=0, t-1
                     cwk(m)  =z(j+m*r)
                  end do
                  call fftnb(n(i),cwk(0),phase(1,i))
                  do m=0, t-1
                     z(j+m*r)  =cwk(  bitrev(m,i))
                  end do
               end do
            end if
         end do
         r=q
      end do
      end

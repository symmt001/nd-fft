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

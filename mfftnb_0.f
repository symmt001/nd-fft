************************************************************************
*     Multiple FFT without bit-reversal
*     integer n: Multiplicity
*     integer m: log2(N)
*     complex*16 z(n,2**m)        : Input & Output
*     complex*16 phase(0:2**(m-1)): Phase-factor
************************************************************************
      subroutine mfftnb(m,n,z,phase)
*odirr NOALTCODE
      implicit none
      complex*16 z(0:*),phase(0:*),x,y,ph
      integer i,j,j2,l,m,n,p,q
      l=n*(2**m)
      p=1
      q=l/2
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
      do j2=0, q-1
         x=z(j2)
         y=z(j2+q)
         z(j2)  =x+y
         z(j2+q)=x-y
      end do
      p=p*2
      l=q
      q=q/2
      do i=2, m
         do j=0, p-1
            ph=phase(j)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
            do j2=j*l, j*l+q-1
               x=z(j2)
               y=z(j2+q)*ph
               z(j2)  =x+y
               z(j2+q)=x-y
            end do
         end do
         p=p*2
         l=q
         q=q/2
      end do
      end

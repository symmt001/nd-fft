************************************************************************
*     Multiple FFT
************************************************************************
      subroutine mfft(n,m,z,phase,bitrev)
      implicit none
      integer i,j,j2,j3,n,p,pm,q,m
      complex*16 z(0:m*(2**n)-1),phase(0:2**n/2-1),x,y
      integer bitrev(0:2**n-1)
      q=2**n
*     bit reversal
      do j=0,q-1
         i=bitrev(j)
         if(i.gt.0) then
            i=i*m
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
            do j2=j*m, (j+1)*m-1
               x=z(i+j2)
               z(i+j2)=z(j2)
               z(j2)=x
            end do
         end if
      end do
      p=1
      q=q/2
      do i=1, n
         pm=p*m
         do j2=0, 2**n-1, 2*p
            do j=0, p-1
               x=phase(j*q)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
               do j3=(j2+j)*m, (j2+j+1)*m-1
                  y=z(pm+j3)*x
                  z(pm+j3)=z(j3)-y
                  z(   j3)=z(j3)+y
               end do
            end do
         end do
         p=p*2
         q=q/2
      end do
      end

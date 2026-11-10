************************************************************************
*     decimate once
************************************************************************
      subroutine decn1m(m,p,q,z,phase)
      implicit none
      integer i,j,k,m,p,q,pm
      integer s0,s1
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 c,s,wr,wi
      pm=p*m
*POPTION INDEP
      do i=0, 2*q*pm-1, 2*pm
         do j=0, p-1, 2
            c=phase(j*q)
            s=phase(j*q+1)
            s0=i+j*m
            s1=pm+s0
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=0, 2*m-1, 2
               wr=z(s1+k)*c-z(s1+k+1)*s
               wi=z(s1+k)*s+z(s1+k+1)*c
               z(s1+k  )=z(s0+k  )-wr
               z(s1+k+1)=z(s0+k+1)-wi
               z(s0+k  )=z(s0+k  )+wr
               z(s0+k+1)=z(s0+k+1)+wi
            end do
         end do
      end do
      end 

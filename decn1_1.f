************************************************************************
*     decimate once
************************************************************************
      subroutine decn1(p,q,z,phase)
      implicit none
      integer i,j,p,q,ip
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 c,s,wr,wi
*POPTION INDEP
      do i=0, 2*q*p-1, 2*p
         ip=i+p
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do j=0, p-1, 2
            c=phase(j*q)
            s=phase(j*q+1)
            wr=z(ip+j)*c-z(ip+j+1)*s
            wi=z(ip+j)*s+z(ip+j+1)*c
            z(ip+j  )=z(i+j  )-wr
            z(ip+j+1)=z(i+j+1)-wi
            z(i +j  )=z(i+j  )+wr
            z(i +j+1)=z(i+j+1)+wi
         end do
      end do
      end 

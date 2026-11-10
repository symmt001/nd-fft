************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx1(p,q,z,phase)
      implicit none
      integer i,j,k,p,q,q2
      integer i0x0,i1x0,i0x1,i1x1
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 c,s,wr0,wi0,wr1,wi1
      q2=2*q
*POPTION INDEP
      do i=0, q2*p-1, 2*q2
*POPTION INDEP
         do k=i, i+q2-1, 2*p
            i0x0=k
            i0x1=i0x0+p
            i1x0=q2+i0x0
            i1x1=q2+i0x1
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1, 2
               c=phase(j*q)
               s=phase(j*q+1)
               wr1=z(i1x1+j)*c-z(i1x1+j+1)*s
               wi1=z(i1x1+j)*s+z(i1x1+j+1)*c
               wr0=z(i1x0+j)*c-z(i1x0+j+1)*s
               wi0=z(i1x0+j)*s+z(i1x0+j+1)*c

               z(i1x1+j  )=z(i0x1+j  )-wr1
               z(i1x1+j+1)=z(i0x1+j+1)-wi1
               z(i1x0+j  )=z(i0x1+j  )+wr1
               z(i1x0+j+1)=z(i0x1+j+1)+wi1

               z(i0x1+j  )=z(i0x0+j  )-wr0
               z(i0x1+j+1)=z(i0x0+j+1)-wi0
               z(i0x0+j  )=z(i0x0+j  )+wr0
               z(i0x0+j+1)=z(i0x0+j+1)+wi0
            end do
         end do
      end do
c      write(*,*)z
      end 

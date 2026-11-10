************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx1m(m,p,q,z,phase)
      implicit none
      integer i,i2,j,k,m,p,q,qm2,pm
      integer i0x0,i1x0,i0x1,i1x1
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 c,s,wr0,wi0,wr1,wi1
c      write(*,*)'X:m,p,q',m,p,q
c      write(*,*)z
      qm2=2*q*m
      pm =  p*m
*POPTION INDEP
      do i=0, qm2*p-1, 2*qm2
*POPTION INDEP
         do i2=i, i+qm2-1, 2*pm
*POPTION INDEP
            do j=0, p-1, 2
               c=phase(j*q)
               s=phase(j*q+1)
               i0x0=i2+j*m
               i0x1=i0x0+pm
               i1x0=qm2+i0x0
               i1x1=qm2+i0x1
c               write(*,*)i0x0,i0x1,i1x0,i1x1
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do k=0, 2*m-1, 2
                  wr1=z(i1x1+k)*c-z(i1x1+k+1)*s
                  wi1=z(i1x1+k)*s+z(i1x1+k+1)*c
                  wr0=z(i1x0+k)*c-z(i1x0+k+1)*s
                  wi0=z(i1x0+k)*s+z(i1x0+k+1)*c

                  z(i1x1+k  )=z(i0x1+k  )-wr1
                  z(i1x1+k+1)=z(i0x1+k+1)-wi1
                  z(i1x0+k  )=z(i0x1+k  )+wr1
                  z(i1x0+k+1)=z(i0x1+k+1)+wi1

                  z(i0x1+k  )=z(i0x0+k  )-wr0
                  z(i0x1+k+1)=z(i0x0+k+1)-wi0
                  z(i0x0+k  )=z(i0x0+k  )+wr0
                  z(i0x0+k+1)=z(i0x0+k+1)+wi0
               end do
            end do
         end do
      end do
c      write(*,*)z
      end 

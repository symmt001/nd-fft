************************************************************************
*     rearrange as bit-reversal order in place
************************************************************************
      subroutine btrv(q,wsize,z,bitrev)
      implicit none
      integer l,m,q,wsize
      integer bitrev(0:q-1)
      complex*16 z(0:q-1),x
      do m=0, q-1
         l=bitrev(m)
         if(l.gt.m)then
            x=z(l)
            z(l)=z(m)
            z(m)=x
         end if
      end do
      end

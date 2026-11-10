************************************************************************
*     rearranging data as bit-reversal order, in-place version
************************************************************************
      subroutine btrvm(q,m,wsize,z,bitrev)
      implicit none
      integer j,k,l,m,q,r,wsize,nunroll
      integer bitrev(0:q-1)
      complex*16 z(0:q*m-1),x
      nunroll=max(min(wsize/2,m),1)
c      write(*,*) nunroll
      do j=0, q-1
         k=bitrev(j)
         if(k.gt.j)then
            do l=0, m-1, nunroll
*SOPTION UNROLL(2)
               do r=l, l+nunroll-1
                  x=z(j*m+r)
                  z(j*m+r)=z(k*m+r)
                  z(k*m+r)=x
               end do
            end do
         end if
      end do
      end

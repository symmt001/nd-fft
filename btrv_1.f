************************************************************************
*     rearranging data as bit-reversal order, in-place version
*     wsize equals to the half of window size
************************************************************************
      subroutine btrv(q,wsize,z,bitrev)
      implicit none
      integer i,j,k,l,q,t,p,wsize
      integer bitrev(0:q-1)
      complex*16 z(0:q-1),x
      p=1
 10   continue
      if( p*p*4 .le. min(q,wsize) ) then
         p=p*2
         goto 10
      end if
      t=q/p
      do k=0, t-1, p
         l=bitrev(k)
         if(l.gt.k)then
            do j=0, q-1, t
               do i=0, p-1
                  x=z(l+bitrev(j)+bitrev(i))
                  z(l+bitrev(j)+bitrev(i))=z(k+j+i)
                  z(k+j+i)=x
               end do
            end do
         else if(l.eq.k) then
            do j=0, q-1, t
               do i=0, bitrev(j)-1
                  x=z(l+bitrev(j)+bitrev(i))
                  z(l+bitrev(j)+bitrev(i))=z(k+j+i)
                  z(k+j+i)=x
               end do
            end do
         end if
      end do
      end

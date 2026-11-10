************************************************************************
*     rearranging data as bit-reversal order, in-place version
************************************************************************
      subroutine btrv(q,wsize,z,bitrev)
      implicit none
      integer i,j,k,l,q,t,p,wsize
      integer bitrev(0:q-1)
      complex*16 z(0:q-1),x
      if( q .le. wsize ) then
         do k=0, q-1
            l=bitrev(k)
            if(l.gt.k)then
               x=z(l)
               z(l)=z(k)
               z(k)=x
            end if
         end do
      else
         p=1
 10      continue
c     divide window into 2 bank (l=bitrev(k) and k)
         if( p*p*8 .le. wsize ) then
            p=p*2
            goto 10
         end if
         t=q/p
         do k=0, t-1, p
            l=bitrev(k)
            if(l.gt.k)then
               do j=0, p-1
                  do i=0, p-1
                     x=z(l+j+bitrev(i))
                     z(l+j+bitrev(i))=z(k+bitrev(j)+i)
                     z(k+bitrev(j)+i)=x
                  end do
               end do
            else if(l.eq.k) then
               do j=0, p-1
                  do i=0, j-1
                     x=z(k+j+bitrev(i))
                     z(k+j+bitrev(i))=z(k+bitrev(j)+i)
                     z(k+bitrev(j)+i)=x
                  end do
               end do
            end if
         end do
      end if
      end

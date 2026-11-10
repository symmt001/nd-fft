************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     p,q should equivalent to 0 (mod 32)
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k, jj, kk, NRL
      parameter(NRL=32)
      complex*16 dst(0:q*p-1),src(0:q*p-1)
      complex*16 buf(0:NRL-1,0:NRL-1)
      do j=0, p-NRL, NRL
         do k=0, q-NRL, NRL
            do jj=0, NRL-1
               do kk=0, NRL-1
                  buf(kk,jj)=src((j+jj)*q+k+kk)
               end do
            end do
            do kk=0, NRL-1
               do jj=0, NRL-1
                  dst((k+kk)*p+j+jj)=buf(kk,jj)
               end do
            end do
         end do
      end do
      end

************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     p,q should equivalent to 0 (mod 32)
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k,jj,kk,l,kp,jq,NRL
      parameter(NRL=32)
      complex*16 dst(0:q*p-1),src(0:q*p-1)
      complex*16 buf(0:NRL-1,0:NRL-1),tmp
      jq=0
      do j=0, p-NRL, NRL
         kp=j
         do k=0, q-NRL, NRL
            l=jq+k
            do jj=0, NRL-1
               do kk=0, NRL-1
                  buf(kk,jj)=src(l+kk) ! l=(j+jj)*q+k
               end do
               l=l+q
            end do
            do jj=0, NRL-1
               do kk=0, NRL-1
                  dst(kp+kk)=buf(jj,kk)! kp=(k+jj)*p+j
               end do
               kp=kp+p
            end do
         end do
         jq=jq+NRL*q
      end do
      end

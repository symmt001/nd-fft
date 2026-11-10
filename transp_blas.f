************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     using BLAS
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j
      complex*16 dst(0:q*p-1),src(0:q*p-1)
*     For effective use of BLAS, vector length is maxmized.
      if(q.ge.p)then
         do j=0, p-1
            call zcopy(q,src(j*q),1,dst(j),p)
         end do
      else
         do j=0, q-1
            call zcopy(p,src(j),q,dst(j*p),1)
         end do
      end if
      end

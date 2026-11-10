************************************************************************
*     transpose + bit-reversal on the second argument
*     using BLAS
*     [complex*16 src(q,bitrev(p))] -> [complex*16 dst(p,q)]
*     [complex*16 src(q,p)] -> [complex*16 dst(bitrev(p),q)]
************************************************************************
      subroutine transb(q,p,src,dst,bitrev)
      implicit none
      integer p,q,j
      integer bitrev(0:p-1)
      complex*16 dst(0:q*p-1),src(0:q*p-1)
      do j=0, p-1
         call zcopy(q,src(j*q),1,dst(bitrev(j)),p)
      end do
      end

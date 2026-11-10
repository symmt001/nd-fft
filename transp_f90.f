************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     using BLAS
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j
      complex*16 dst(p,q),src(q,p)
      dst=transpose(src)
      end

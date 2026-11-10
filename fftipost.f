************************************************************************
*     post process for ifft
*     multiply 1/2**(n(1)+n(2)+...+n(nd))
*     dimension x(2**n(1),2**n(2),...,2**n(nd))
************************************************************************
      subroutine fftipost(nd,n,x)
*odirr NOALTCODE
      implicit none
      complex*16 x(*)
      real*8     f
      integer    j,k,nd,n(nd),nsum
      nsum=0
      do j=1, nd
         nsum=nsum+n(j)
      end do
      k=2**nsum
      f=1.d0/k
*vdir noaltcode
      do j=1,k
         x(j)=f*x(j)
      end do
      end

************************************************************************
*     BENCH
************************************************************************
      program bench
      implicit none
      integer N
      parameter(N=200)
      real*8  usrtime, u_start, u_end, t, q
      real*8  a(N,N),b(N,N),c(N,N)
      integer i,j,k
      common a,b,c
      do j=1, N
         do i=1, N
            a(i,j)=0.d0
            b(i,j)=0.d0
            c(i,j)=0.d0
         end do
      end do
      write(*,*) 'Testing...'
      u_start=usrtime()
      do j=1, N
         do i=1, N
            do k=1, N
               c(i,j)=c(i,j)+a(i,k)*b(k,j)
            end do
         end do
      end do
      u_end=usrtime()
      t=(u_end-u_start)
      q=2*N*N*N
      write(*,200) N,t,q/t/1.d6
 200  format(1P,'N=',I6,', TIME=',E9.2,', MFLOPS=',E9.2)
      end

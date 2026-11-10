************************************************************************
*     BENCH
************************************************************************
      program bench
      implicit none
      integer N,ITE,i,j
      parameter(N=2**14+1,ITE=2**6)
      real*8  usrtime, u_start, u_end, t, q
      real*8  a(4,N),b(4,N),u,v,w,x,y,z
      common a,b
      do j=1, N
         a(1,j)=1.d0
         a(2,j)=2.d0
         a(3,j)=3.d0
         a(4,j)=4.d0
      end do
      write(*,*) 'Testing...'
      u=1/SQRT(2.d0)
      v=1/SQRT(2.d0)
      u_start=usrtime()
      do i=1, ITE, 2
         do j=1, N
            w=a(1,j)
            x=a(2,j)
            y=a(3,j)
            z=a(4,j)
            b(1,j)=w+y
            b(2,j)=x+z
            b(3,j)=(w-y)*u-(x-z)*v
            b(4,j)=(w-y)*v+(x-z)*u
         end do
         do j=1, N
            w=b(1,j)
            x=b(2,j)
            y=b(3,j)
            z=b(4,j)
            a(1,j)=w+y
            a(2,j)=x+z
            a(3,j)=(w-y)*u-(x-z)*v
            a(4,j)=(w-y)*v+(x-z)*u
         end do
      end do
      u_end=usrtime()
      t=(u_end-u_start)
      q=10*dble(N)*dble(ITE)
      write(*,200) q,t,q/t/1.d6
 200  format(1P,'Q=',E9.2,', TIME=',E9.2,', MFLOPS=',E9.2)
      end

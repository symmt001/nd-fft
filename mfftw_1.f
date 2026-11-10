************************************************************************
*     multiple fft routine
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 wk(m,2**n): work space
*     complex*16 phase(2**(n-1)) : phase factors
************************************************************************
      subroutine mfftw(n,m,z,wk,phase,iside)
      implicit none
c      real*8 z(0:2*m*2**n-1),wk(0:2*m*2**n-1),phase(0:2**n-1)
      real*8 z(0:*),wk(0:*),phase(0:*)
      real*8 c1,s1,wr,wi,iflag
      integer i,j,k,l,n,m,p,q,r,iside
      r=2**n
      q=r
      iflag=phase(r/2+1)
      do i=1, n
         if(iside.eq.1)then
*POPTION INDEP
            do j=0, r-1, q
               c1=phase(j)
               s1=phase(j+1)
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do k=0, q*m-1, 2
                  l=2*j*m+k
                  p=j*m+k
                  wr=z(q*m+l)*c1-z(q*m+l+1)*s1
                  wi=z(q*m+l)*s1+z(q*m+l+1)*c1
                  wk(    p  )=z(l  )+wr
                  wk(    p+1)=z(l+1)+wi
                  wk(r*m+p  )=z(l  )-wr
                  wk(r*m+p+1)=z(l+1)-wi
               end do
            end do
         else
*POPTION INDEP
            do j=0, r-1, q
               c1=phase(j)
               s1=phase(j+1)
*VOCL LOOP,NOVREC(WK,Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do k=0, q*m-1, 2
                  l=2*j*m+k
                  p=j*m+k
                  wr=wk(q*m+l)*c1-wk(q*m+l+1)*s1
                  wi=wk(q*m+l)*s1+wk(q*m+l+1)*c1
                  z(    p  )=wk(l  )+wr
                  z(    p+1)=wk(l+1)+wi
                  z(r*m+p  )=wk(l  )-wr
                  z(r*m+p+1)=wk(l+1)-wi
               end do
            end do
         end if
         iside=-iside
         q=q/2
      end do
      end

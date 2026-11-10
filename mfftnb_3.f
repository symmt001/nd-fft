************************************************************************
*     multiple fft routine without bit-reversal
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors (bit-reversal order)
************************************************************************
      subroutine mfftnb(n,m,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*)
      integer n,m,p,q,r
      q=2**(n+1)
      r=2
      p=mod(n,4)
c     first decimation
      if(p.eq.0)then
         q=q/16
         call dec4fm(m,q,z,phase)
         r=r*16
      else if(p.eq.3)then
         q=q/8
         call dec3fm(m,q,z,phase)
         r=r*8
      else if(p.eq.2)then
         q=q/4
         call dec2fm(m,q,z,phase)
         r=r*4
      else if(p.eq.1 .and. n.gt.1)then
         q=q/32
         call dec5fm(m,q,z,phase)
         r=r*32
      else if(p.eq.1 .and. n.eq.1)then
         q=q/2
         call dec1fm(m,q,z)
         r=r*2
      else
         stop
      end if
c     main loop
 10   continue
         q=q/16
         call dec4m(m,q,r,z,phase)
         r=r*16
      if (q.gt.2) goto 10
      end

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
      integer n,m,q,r
      q=2**(n+1)
      r=2
c     first decimation
c  It turns out to be useless. (dec4f/HP-9000J282)
c$$$      if(q.ge.32)then
c$$$         q=q/16
c$$$         call dec4fm(m,q,z,phase)
c$$$         r=r*16
c$$$      else if(q.ge.16)then
      if(q.ge.16)then
         q=q/8
         call dec3fm(m,q,z,phase)
         r=r*8
c$$$      else if(q.ge.8)then
c$$$         q=q/4
c$$$         call dec2fm(m,q,z,phase)
c$$$         r=r*4
      end if
c     do while(q.gt.2)
 10   continue
c  It turns out to be useless. (dec5/HP-9000J282)
c$$$      if(q.ge.64)then
c$$$         q=q/32
c$$$         call dec5m(m,q,r,z,phase)
c$$$         r=r*32
      if(q.ge.32)then
         q=q/16
         call dec4m(m,q,r,z,phase)
         r=r*16
      else if(q.ge.16)then
         q=q/8
         call dec3m(m,q,r,z,phase)
         r=r*8
      else if(q.ge.8)then
         q=q/4
         call dec2m(m,q,r,z,phase)
         r=r*4
      else if(q.ge.4)then
         q=q/2
         call dec1m(m,q,r,z,phase)
         r=r*2
      end if
      if (q.gt.2) goto 10
c      end do
      end

************************************************************************
*     multiple fft routine without bit-reversal
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors (bit-reversal order)
************************************************************************
      subroutine fftnb(n,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*),iflag
      integer n,q,r
      q=2**(n+1)
      r=2
      if(n.ge.2) iflag=phase(3)
c     first decimation
c  It turns out to be useless. (dec4f/HP-9000J282)
c$$$      if(q.ge.32)then
c$$$         q=q/16
c$$$         call dec4fm(1,q,z,phase)
c$$$         r=r*16
c$$$      else if(q.ge.16)then
      if(q.ge.16)then
         q=q/8
         call dec3fm(1,q,z,phase)
         r=r*8
      end if
c     do while(i.gt.0)
 10   continue
c  It turns out to be useless. (dec5/HP-9000J282)
c$$$      if(i.ge.5)then
c$$$         q=q/32
c$$$         call dec5m(1,q,r,z,phase)
c$$$         r=r*32
c$$$      else if(i.ge.4)then
c  It turns out to be almost useless. (dec4l/HP-9000J282)
      if(q.ge.32)then
         q=q/16
         call dec4m(1,q,r,z,phase)
         r=r*16
c  It turns out to be almost useless. (dec4l/HP-9000J282)
c$$$      else if(q.eq.32)then
c$$$         q=q/16
c$$$         call dec4l(r,iflag,z,phase)
c$$$         r=r*16
      else if(q.eq.16)then
         q=q/8
         call dec3l(r,iflag,z,phase)
         r=r*8
      else if(q.eq.8)then
         q=q/4
         call dec2l(r,iflag,z,phase)
         r=r*4
      else if(q.eq.4)then
         q=q/2
         call dec1l(r,iflag,z,phase)
         r=r*2
      end if
      if (q.gt.2) goto 10
c      end do
      end

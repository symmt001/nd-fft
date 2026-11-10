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
      real*8 z(0:*),wk(0:*),phase(0:*),iflag
      integer n,m,q,r,iside
      r=2**(n+1)
      q=r
      iflag=phase(r/4+1)
c     first decimation
         if(q.ge.16)then
            q=q/8
            if(iside.eq.1)then
               call decb3fm(m,q,iflag,z,wk,phase)
            else
               call decb3fm(m,q,iflag,wk,z,phase)
            end if
            iside=-iside
         end if
c      do while(q.gt.2)
 10   continue
c     Decimate 4 times -> less performance on SX-3
c$$$         if(q.ge.32)then
c$$$            q=q/16
c$$$            if(iside.eq.1)then
c$$$               call decb4m(m,q,r/16,z,wk,phase)
c$$$            else
c$$$               call decb4m(m,q,r/16,wk,z,phase)
c$$$            end if
c$$$            iside=-iside
c$$$         else if(q.ge.16)then
         if(q.ge.16)then
            q=q/8
            if(iside.eq.1)then
               call decb3m(m,q,r/8,z,wk,phase)
            else
               call decb3m(m,q,r/8,wk,z,phase)
            end if
            iside=-iside
         else if(q.ge.8)then
            q=q/4
            if(iside.eq.1)then
               call decb2m(m,q,r/4,z,wk,phase)
            else
               call decb2m(m,q,r/4,wk,z,phase)
            end if
            iside=-iside
         else if(q.ge.4)then
            q=q/2
            if(iside.eq.1)then
               call decb1m(m,q,r/2,z,wk,phase)
            else
               call decb1m(m,q,r/2,wk,z,phase)
            end if
            iside=-iside
         end if
      if (q.gt.2) goto 10
c      end do
      end

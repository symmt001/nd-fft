************************************************************************
*     FFT routine
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     integer bitrev : dummy argument for backword compatibility
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors
************************************************************************
      subroutine fft(n,z,phase,bitrev)
      implicit none
      integer n,p,q,r,bitrev(*)
      real*8 z(0:2*(2**n)-1),phase(0:2**n-1),iflag
      p=2
      q=2**(n-1)
      r=q*p
      iflag=phase(r/2+1)
c     first decimation
      if(q.ge.p*4)then
         call decx2f(q,iflag,z)
         p=p*4
         q=q/4
      end if
 10   continue
         if(q.ge.p*4)then
            call decx2(p,q,z,phase)
            p=p*4
            q=q/4
         else if(q.ge.p)then
            call decx1(p,q,z,phase)
            p=p*2
            q=q/2
         end if
      if (q.ge.p) goto 10
 20   continue
         if(q.ge.4)then
            call decn3(p,q,z,phase)
            p=p*8
            q=q/8
         else if(q.ge.2)then
            call decn2(p,q,z,phase)
            p=p*4
            q=q/4
         else if(q.ge.1)then
            call decn1(p,q,z,phase)
            p=p*2
            q=q/2
         end if
      if (q.ge.1) goto 20
c      end do
      end

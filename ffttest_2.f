************************************************************************
*     fft test (1-d tests for all the basis)
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPX,NX,ND
      real*8  EPS
      parameter(ND=1)
      parameter(NPX=9)
      parameter(NX=2**NPX)
      parameter(EPS=1.d-13)
      complex*16 phase(NX/2),x(NX),cwk(NX)
      complex*16 y,phx
      integer    bitrev(NX)
      integer    i,ip,n,p,q
      common x,cwk
      n=NPX
*     set bit reversal table
      call fftprep1(ND,NX,n,bitrev)
*     set phase table
      call fftprep2(ND,NX,n,phase)
      write(*,*) ' Testing...'
      do 999 q=0, NX-1
         p=q
         do 40 i=0, NX-1
c$$$  x(i+1)=cdexp(iu*PI2/dble(NMAX)(i*p))
            ip=i*p
            ip=mod(ip,NX)
            if(ip.ge.NX/2)then
               phx=-phase(ip-NX/2+1)
            else
               phx= phase(ip+1)
            end if
            x(i+1)=phx
 40      continue
*     call fftprep1(ND,NM,n,bitrev)
*     call fftprep2(ND,NM,n,phase)
*     (phase() = phase(bitrev()) , if it is needed)
         call fftprep4(ND,NX,n,phase,bitrev)
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NX,phase)
         call fftnd(ND,NX,n,x,cwk,phase,bitrev)
c     IDFT
         do 190 i=0, 2**n-1
            y=x(i+1)
            if (i.eq.p) then
               y=y/2**n-1.d0
            else
               y=y/2**n
            end if
            if (abs(y) .gt. EPS) then
               write(*,*)i,x(i+1),2**n
               write(*,*) 'error in FFT'
               stop
            end if
 190     continue
         call fftprep4(ND,NX,n,phase,bitrev)
 999  continue
      write(*,*) ' OK!'
      end

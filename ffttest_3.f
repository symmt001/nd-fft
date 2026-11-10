************************************************************************
*     fft test (test for 1-d speed and precision)
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPX,NX,ND,ITE
      parameter(ND=1)
      parameter(NPX=18,ITE=5)
      parameter(NX=2**NPX)
      complex*16 phase(NX/2),x(NX),cwk(NX),y
      real*8     rndm(NX),usrtime,u_start,u_end,t,q
      real*8     e_norm,v_norm
      integer    bitrev(NX)
      integer    i,n
      common x,cwk,rndm,bitrev,phase

      call RINISP(2147483647)
      call rndoid(NX,rndm)

      n=NPX

*     set bit reversal table
      call fftprep1(ND,NX,n,bitrev)
*     set phase table
      call fftprep2(ND,NX,n,phase)
      call RINISP(2147483647,i)

      v_norm=0.d0
      do i=1, NX
         x(i)=dcmplx(rndm(i))
         v_norm=v_norm+rndm(i)*rndm(i)
      end do
*     (phase() = phase(bitrev()) , if it is needed)
      call fftprep4(ND,NX,n,phase,bitrev)
      write(*,*) ' Testing...'
      u_start=usrtime()
      do i=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NX,phase)
*     DFT
         call fftnd(ND,NX,n,x,cwk,phase,bitrev)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,NX,phase)
*     IDFT
         call fftnd(ND,NX,n,x,cwk,phase,bitrev)
*     x is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times x
         call fftipost(ND,n,x)
      end do
      u_end=usrtime()
      e_norm=0.d0
      do i=1, NX
         y=x(i)-rndm(i)
         e_norm=e_norm+dble(y*conjg(y))
      end do
      t=(u_end-u_start)/(2*ITE)
      q=5*NX*log(dble(NX))/log(2.d0)
      write(*,200) NX,2*ITE,t,q/t/1.d6
      write(*,201)sqrt(e_norm/v_norm)
 200  format(1P,'N=',I8,', M=',I4,', TIME/M=',E9.2,', MFLOPS=',E9.2)
 201  format(1P,'||e||/||v||=',E9.2)
      end
************************************************************************      
*     GENERATE RANDOM NUMBERS FOLLOWING UNIFORM DISTRIBUTION
*     rndm \in (0,1]
************************************************************************      
      subroutine rndoid(NRANDOM,rndm)
!$HP$check_overflow integer off
      implicit none
      integer NRANDOM,nmax,irand,i,k
      real*8 rndm(*)
      common/rndmsd/k,nmax
      irand(i)=iand(i*1812433253,nmax)
      do 10 i=1, NRANDOM
         k=irand(k)
         rndm(i)=k/dble(nmax)
 10   continue
      end
************************************************************************
*     INITIALIZE RANDOM NUMBERS
************************************************************************
      subroutine rinisp(iseed)
      implicit none
      integer iseed,nmax,k
      common/rndmsd/k,nmax
c      nmax=2**31-1
      nmax=2**30-1
      nmax=nmax*2+1
      k=iseed
      end

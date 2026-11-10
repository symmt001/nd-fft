************************************************************************
*     fft test
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,ITE,NTOT
      real*8  EPS, usrtime, u_start, u_end, t, q, fac
      parameter(ND=3,ITE=10)
c      parameter(ND=3,ITE=100)
      parameter(NPX=7,NPY=7,NPZ=7)
c      parameter(NPX=6,NPY=6,NPZ=6)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
      parameter(NTOT=NX*NY*NZ)
c      parameter(NPM=max(NPX,NPY,NPZ),NM=2**NPM)
      parameter(NPM=NPZ,NM=2**NPM)
      parameter(EPS=1.d-13)
      complex*16 phase(NM/2,ND),phx,phy,phz
      complex*16 x(NX+1,NY,NZ),cwk(NX,NY,NZ),y
      integer    bitrev(NM,ND),i,j,k,l,ip,jp,kp,n(ND),p(ND)
      common x,cwk
      do k=1, NZ
         do j=1, NY
            do i=1, NX+1
               x(i,j,k)=(0.d0,0.d0)
            end do
         end do
      end do
      n(1)=NPX
      n(2)=NPY
      n(3)=NPZ
*     set bit reversal table
      call fftprep1(ND,NM,n,bitrev)
*     set phase table
      call fftprep2(ND,NM,n,phase)
      p(1)=7
      p(2)=16
      p(3)=17
      fac=1.d0/(NX*NY*NZ)
      do 20 k=0, NZ-1
         kp=k*p(3)
         kp=mod(kp,NZ)
         if(kp.ge.NZ/2)then
            phz=-phase(kp-NZ/2+1,3)
         else
            phz= phase(kp+1,3)
         end if
         do 30 j=0, NY-1
            jp=j*p(2)
            jp=mod(jp,NY)
            if(jp.ge.NY/2)then
               phy=-phase(jp-NY/2+1,2)
            else
               phy= phase(jp+1,2)
            end if
            do 40 i=0, NX-1
c$$$  x(i+1)=cdexp(iu*PI2/dble(NMAX)(i*p(1)+j*p(2)+k*p(3)))
               ip=i*p(1)
               ip=mod(ip,NX)
               if(ip.ge.NX/2)then
                  phx=-phase(ip-NX/2+1,1)
               else
                  phx= phase(ip+1,1)
               end if
               x(i+1,j+1,k+1)=phx*phy*phz
 40         continue
 30      continue
 20   continue
*     call fftprep1(ND,NM,n,bitrev)
*     call fftprep2(ND,NM,n,phase)
      write(*,*) 'Testing...'
      u_start=usrtime()
      do 100 l=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NM,phase)
         call fftnd(ND,NM,n,x,cwk,phase,bitrev)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,NM,phase)
         call fftnd(ND,NM,n,x,cwk,phase,bitrev)
*     x is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times x
      do k=1, NZ
         do j=1, NY
            do i=1, NX+1
               x(i,j,k)=fac*x(i,j,k)
            end do
         end do
      end do
 100  continue
*     phase = conjg(phase), for DFT
c      call fftprep3(ND,NM,phase)
      call fftnd(ND,NM,n,x,cwk,phase,bitrev)
      u_end=usrtime()
c     IDFT
      p(1)=NX-p(1)
      p(2)=NY-p(2)
      p(3)=NZ-p(3)
      do 170 k=0, 2**n(3)-1
         do 180 j=0, 2**n(2)-1
            do 190 i=0, 2**n(1)-1
               y=x(i+1,j+1,k+1)
               if (i.eq.p(1) .and. j.eq.p(2) .and. k.eq.p(3)) then
                  y=y/2**(n(1)+n(2)+n(3))-1.d0
               else
                  y=y/2**(n(1)+n(2)+n(3))
               end if
               if (abs(y) .gt. EPS) then
                  write(*,*)i,j,k,x(i+1,j+1,k+1),2**(n(1)+n(2)+n(3))
                  stop 'error in FFT'
               end if
 190        continue
 180     continue
 170  continue
      t=(u_end-u_start)/(2*ITE+1)
      q=5*NTOT*log(dble(NTOT))/log(2.d0)
      write(*,200) NX,NY,NZ,2*ITE+1,t,q/t/1.d6
 200  format(1P,'NX=',I4,', NY=',I4,', NZ=',I4,', M=',I4,
     $     ', TIME/M=',E9.2,', MFLOPS=',E9.2)
      end
************************************************************************
*     preparation for fft
*     set bit reversal table
*     nd:    dimension n(nd)
*     nmax = 2**max(n)
*     bitrev(0:nmax-1,nd)
************************************************************************
      subroutine fftprep1(nd,nmax,n,bitrev)
*odirr NOALTCODE
      implicit none
      integer nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      integer p,nsum

      integer ID, NN(3)
      common /GPFAWRK/ID,NN

      if(nd.ne.3) stop '3-dim only'
      nsum=0
      do p=1, nd
         NN(p)=2**n(p)
         nsum=nsum+n(p)
      end do
      ID=NN(1)+1
      end
************************************************************************
*     3-dimensional fft routine
*     Comparison against my own routine.
************************************************************************
      subroutine fftnd(nd,nmax,n,x,cwk,phase,bitrev)
      implicit none
      integer nmax,nd
      complex*16 x(0:*),cwk(0:*),phase(0:nmax/2-1,nd)
      integer bitrev(0:nmax-1,nd),n(nd)

      integer IS

      integer ID, NN(3)
      common /GPFAWRK/ID,NN

      if( dimag( phase(NN(1)/4,1) ) .gt. 0 ) then
         IS=-1
      else
         IS=1
      end if
      call GPF3D(x,ID,NN,IS)
      end

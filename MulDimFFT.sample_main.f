************************************************************************
*     fft test
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPX,NPY,NPZ,NX,NY,NZ,NPM,NM,ND,ITE,NTOT
      real*8  EPS, u_start, u_end, t, q
      real    etime, tarray(2)
      parameter(ND=3,ITE=0)
c      parameter(ND=3,ITE=10)
c      parameter(ND=3,ITE=100)
c      parameter(ND=3,ITE=1000)
c      parameter(NPX=4,NPY=4,NPZ=4)
c      parameter(NPX=5,NPY=5,NPZ=5)
c      parameter(NPX=6,NPY=6,NPZ=6)
      parameter(NPX=7,NPY=7,NPZ=7)
      parameter(NX=2**NPX,NY=2**NPY,NZ=2**NPZ)
      parameter(NTOT=NX*NY*NZ)
c      parameter(NPM=max(NPX,NPY,NPZ),NM=2**NPM)
      parameter(NPM=NPZ,NM=2**NPM)
      parameter(EPS=1.d-13)
      complex*16 phase(0:NM/2-1,ND),phx,phy,phz
      complex*16 x(0:NX-1,0:NY-1,0:NZ-1),cwk(NX,NY,NZ)
      real*8     rndm(ND),emax,y
      integer    bitrev(NM,ND),n(ND),p(ND),nemax(ND)
      integer    i,j,k,l
      common     x,cwk
      data       n/NPX,NPY,NPZ/

      call RINISP(2147483647)
      call rndoid(3,rndm)

*     set bit reversal table
      call fftprep1(ND,NM,n,bitrev)
*     set phase table
      call fftprep2(ND,NM,n,phase)
      do i=1, 3
         p(i)=int(rndm(i)*(2**n(i)))
      end do
      do k=0, NZ-1
         l=mod(k*p(3),NZ)
         if(l.ge.NZ/2)then
            phz=-phase(l-NZ/2,3)
         else
            phz= phase(l,3)
         end if
         do j=0, NY-1
            l=mod(j*p(2),NY)
            if(l.ge.NY/2)then
               phy=-phase(l-NY/2,2)
            else
               phy= phase(l,2)
            end if
            do i=0, NX-1
c$$$  x(i+1)=cdexp(iu*PI2/dble(NMAX)(i*p(1)+j*p(2)+k*p(3)))
               l=mod(i*p(1),NX)
               if(l.ge.NX/2)then
                  phx=-phase(l-NX/2,1)
               else
                  phx= phase(l,1)
               end if
               x(i,j,k)=phx*phy*phz
            end do
         end do
      end do
*     call fftprep1(ND,NM,n,bitrev)
*     call fftprep2(ND,NM,n,phase)
*     (phase() = phase(bitrev()) , if it is needed)
      call fftprep4(ND,NM,n,phase,bitrev)
      write(*,*) 'Testing...'
      u_start=etime(tarray)
      do i=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,NM,phase)
         call fftnd(ND,NM,n,x,cwk,phase,bitrev)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,NM,phase)
         call fftnd(ND,NM,n,x,cwk,phase,bitrev)
*     x is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times x
         call fftipost(ND,n,x)
      end do
*     phase = conjg(phase), for DFT
c      call fftprep3(ND,NM,phase)
      call fftnd(ND,NM,n,x,cwk,phase,bitrev)
      u_end=etime(tarray)
      call fftipost(ND,n,x)
c     IDFT
      do i=1, 3
         if (p(i).ne.0) p(i)=2**n(i)-p(i)
      end do
      emax =0.d0
      x(p(1),p(2),p(3))
     $     =x(p(1),p(2),p(3))-(1.d0,0.d0)
      do k=0, 2**n(3)-1
         do j=0, 2**n(2)-1
            do i=0, 2**n(1)-1
               y=abs(x(i,j,k))
               if( y .gt. emax) then
                  nemax(1)=i
                  nemax(2)=j
                  nemax(3)=k
                  emax=y
               end if
            end do
         end do
      end do
      t=(u_end-u_start)/(2*ITE+1)
      q=5*NTOT*log(dble(NTOT))/log(2.d0)
      write(*,200) NX,NY,NZ,2*ITE+1,t,q/t/1.d6
      if(emax.gt.EPS) write(*,201) emax,nemax,p
 200  format(1P,'NX=',I4,', NY=',I4,', NZ=',I4,', M=',I4,
     $     ', TIME/M=',E9.2,', MFLOPS=',E9.2)
 201  format(1P,'MaxError=',E9.2,' ,Place:',3I4,' ,Incident:',3I4)
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

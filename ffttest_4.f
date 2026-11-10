************************************************************************
*     fft test (Multi-dimensional test. DIM must be giveb by hand.
************************************************************************
      program ffttest
*odirr NOAUTOEXPAND
*odirr NOALTCODE
      implicit none
      integer NPMAX,NTOT,ND
      parameter(ND=3,NPMAX=21)
      parameter(NTOT=2**NPMAX)
      complex*16 x(0:NTOT-1),cwk(NTOT),phase(0:NTOT/2-1),c
      real*8     rndm(ND)
      real*8     t,y,q,usrtime,u_start,u_end,emax,err,EPS
      integer    BITREV(NTOT),N(ND),M(ND),P(ND),PE(ND)
      integer    i,j,k,l,r,s,v,ITE,MM,NT,kk,ll,k1,k2,l1,l2
      data       N/7,8,6/, ITE/5/, EPS/1.d-13/, MM/0/, NT/1/
      common     x,cwk,phase,BITREV

      call RINISP(2147483647)
      call rndoid(3,rndm)
      j=0
      do i=1, ND
         j=j+n(i)
      end do
      if(j.ne.NPMAX) stop 'Parameter error : NPMAX'
      
      do i=1, ND
         if(N(i).gt.MM) MM=N(i)
         j=2**N(i)
         M(i)=j
         NT=NT*j
         P(i)=int(rndm(i)*j)
      end do
      MM=2**MM
*     set bit reversal table
      call fftprep1(ND,MM,N,BITREV)
*     set phase table
      call fftprep2(ND,MM,N,phase)
c
c     preparing input data
c     x(i,j,k)=exp(i_unit*2*PI/NT*(i*p(1)+j*p(2)+k*p(3)))
      do i=0, NTOT-1
         x(i)=(1.d0,0.d0)
      end do
      r=1
      do i=1, ND
         s=r*M(i)
         kk=2**(N(i)/2)
         ll=M(i)/kk
         l1=p(i)/ll
         l2=p(i)-l1*ll
         do k=0, M(i)-1
            k1=k/kk
            k2=k-k1*kk
            v=mod(l1*ll*k2+l2*k1*kk+l2*k2,M(i))
            if(v.ge.M(i)/2)then
               c=-phase((i-1)*MM/2+v-M(i)/2)
            else
               c= phase((i-1)*MM/2+v)
            end if
            do j=0, NT-1, s
               do l=0, r-1
                  x(j+k*r+l)=x(j+k*r+l)*c
               end do
            end do
         end do
         r=s
      end do
*     call fftprep1(ND,MM,n,bitrev)
*     call fftprep2(ND,MM,n,phase)
*     (phase() = phase(bitrev()) , if it is needed)
      call fftprep4(ND,MM,n,phase,bitrev)
      write(*,*) 'Testing...'
      u_start=usrtime()
      do i=1, ITE
*     phase = conjg(phase), for DFT
         call fftprep3(ND,MM,phase)
         call fftnd(ND,MM,n,x,cwk,phase,bitrev)
*     phase = conjg(phase), for IDFT
         call fftprep3(ND,MM,phase)
         call fftnd(ND,MM,n,x,cwk,phase,bitrev)
*     x is substituted with 1/2**(n(1)+n(2)+...+n(nd)) times x
         call fftipost(ND,n,x)
      end do
*     phase = conjg(phase), for DFT
c      call fftprep3(ND,MM,phase)
      call fftnd(ND,MM,n,x,cwk,phase,bitrev)
      u_end=usrtime()
      call fftipost(ND,n,x)
      r=1
      j=0
      do i=1, ND
c     IDFT
         if (P(i).ne.0) P(i)=M(i)-P(i)
c
         j=j+P(i)*r
         r=r*M(i)
      end do
c     Input data is single plane wave.
      x(j)=x(j)-(1.d0,0.d0)
c
      emax = 0.d0
      do i=0, NT-1
         y=abs(x(i))
         if( y .gt. emax) then
            j=i
            emax=y
            err=x(i)
         end if
      end do
      r=NT
      do i=ND, 1, -1
         r=r/M(i)
         k=j/r
         j=j-k*r
         PE(i)=k
      end do
      s=2*ITE+1
      t=(u_end-u_start)/s
      q=5*NT*log(dble(NT))/log(2.d0)
      write(*,*) 'N=',M,', M=',s
      write(*,*) 'TIME/M=',t,', MFLOPS=',q/t/1.d6
      if(emax.gt.EPS) then
         write(*,200) (P(i),i=1,ND)
         write(*,201) emax,(PE(i),i=1,ND)
         write(*,202) err
      end if
 200  format(1P,'                 incident:',7I7)
 201  format(1P,'MaxError=',E9.2,', place:',7I7)
 202  format(1P,'   Error=',E9.2)
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

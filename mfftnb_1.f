************************************************************************
*     multiple fft routine without bit-reversal
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors (bit-reversal order)
************************************************************************
      subroutine mfftnb(n,m,z,phase)
*odirr NOALTCODE
      implicit none
      real*8 z(0:*),phase(0:*),iflag
      real*8 ur,ui,vr,vi,xr,xi,yr,yi,wr,wi,s1,c1,s2,c2
      integer i,j,k,n,m,p,q,q2,q3
      p=2
      q=(2**(n-1))*m
      if(n.ge.2) iflag=phase(3)
*     Loop Unrolling, Stride=2
      do i=1, n-1, 2
         q2=q+q
         q3=q2+q
*     Loop Unrolling, Stride=2
         do j=0, p-1, 2
            c1=phase(j)
            s1=phase(j+1)
            c2=phase(2*j)
            s2=phase(2*j+1)
*     phase(2*j+2)=-phase(2*j+1)*phase(3)=-s2*phase(3)
*     phase(2*j+3)= phase(2*j)  *phase(3)= c2*phase(3)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=q2*j, q2*j+q-1, 2
               wr=z(q2+k)*c1-z(q2+k+1)*s1
               wi=z(q2+k)*s1+z(q2+k+1)*c1
               xr=z(  k)  +wr
               ur=z(  k)  -wr
               xi=z(  k+1)+wi
               ui=z(  k+1)-wi
*
               wr=z(q3+k)*c1-z(q3+k+1)*s1
               wi=z(q3+k)*s1+z(q3+k+1)*c1
               yr=z(q+k)  +wr
               vr=z(q+k)  -wr
               yi=z(q+k+1)+wi
               vi=z(q+k+1)-wi
*
               wr       =yr*c2-yi*s2
               wi       =yr*s2+yi*c2
               z(  k)   =xr+wr
               z(q+k)   =xr-wr
               z(  k+1) =xi+wi
               z(q+k+1) =xi-wi
*
               wr       =(vr*s2+vi*c2)*iflag
               wi       =(vr*c2-vi*s2)*iflag
               z(q2+k)  =ur-wr
               z(q3+k)  =ur+wr
               z(q2+k+1)=ui+wi
               z(q3+k+1)=ui-wi
            end do
         end do
         p=p*4
         q=q/4
      end do
      if(i.eq.n)then
         q=m*2
         q2=q+q
         q3=q2+q
         do j=0, 2**n-1, 4
            c1=phase(j)
            s1=phase(j+1)
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=j*q, j*q+q-1, 2
               xr=z(   k)
               xi=z(   k+1)
c
               wr=z(q +k)
               wi=z(q +k+1)
               ur=wr*c1-wi*s1
               ui=wr*s1+wi*c1
               yr=z(q2+k)
               yi=z(q2+k+1)
c
               wr=z(q3+k)
               wi=z(q3+k+1)
               vr=( wr*s1+wi*c1)*iflag
               vi=(-wr*c1+wi*s1)*iflag
               z(   k)  =xr+ur
               z(   k+1)=xi+ui
               z(q +k)  =xr-ur
               z(q +k+1)=xi-ui
               z(q2+k)  =yr-vr
               z(q2+k+1)=yi-vi
               z(q3+k)  =yr+vr
               z(q3+k+1)=yi+vi
            end do
         end do
      end if
      end

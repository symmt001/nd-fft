************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     x:      Input & Output (serialized x(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2 & fftprep3
*     wk:    Work Area
*     bitrev: Bit Reversal Table Prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+...+n(nd))
*     Written by S. Yamamoto
*     
*     Comments:
*     *odir       directive to the optimizer on SX-3   (NEC)
*     *vdir       directive to the compiler  on SX-3   (NEC)
*     *VOPTION    directive to the compiler  on S-3800 (HITACHI)
*     *VOCL       directive to the compiler  on VPP    (FUJITSU)
************************************************************************
      subroutine fftnd(nd,nmax,n,x,wk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd
      integer bitrev(0:nmax-1,nd),n(nd)
      real*8 x(0:*),wk(0:*),phase(0:nmax-1,nd)
      integer axis,i,j,k,l,m,p,q,iside
      real*8 x0,x1,phr,phi
      iside=1
      m=0
      do axis=1, nd
         m=m+n(axis)
      end do
      m=2**m
      do axis=1, nd
         l=2**n(axis)
         q=m/l
         p=l
*     bit reversal
         if(iside.eq.1)then
            call rearr(l,q,x,wk,bitrev(0,axis))
         else
            call rearr(l,q,wk,x,bitrev(0,axis))
         end if
         iside=-iside
*     Loop Unrolling with Stride 2
         do i=1, n(axis)
* actually, next do loop is *POPTION INDEP, but
* inserting POPTION directive causes discouraging result
            do k=0, l-1, p
               phr=phase(k,axis)
               phi=phase(k+1,axis)
               if(iside.eq.1)then
*VOCL LOOP,NOVREC(WK,X)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1, 2
                     x0=x(2*j+2)*phr-x(2*j+3)*phi
                     x1=x(2*j+2)*phi+x(2*j+3)*phr
                     wk(j)    =x(2*j)  +x0
                     wk(j+1)  =x(2*j+1)+x1
                     wk(j+m)  =x(2*j)  -x0
                     wk(j+m+1)=x(2*j+1)-x1
                  end do
               else
*VOCL LOOP,NOVREC(WK,X)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1, 2
                     x0=wk(2*j+2)*phr-wk(2*j+3)*phi
                     x1=wk(2*j+2)*phi+wk(2*j+3)*phr
                     x(j)    =wk(2*j)  +x0
                     x(j+1)  =wk(2*j+1)+x1
                     x(j+m)  =wk(2*j)  -x0
                     x(j+m+1)=wk(2*j+1)-x1
                  end do
               end if
            end do
            iside=-iside
            p=p/2
         end do
      end do
      if(iside.ne.1)then
         do i=0, 2*m-1
            x(i)=wk(i)
         end do
      end if
      end

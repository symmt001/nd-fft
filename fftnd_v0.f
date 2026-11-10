************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension z(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2 & fftprep3
*     cwk:    Work Area
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
      subroutine fftnd(nd,nmax,n,z,cwk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer    nmax,nd
      integer    bitrev(0:nmax-1,nd),n(nd)
      complex*16 z(0:*),cwk(0:*),phase(0:nmax/2-1,nd),ph
      integer    i,j,k,l,m,p,q,axis,iside
c      complex*16 x0,x1
*     "iside" indicates on which site the data is located.
      iside=1
      m=0
      do axis=1, nd
         m=m+n(axis)
      end do
      m=2**(m-1)
      do axis=1, nd
         l=2**(n(axis)-1)
         p=l
         q=m/l
*     bit-reversal
         if(iside.eq.1)then
            call rearr(2*l,q,z,cwk,bitrev(0,axis))
         else
            call rearr(2*l,q,cwk,z,bitrev(0,axis))
         end if
*     exchange z and cwk
         iside=-iside
         do i=1, n(axis)
* actually, next do loop is *POPTION INDEP, but
* inserting POPTION directive causes discouraging result
            do k=0, l-1, p
               ph=phase(k,axis)
*     Doing radix 2 DIT & transpose at the sametime
               if(iside.eq.1)then
CDIR$ IVDEP
CDIR$ VECTOR ALWAYS
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1
c$$$                     x0=z(2*j)
c$$$                     x1=z(2*j+1)*ph
c$$$                     cwk(  j)=x0+x1
c$$$                     cwk(m+j)=x0-x1
c     better on SR-8000
                     cwk(  j)=z(2*j)+z(2*j+1)*ph
                     cwk(m+j)=z(2*j)-z(2*j+1)*ph
                  end do
               else
CDIR$ IVDEP
CDIR$ VECTOR ALWAYS
*VOCL LOOP,NOVREC(CWK,Z)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
                  do j=k*q, (k+p)*q-1
c$$$                     x0=cwk(2*j)
c$$$                     x1=cwk(2*j+1)*ph
c$$$                     z(  j)=x0+x1
c$$$                     z(m+j)=x0-x1
c     better on SR-8000
                     z(  j)=cwk(2*j)+cwk(2*j+1)*ph
                     z(m+j)=cwk(2*j)-cwk(2*j+1)*ph
                  end do
               end if
            end do
            p=p/2
*     exchange z and cwk
            iside=-iside
         end do
      end do
      if (iside.ne.1) then
CDIR$ VECTOR ALWAYS
         do i=0, 2*m-1
            z(i)=cwk(i)
         end do
      end if
      end

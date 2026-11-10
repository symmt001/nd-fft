************************************************************************
*     n-dimensional fft routine
*     Decimation-In-Time Method with small modification
*     z:      Input & Output (serialized z(i1,i2,...,ind))
*     n,nd:   dimension x(2**n(1),2**n(2),...,2**n(nd))
*     phase:  Phase Table Prepared by fftprep2, fftprep3 & fftprep4
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
      implicit none
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      complex*16 z(0:*),cwk(0:*),phase(nmax/2,nd)
      integer i,j,p,q,r,iside
      iside=1
      r=0
      do i=1, nd
         r=r+n(i)
      end do
      r=2**r
      do i=1, nd
         p=2**n(i)
         q=r/p
         do j=0, r-1, p
            if(iside.eq.1)then
               call fft(n(i),z(j),phase(1,i),bitrev(1,i))
            else
               call fft(n(i),cwk(j),phase(1,i),bitrev(1,i))
            end if
         end do
         if(iside.eq.1)then
            call transp(p,q,z,cwk)
         else
            call transp(p,q,cwk,z)
         end if
         iside=-iside
      end do
      if(iside.ne.1)then
         do i=0, r-1
            z(i)=cwk(i)
         end do
      end if
      end
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
************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     using BLAS
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j
      complex*16 dst(0:q*p-1),src(0:q*p-1)
*     For effective use of BLAS, vector length is maxmized.
      if(q.ge.p)then
         do j=0, p-1
            call zcopy(q,src(j*q),1,dst(j),p)
         end do
      else
         do j=0, q-1
            call zcopy(p,src(j),q,dst(j*p),1)
         end do
      end if
      end
************************************************************************
*     decimate once
************************************************************************
      subroutine decn1(p,q,z,phase)
      implicit none
      integer i,j,p,q,ip
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 c,s,wr,wi
*POPTION INDEP
      do i=0, 2*q*p-1, 2*p
         ip=i+p
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do j=0, p-1, 2
            c=phase(j*q)
            s=phase(j*q+1)
            wr=z(ip+j)*c-z(ip+j+1)*s
            wi=z(ip+j)*s+z(ip+j+1)*c
            z(ip+j  )=z(i+j  )-wr
            z(ip+j+1)=z(i+j+1)-wi
            z(i +j  )=z(i+j  )+wr
            z(i +j+1)=z(i+j+1)+wi
         end do
      end do
      end 
************************************************************************
*     decimate once
************************************************************************
      subroutine decn2(p,q,z,phase)
      implicit none
      integer i,j,p,q,ip1,ip2,ip3,q2
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 wr,wi
      real*8 c1,c2,c3,s1,s2,s3
      real*8 wr00,wr01,wr10,wr11
      real*8 wi00,wi01,wi10,wi11
      q2=q/2
*POPTION INDEP
      do i=0, 2*q*p-1, 4*p
         ip1=i+p
         ip2=ip1+p
         ip3=ip2+p
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do j=0, p-1, 2
            c1=phase(j*q)
            s1=phase(j*q+1)
            c2=phase(j*q2)
            s2=phase(j*q2+1)
            c3=phase((j+p)*q2)
            s3=phase((j+p)*q2+1)
            wr=z(ip1+j)*c1-z(ip1+j+1)*s1
            wi=z(ip1+j)*s1+z(ip1+j+1)*c1
            wr00=z(i+j  )+wr
            wi00=z(i+j+1)+wi
            wr01=z(i+j  )-wr
            wi01=z(i+j+1)-wi
            wr=z(ip3+j)*c1-z(ip3+j+1)*s1
            wi=z(ip3+j)*s1+z(ip3+j+1)*c1
            wr10=z(ip2+j  )+wr
            wi10=z(ip2+j+1)+wi
            wr11=z(ip2+j  )-wr
            wi11=z(ip2+j+1)-wi

            wr=wr10*c2-wi10*s2
            wi=wr10*s2+wi10*c2
            z(i+j    )=wr00+wr
            z(i+j+1  )=wi00+wi
            z(ip2+j  )=wr00-wr
            z(ip2+j+1)=wi00-wi
            wr=wr11*c3-wi11*s3
            wi=wr11*s3+wi11*c3
            z(ip1+j  )=wr01+wr
            z(ip1+j+1)=wi01+wi
            z(ip3+j  )=wr01-wr
            z(ip3+j+1)=wi01-wi
         end do
      end do
      end 
************************************************************************
*     decimate once
************************************************************************
      subroutine decn3(p,q,z,phase)
      implicit none
      integer i,j,p,q,ip1,ip2,ip3,ip4,ip5,ip6,ip7,q2,q4
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 wr,wi
      real*8 c1,c2,c3,c4,c5,c6,c7,s1,s2,s3,s4,s5,s6,s7
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      q4=q/4
      q2=q4*2
*POPTION INDEP
      do i=0, 2*q*p-1, 8*p
         ip1=i+p
         ip2=ip1+p
         ip3=ip2+p
         ip4=ip3+p
         ip5=ip4+p
         ip6=ip5+p
         ip7=ip6+p
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
         do j=0, p-1, 2
            c1=phase(j*q)
            s1=phase(j*q+1)
            c2=phase(j*q2)
            s2=phase(j*q2+1)
            c3=phase((j+p)*q2)
            s3=phase((j+p)*q2+1)
            c4=phase(j*q4)
            s4=phase(j*q4+1)
            c5=phase((j+p)*q4)
            s5=phase((j+p)*q4+1)
            c6=phase((j+2*p)*q4)
            s6=phase((j+2*p)*q4+1)
            c7=phase((j+3*p)*q4)
            s7=phase((j+3*p)*q4+1)
            wr=z(ip1+j)*c1-z(ip1+j+1)*s1
            wi=z(ip1+j)*s1+z(ip1+j+1)*c1
            wr000=z(i+j  )+wr
            wi000=z(i+j+1)+wi
            wr001=z(i+j  )-wr
            wi001=z(i+j+1)-wi
            wr=z(ip3+j)*c1-z(ip3+j+1)*s1
            wi=z(ip3+j)*s1+z(ip3+j+1)*c1
            wr010=z(ip2+j  )+wr
            wi010=z(ip2+j+1)+wi
            wr011=z(ip2+j  )-wr
            wi011=z(ip2+j+1)-wi
            wr=z(ip5+j)*c1-z(ip5+j+1)*s1
            wi=z(ip5+j)*s1+z(ip5+j+1)*c1
            wr100=z(ip4+j  )+wr
            wi100=z(ip4+j+1)+wi
            wr101=z(ip4+j  )-wr
            wi101=z(ip4+j+1)-wi
            wr=z(ip7+j)*c1-z(ip7+j+1)*s1
            wi=z(ip7+j)*s1+z(ip7+j+1)*c1
            wr110=z(ip6+j  )+wr
            wi110=z(ip6+j+1)+wi
            wr111=z(ip6+j  )-wr
            wi111=z(ip6+j+1)-wi

            wr=wr010*c2-wi010*s2
            wi=wr010*s2+wi010*c2
            wr010=wr000-wr
            wi010=wi000-wi
            wr000=wr000+wr
            wi000=wi000+wi
            wr=wr011*c3-wi011*s3
            wi=wr011*s3+wi011*c3
            wr011=wr001-wr
            wi011=wi001-wi
            wr001=wr001+wr
            wi001=wi001+wi
            wr=wr110*c2-wi110*s2
            wi=wr110*s2+wi110*c2
            wr110=wr100-wr
            wi110=wi100-wi
            wr100=wr100+wr
            wi100=wi100+wi
            wr=wr111*c3-wi111*s3
            wi=wr111*s3+wi111*c3
            wr111=wr101-wr
            wi111=wi101-wi
            wr101=wr101+wr
            wi101=wi101+wi

            wr=wr100*c4-wi100*s4
            wi=wr100*s4+wi100*c4
            z(i+j  )=wr000+wr
            z(i+j+1)=wi000+wi
            z(ip4+j  )=wr000-wr
            z(ip4+j+1)=wi000-wi
            wr=wr101*c5-wi101*s5
            wi=wr101*s5+wi101*c5
            z(ip1+j  )=wr001+wr
            z(ip1+j+1)=wi001+wi
            z(ip5+j  )=wr001-wr
            z(ip5+j+1)=wi001-wi
            wr=wr110*c6-wi110*s6
            wi=wr110*s6+wi110*c6
            z(ip2+j  )=wr010+wr
            z(ip2+j+1)=wi010+wi
            z(ip6+j  )=wr010-wr
            z(ip6+j+1)=wi010-wi
            wr=wr111*c7-wi111*s7
            wi=wr111*s7+wi111*c7
            z(ip3+j  )=wr011+wr
            z(ip3+j+1)=wi011+wi
            z(ip7+j  )=wr011-wr
            z(ip7+j+1)=wi011-wi
         end do
      end do
      end 
************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx1(p,q,z,phase)
      implicit none
      integer i,j,k,p,q,q2
      integer i0x0,i1x0,i0x1,i1x1
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 c,s,wr0,wi0,wr1,wi1
      q2=2*q
*POPTION INDEP
      do i=0, q2*p-1, 2*q2
*POPTION INDEP
         do k=i, i+q2-1, 2*p
            i0x0=k
            i0x1=i0x0+p
            i1x0=q2+i0x0
            i1x1=q2+i0x1
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1, 2
               c=phase(j*q)
               s=phase(j*q+1)
               wr1=z(i1x1+j)*c-z(i1x1+j+1)*s
               wi1=z(i1x1+j)*s+z(i1x1+j+1)*c
               wr0=z(i1x0+j)*c-z(i1x0+j+1)*s
               wi0=z(i1x0+j)*s+z(i1x0+j+1)*c

               z(i1x1+j  )=z(i0x1+j  )-wr1
               z(i1x1+j+1)=z(i0x1+j+1)-wi1
               z(i1x0+j  )=z(i0x1+j  )+wr1
               z(i1x0+j+1)=z(i0x1+j+1)+wi1

               z(i0x1+j  )=z(i0x0+j  )-wr0
               z(i0x1+j+1)=z(i0x0+j+1)-wi0
               z(i0x0+j  )=z(i0x0+j  )+wr0
               z(i0x0+j+1)=z(i0x0+j+1)+wi0
            end do
         end do
      end do
c      write(*,*)z
      end 
************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx2(p,q,z,phase)
      implicit none
      integer i,j,k,p,q,q2
      integer i00x00,i10x00,i00x01,i10x01
      integer i00x10,i10x10,i00x11,i10x11
      integer i01x00,i11x00,i01x01,i11x01
      integer i01x10,i11x10,i01x11,i11x11
      real*8 z(0:2*q*p-1),phase(0:q*p-1)
      real*8 c1,s1,c2,s2,c3,s3
      real*8 wr,wi
      real*8 wr0000,wi0000,wr0001,wi0001,wr0010,wi0010,wr0011,wi0011
      real*8 wr0100,wi0100,wr0101,wi0101,wr0110,wi0110,wr0111,wi0111
      real*8 wr1000,wi1000,wr1001,wi1001,wr1010,wi1010,wr1011,wi1011
      real*8 wr1100,wi1100,wr1101,wi1101,wr1110,wi1110,wr1111,wi1111
      q2=q/2
c      write(*,*)'X:m,p,q',m,p,q
c      write(*,*)z
*POPTION INDEP
      do i=0, 2*q*p-1, 4*q
*POPTION INDEP
         do k=i, i+q-1, 4*p
            i00x00=k
            i00x01=i00x00+p
            i10x00=2*q+i00x00
            i10x01=2*q+i00x01
            i00x10=i00x00+2*p
            i00x11=i00x01+2*p
            i10x10=i10x00+2*p
            i10x11=i10x01+2*p
            i01x00=i00x00+q
            i01x01=i00x01+q
            i11x00=i10x00+q
            i11x01=i10x01+q
            i01x10=i00x10+q
            i01x11=i00x11+q
            i11x10=i10x10+q
            i11x11=i10x11+q
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1, 2
               c1=phase(j*q)
               s1=phase(j*q+1)
               c2=phase(j*q2)
               s2=phase(j*q2+1)
               c3=phase((p+j)*q2)
               s3=phase((p+j)*q2+1)
               wr=z(i10x00+j)*c1-z(i10x00+j+1)*s1
               wi=z(i10x00+j)*s1+z(i10x00+j+1)*c1
               wr0000=z(i00x00+j  )+wr
               wi0000=z(i00x00+j+1)+wi
               wr0001=z(i00x00+j  )-wr
               wi0001=z(i00x00+j+1)-wi

               wr=z(i10x10+j)*c1-z(i10x10+j+1)*s1
               wi=z(i10x10+j)*s1+z(i10x10+j+1)*c1
               wr0010=z(i00x10+j  )+wr
               wi0010=z(i00x10+j+1)+wi
               wr0011=z(i00x10+j  )-wr
               wi0011=z(i00x10+j+1)-wi

               wr=z(i11x00+j)*c1-z(i11x00+j+1)*s1
               wi=z(i11x00+j)*s1+z(i11x00+j+1)*c1
               wr0100=z(i01x00+j  )+wr
               wi0100=z(i01x00+j+1)+wi
               wr0101=z(i01x00+j  )-wr
               wi0101=z(i01x00+j+1)-wi

               wr=z(i10x01+j)*c1-z(i10x01+j+1)*s1
               wi=z(i10x01+j)*s1+z(i10x01+j+1)*c1
               wr1000=z(i00x01+j  )+wr
               wi1000=z(i00x01+j+1)+wi
               wr1001=z(i00x01+j  )-wr
               wi1001=z(i00x01+j+1)-wi

               wr=z(i11x10+j)*c1-z(i11x10+j+1)*s1
               wi=z(i11x10+j)*s1+z(i11x10+j+1)*c1
               wr0110=z(i01x10+j  )+wr
               wi0110=z(i01x10+j+1)+wi
               wr0111=z(i01x10+j  )-wr
               wi0111=z(i01x10+j+1)-wi

               wr=z(i10x11+j)*c1-z(i10x11+j+1)*s1
               wi=z(i10x11+j)*s1+z(i10x11+j+1)*c1
               wr1010=z(i00x11+j  )+wr
               wi1010=z(i00x11+j+1)+wi
               wr1011=z(i00x11+j  )-wr
               wi1011=z(i00x11+j+1)-wi

               wr=z(i11x01+j)*c1-z(i11x01+j+1)*s1
               wi=z(i11x01+j)*s1+z(i11x01+j+1)*c1
               wr1100=z(i01x01+j  )+wr
               wi1100=z(i01x01+j+1)+wi
               wr1101=z(i01x01+j  )-wr
               wi1101=z(i01x01+j+1)-wi

               wr=z(i11x11+j)*c1-z(i11x11+j+1)*s1
               wi=z(i11x11+j)*s1+z(i11x11+j+1)*c1
               wr1110=z(i01x11+j  )+wr
               wi1110=z(i01x11+j+1)+wi
               wr1111=z(i01x11+j  )-wr
               wi1111=z(i01x11+j+1)-wi

               wr=wr0100*c2-wi0100*s2
               wi=wr0100*s2+wi0100*c2
               z(i00x00+j  )=wr0000+wr
               z(i00x00+j+1)=wi0000+wi
               z(i00x10+j  )=wr0000-wr
               z(i00x10+j+1)=wi0000-wi

               wr=wr0101*c3-wi0101*s3
               wi=wr0101*s3+wi0101*c3
               z(i00x01+j  )=wr0001+wr
               z(i00x01+j+1)=wi0001+wi
               z(i00x11+j  )=wr0001-wr
               z(i00x11+j+1)=wi0001-wi

               wr=wr0110*c2-wi0110*s2
               wi=wr0110*s2+wi0110*c2
               z(i01x00+j  )=wr0010+wr
               z(i01x00+j+1)=wi0010+wi
               z(i01x10+j  )=wr0010-wr
               z(i01x10+j+1)=wi0010-wi

               wr=wr0111*c3-wi0111*s3
               wi=wr0111*s3+wi0111*c3
               z(i01x01+j  )=wr0011+wr
               z(i01x01+j+1)=wi0011+wi
               z(i01x11+j  )=wr0011-wr
               z(i01x11+j+1)=wi0011-wi

               wr=wr1100*c2-wi1100*s2
               wi=wr1100*s2+wi1100*c2
               z(i10x00+j  )=wr1000+wr
               z(i10x00+j+1)=wi1000+wi
               z(i10x10+j  )=wr1000-wr
               z(i10x10+j+1)=wi1000-wi

               wr=wr1101*c3-wi1101*s3
               wi=wr1101*s3+wi1101*c3
               z(i10x01+j  )=wr1001+wr
               z(i10x01+j+1)=wi1001+wi
               z(i10x11+j  )=wr1001-wr
               z(i10x11+j+1)=wi1001-wi

               wr=wr1110*c2-wi1110*s2
               wi=wr1110*s2+wi1110*c2
               z(i11x00+j  )=wr1010+wr
               z(i11x00+j+1)=wi1010+wi
               z(i11x10+j  )=wr1010-wr
               z(i11x10+j+1)=wi1010-wi

               wr=wr1111*c3-wi1111*s3
               wi=wr1111*s3+wi1111*c3
               z(i11x01+j  )=wr1011+wr
               z(i11x01+j+1)=wi1011+wi
               z(i11x11+j  )=wr1011-wr
               z(i11x11+j+1)=wi1011-wi
            end do
         end do
      end do
      end
************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx2f(q,iflag,z)
      implicit none
      integer i,j,q
      integer i00x00,i10x00,i00x01,i10x01
      integer i00x10,i10x10,i00x11,i10x11
      integer i01x00,i11x00,i01x01,i11x01
      integer i01x10,i11x10,i01x11,i11x11
      real*8 z(0:4*q-1),iflag
      real*8 wr0000,wi0000,wr0001,wi0001,wr0010,wi0010,wr0011,wi0011
      real*8 wr0100,wi0100,wr0101,wi0101,wr0110,wi0110,wr0111,wi0111
      real*8 wr1000,wi1000,wr1001,wi1001,wr1010,wi1010,wr1011,wi1011
      real*8 wr1100,wi1100,wr1101,wi1101,wr1110,wi1110,wr1111,wi1111
*POPTION INDEP
      do i=0, 4*q-1, 4*q
         i00x00=0
         i00x01=i00x00+2
         i10x00=2*q+i00x00
         i10x01=2*q+i00x01
         i00x10=i00x00+4
         i00x11=i00x01+4
         i10x10=i10x00+4
         i10x11=i10x01+4
         i01x00=i00x00+q
         i01x01=i00x01+q
         i11x00=i10x00+q
         i11x01=i10x01+q
         i01x10=i00x10+q
         i01x11=i00x11+q
         i11x10=i10x10+q
         i11x11=i10x11+q
         do j=i, i+q-1, 8
            wr1001=z(i00x01+j  )-z(i10x01+j)
            wi1001=z(i00x01+j+1)-z(i10x01+j+1)
            wr1000=z(i00x01+j  )+z(i10x01+j)
            wi1000=z(i00x01+j+1)+z(i10x01+j+1)
            wr0001=z(i00x00+j  )-z(i10x00+j)
            wi0001=z(i00x00+j+1)-z(i10x00+j+1)
            wr0000=z(i00x00+j  )+z(i10x00+j)
            wi0000=z(i00x00+j+1)+z(i10x00+j+1)

            wr1011=z(i00x11+j  )-z(i10x11+j)
            wi1011=z(i00x11+j+1)-z(i10x11+j+1)
            wr1010=z(i00x11+j  )+z(i10x11+j)
            wi1010=z(i00x11+j+1)+z(i10x11+j+1)
            wr0011=z(i00x10+j  )-z(i10x10+j)
            wi0011=z(i00x10+j+1)-z(i10x10+j+1)
            wr0010=z(i00x10+j  )+z(i10x10+j)
            wi0010=z(i00x10+j+1)+z(i10x10+j+1)

            wr1101=z(i01x01+j  )-z(i11x01+j)
            wi1101=z(i01x01+j+1)-z(i11x01+j+1)
            wr1100=z(i01x01+j  )+z(i11x01+j)
            wi1100=z(i01x01+j+1)+z(i11x01+j+1)
            wr0101=z(i01x00+j  )-z(i11x00+j)
            wi0101=z(i01x00+j+1)-z(i11x00+j+1)
            wr0100=z(i01x00+j  )+z(i11x00+j)
            wi0100=z(i01x00+j+1)+z(i11x00+j+1)

            wr1111=z(i01x11+j  )-z(i11x11+j)
            wi1111=z(i01x11+j+1)-z(i11x11+j+1)
            wr1110=z(i01x11+j  )+z(i11x11+j)
            wi1110=z(i01x11+j+1)+z(i11x11+j+1)
            wr0111=z(i01x10+j  )-z(i11x10+j)
            wi0111=z(i01x10+j+1)-z(i11x10+j+1)
            wr0110=z(i01x10+j  )+z(i11x10+j)
            wi0110=z(i01x10+j+1)+z(i11x10+j+1)

            z(i01x10+j  )=wr0010-wr0110
            z(i01x10+j+1)=wi0010-wi0110
            z(i01x00+j  )=wr0010+wr0110
            z(i01x00+j+1)=wi0010+wi0110
            z(i00x10+j  )=wr0000-wr0100
            z(i00x10+j+1)=wi0000-wi0100
            z(i00x00+j  )=wr0000+wr0100
            z(i00x00+j+1)=wi0000+wi0100

            z(i01x11+j  )=wr0011+wi0111*iflag
            z(i01x11+j+1)=wi0011-wr0111*iflag
            z(i01x01+j  )=wr0011-wi0111*iflag
            z(i01x01+j+1)=wi0011+wr0111*iflag
            z(i00x11+j  )=wr0001+wi0101*iflag
            z(i00x11+j+1)=wi0001-wr0101*iflag
            z(i00x01+j  )=wr0001-wi0101*iflag
            z(i00x01+j+1)=wi0001+wr0101*iflag

            z(i11x10+j  )=wr1010-wr1110
            z(i11x10+j+1)=wi1010-wi1110
            z(i11x00+j  )=wr1010+wr1110
            z(i11x00+j+1)=wi1010+wi1110
            z(i10x10+j  )=wr1000-wr1100
            z(i10x10+j+1)=wi1000-wi1100
            z(i10x00+j  )=wr1000+wr1100
            z(i10x00+j+1)=wi1000+wi1100

            z(i11x11+j  )=wr1011+wi1111*iflag
            z(i11x11+j+1)=wi1011-wr1111*iflag
            z(i11x01+j  )=wr1011-wi1111*iflag
            z(i11x01+j+1)=wi1011+wr1111*iflag
            z(i10x11+j  )=wr1001+wi1101*iflag
            z(i10x11+j+1)=wi1001-wr1101*iflag
            z(i10x01+j  )=wr1001-wi1101*iflag
            z(i10x01+j+1)=wi1001+wr1101*iflag
         end do
      end do
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
      integer nd,nmax
      integer n(nd),bitrev(0:nmax-1,nd)
      integer i,j,k,l,p,q
      do i=1, nd
         k=2**n(i)
         do j=0, k-1
            bitrev(j,i)=0
         end do
         p=1
         q=k/2
         do k=1, n(i)
*     tuning for vector machine
            if(q.gt.p)then
               do l=1, 2*p-1, 2
*vdir noloopchg
*SOPTION UNROLL(4)
                  do j=0, q-1
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            else
               do j=0, q-1
*vdir noloopchg
*SOPTION UNROLL(4)
                  do l=1, 2*p-1, 2
*     seting 2**(i-1)
                     bitrev(l*q+j,i)=bitrev(l*q+j,i)+p
                  end do
               end do
            end if
*     p*q=k/2
            p=p*2
            q=q/2
         end do
      end do
      end
************************************************************************
*     preparation for fft
*     set phase table
*     nd:    dimension n(nd)
*     nmax = 2**max(n)
*     phase(0:(nmax/2-1),nd)
************************************************************************
      subroutine fftprep2(nd,nmax,n,phase)
*odirr NOALTCODE
      implicit none
      integer i,j,k,m,nd,n(nd),nmax
      real*8  phase(0:nmax-1,nd)
      real*8  delta,PIF
c     PIF=PI/4
      PIF=atan(1.d0)
      m=1
c     Find max{n}
      do i=2, nd
         if(n(i).gt.n(m)) m=i
      end do
      i=m
c     Calculate phase table for the most large n(i).
      if( n(i)-3 .ge. 0 ) then
         k=2**(n(i)-3)
         delta=PIF/k
         do j=0,k
            phase(2*j  ,i)=cos(j*delta)
            phase(2*j+1,i)=sin(j*delta)
         end do
         m=k*2
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=k+1,m
            phase(2*j  ,i)=phase(2*(m-j)+1,i)
            phase(2*j+1,i)=phase(2*(m-j)  ,i)
         end do
         k=k*4
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=m+1,k-1
            phase(2*j  ,i)=-phase(2*(k-j)  ,i)
            phase(2*j+1,i)= phase(2*(k-j)+1,i)
         end do
      else if ( n(i) .ge. 1 ) then
         phase(0,i)=1.d0
         phase(1,i)=0.d0
           if ( n(i) .eq. 2 ) then
              phase(2,i)=0.d0
              phase(3,i)=1.d0
           end if
      end if
C     Copy phase table to the others.
      k=i
      do i=1, nd
         if(i.ne.k)then
            m=2**(n(k)-n(i))
            do j=0, 2**(n(i)-1)-1
               phase(2*j  ,i)=phase(2*j*m  ,k)
               phase(2*j+1,i)=phase(2*j*m+1,k)
            end do
         end if
      end do
      end
************************************************************************
*     preparation for fft
*     conjugate phase table
*     phase(0:(nmax/2-1),nd)
************************************************************************
      subroutine fftprep3(nd,nmax,phase)
*odirr NOALTCODE
      implicit none
      integer j,nd,nmax
      real*8 phase(nd*nmax)
*vdir noaltcode
      do j=2, nd*nmax, 2
         phase(j)=-phase(j)
      end do
      end
************************************************************************
*     dummy for compatibility problem
************************************************************************
      subroutine fftprep4(nd,nmax,n,phase,bitrev)
      implicit none
      integer nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      complex*16 phase(0:nmax/2-1,nd)
      end
************************************************************************
*     post process for ifft
*     multiply 1/2**(n(1)+n(2)+...+n(nd))
*     dimension x(2**n(1),2**n(2),...,2**n(nd))
************************************************************************
      subroutine fftipost(nd,n,x)
*odirr NOALTCODE
      implicit none
      complex*16 x(*)
      real*8     f
      integer    j,k,nd,n(nd),nsum
      nsum=0
      do j=1, nd
         nsum=nsum+n(j)
      end do
      k=2**nsum
      f=1.d0/k
*vdir noaltcode
      do j=1,k
         x(j)=f*x(j)
      end do
      end

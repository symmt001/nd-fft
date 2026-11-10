************************************************************************
*     N-dimensional fft routine
*     Multiple Call of Decimation-In-Time Method 
*     z:  Input & Output (serialized z(i,j,k))
*     n:  dimension z(2**n(1),2**n(2),2**n(3))
*     phase: phase table prepared by fftprep2 & fftprep3
*     cwk: work area
*     bitrev: bit reversal table prepared by fftprep1
*     To get Inverse-FFT, use fftprep3 to prepare conjugate phase table,
*     and fftipost to multiply the factor 1/2**(n(1)+n(2)+n(3))
*     Written by S. Yamamoto
************************************************************************
      subroutine fftnd(nd,nmax,n,z,cwk,phase,bitrev)
*odirr NOALTCODE
      implicit none
      integer nmax,nd,n(nd),bitrev(nmax,nd)
      complex*16 z(0:*),cwk(0:*),phase(nmax/2,nd)
      integer i,m,p,q,r,iside
      iside=1
      m=0
      do i=1, nd
         m=m+n(i)
      end do
      r=2**m
      do i=nd, 1, -1
         p=2**n(i)
         q=r/p
         if(iside.eq.1)then
            call mfft(n(i),q,z,phase(1,i),bitrev(1,i))
            call transp(q,p,z,cwk)
         else
            call mfft(n(i),q,cwk,phase(1,i),bitrev(1,i))
            call transp(q,p,cwk,z)
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
*     multiple fft routine
*     Decimation-In-Time Method
*     integer n : rank = 2**n
*     integer m : multiplicity
*     integer bitrev : dummy argument for backword compatibility
*     complex*16 z(m,2**n) : data
*     complex*16 phase(2**(n-1)) : phase factors
************************************************************************
      subroutine mfft(n,m,z,phase,bitrev)
      implicit none
      integer n,m,p,q,r,bitrev(*)
      real*8 z(0:2*m*(2**n)-1),phase(0:2**n-1),iflag
      p=2
      q=2**(n-1)
      r=q*p
      iflag=phase(r/2+1)
c     first decimation
      if(q.ge.p*4)then
         call decx2fm(m,q,iflag,z)
         p=p*4
         q=q/4
      end if
 10   continue
         if(q.ge.p*4)then
            call decx2m(m,p,q,z,phase)
            p=p*4
            q=q/4
         else if(q.ge.p)then
            call decx1m(m,p,q,z,phase)
            p=p*2
            q=q/2
         end if
      if (q.ge.p) goto 10
 20   continue
         if(q.ge.4)then
            call decn3m(m,p,q,z,phase)
            p=p*8
            q=q/8
         else if(q.ge.2)then
            call decn2m(m,p,q,z,phase)
            p=p*4
            q=q/4
         else if(q.ge.1)then
            call decn1m(m,p,q,z,phase)
            p=p*2
            q=q/2
         end if
      if (q.ge.1) goto 20
c      end do
      end
************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     This routine is designed especially for the case
*     that both q and p equal to 2**n. But it works on the other case.
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q,j,k
      complex*16 dst(0:q*p-1),src(0:q*p-1)
c     Avoiding Bank Conflict
      if( q.ge.p ) then
*POPTION INDEP
         do k=0, q-p
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, p-1
c$$$               dst((k+j)*p+j)=src(j*q+(k+j))
               dst(k*p+(p+1)*j)=src((q+1)*j+k)
            end do
         end do
*POPTION INDEP
         do k=1, p-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
            end do
         end do
      else
*POPTION INDEP
         do k=0, p-q
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, q-1
c$$$               dst(j*p+(k+j))=src((k+j)*q+j)
               dst((p+1)*j+k)=src(k*q+(q+1)*j)
            end do
         end do
*POPTION INDEP
         do k=1, q-1
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do j=0, k-1
c$$$               dst((q-k+j)*p+j)=src(j*q+(q-k+j))
c$$$               dst(j*p+(p-k+j))=src((p-k+j)*q+j)
               dst((p-k)  +(p+1)*j)=src((p-k)*q+(q+1)*j)
               dst((q-k)*p+(p+1)*j)=src((q-k)  +(q+1)*j)
            end do
         end do
      end if
      end 
************************************************************************
*     decimate once
************************************************************************
      subroutine decn1m(m,p,q,z,phase)
      implicit none
      integer i,j,k,m,p,q,pm
      integer s0,s1
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 c,s,wr,wi
      pm=p*m
*POPTION INDEP
      do i=0, 2*q*pm-1, 2*pm
         do j=0, p-1, 2
            c=phase(j*q)
            s=phase(j*q+1)
            s0=i+j*m
            s1=pm+s0
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=0, 2*m-1, 2
               wr=z(s1+k)*c-z(s1+k+1)*s
               wi=z(s1+k)*s+z(s1+k+1)*c
               z(s1+k  )=z(s0+k  )-wr
               z(s1+k+1)=z(s0+k+1)-wi
               z(s0+k  )=z(s0+k  )+wr
               z(s0+k+1)=z(s0+k+1)+wi
            end do
         end do
      end do
      end 
************************************************************************
*     decimate once
************************************************************************
      subroutine decn2m(m,p,q,z,phase)
      implicit none
      integer i,j,k,m,p,q,pm,q2
      integer s00,s01,s10,s11
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 wr,wi
      real*8 c1,c2,c3,s1,s2,s3
      real*8 wr00,wr01,wr10,wr11
      real*8 wi00,wi01,wi10,wi11
      q2=q/2
      pm=p*m
*POPTION INDEP
      do i=0, 2*q*pm-1, 4*pm
         do j=0, p-1, 2
            c1=phase(j*q)
            s1=phase(j*q+1)
            c2=phase(j*q2)
            s2=phase(j*q2+1)
            c3=phase((j+p)*q2)
            s3=phase((j+p)*q2+1)
            s00=i+j*m
            s01=  pm+s00
            s10=2*pm+s00
            s11=  pm+s10
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
            do k=0, 2*m-1, 2
               wr=z(s01+k)*c1-z(s01+k+1)*s1
               wi=z(s01+k)*s1+z(s01+k+1)*c1
               wr00=z(s00+k  )+wr
               wi00=z(s00+k+1)+wi
               wr01=z(s00+k  )-wr
               wi01=z(s00+k+1)-wi
               wr=z(s11+k)*c1-z(s11+k+1)*s1
               wi=z(s11+k)*s1+z(s11+k+1)*c1
               wr10=z(s10+k  )+wr
               wi10=z(s10+k+1)+wi
               wr11=z(s10+k  )-wr
               wi11=z(s10+k+1)-wi

               wr=wr10*c2-wi10*s2
               wi=wr10*s2+wi10*c2
               z(s00+k  )=wr00+wr
               z(s00+k+1)=wi00+wi
               z(s10+k  )=wr00-wr
               z(s10+k+1)=wi00-wi
               wr=wr11*c3-wi11*s3
               wi=wr11*s3+wi11*c3
               z(s01+k  )=wr01+wr
               z(s01+k+1)=wi01+wi
               z(s11+k  )=wr01-wr
               z(s11+k+1)=wi01-wi
            end do
         end do
      end do
      end 
************************************************************************
*     decimate once
************************************************************************
      subroutine decn3m(m,p,q,z,phase)
      implicit none
      integer i,j,k,m,p,q,pm
      integer s000,s001,s010,s011,s100,s101,s110,s111
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 wr,wi
      real*8 c1,c2,c3,c4,c5,c6,c7,s1,s2,s3,s4,s5,s6,s7
      real*8 wr000,wr001,wr010,wr011,wi000,wi001,wi010,wi011
      real*8 wr100,wr101,wr110,wr111,wi100,wi101,wi110,wi111
      pm=p*m
*POPTION INDEP
      do i=0, 2*q*pm-1, 8*pm
*POPTION INDEP
         do j=0, p-1, 2
            c1=phase(j*q)
            s1=phase(j*q+1)
            c2=phase(j*q/2)
            s2=phase(j*q/2+1)
            c3=phase((j+p)*q/2)
            s3=phase((j+p)*q/2+1)
            c4=phase(j*q/4)
            s4=phase(j*q/4+1)
            c5=phase((j+p)*q/4)
            s5=phase((j+p)*q/4+1)
            c6=phase((j+2*p)*q/4)
            s6=phase((j+2*p)*q/4+1)
            c7=phase((j+3*p)*q/4)
            s7=phase((j+3*p)*q/4+1)
            s000=i+j*m
            s001=  pm+s000
            s010=2*pm+s000
            s011=  pm+s010
            s100=4*pm+s000
            s101=  pm+s100
            s110=2*pm+s100
            s111=  pm+s110
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
*VOPTION INDEP
*POPTION INDEP
            do k=0, 2*m-1, 2
               wr=z(s001+k)*c1-z(s001+k+1)*s1
               wi=z(s001+k)*s1+z(s001+k+1)*c1
               wr000=z(s000+k  )+wr
               wi000=z(s000+k+1)+wi
               wr001=z(s000+k  )-wr
               wi001=z(s000+k+1)-wi
               wr=z(s011+k)*c1-z(s011+k+1)*s1
               wi=z(s011+k)*s1+z(s011+k+1)*c1
               wr010=z(s010+k  )+wr
               wi010=z(s010+k+1)+wi
               wr011=z(s010+k  )-wr
               wi011=z(s010+k+1)-wi
               wr=z(s101+k)*c1-z(s101+k+1)*s1
               wi=z(s101+k)*s1+z(s101+k+1)*c1
               wr100=z(s100+k  )+wr
               wi100=z(s100+k+1)+wi
               wr101=z(s100+k  )-wr
               wi101=z(s100+k+1)-wi
               wr=z(s111+k)*c1-z(s111+k+1)*s1
               wi=z(s111+k)*s1+z(s111+k+1)*c1
               wr110=z(s110+k  )+wr
               wi110=z(s110+k+1)+wi
               wr111=z(s110+k  )-wr
               wi111=z(s110+k+1)-wi

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
               z(s000+k  )=wr000+wr
               z(s000+k+1)=wi000+wi
               z(s100+k  )=wr000-wr
               z(s100+k+1)=wi000-wi
               wr=wr101*c5-wi101*s5
               wi=wr101*s5+wi101*c5
               z(s001+k  )=wr001+wr
               z(s001+k+1)=wi001+wi
               z(s101+k  )=wr001-wr
               z(s101+k+1)=wi001-wi
               wr=wr110*c6-wi110*s6
               wi=wr110*s6+wi110*c6
               z(s010+k  )=wr010+wr
               z(s010+k+1)=wi010+wi
               z(s110+k  )=wr010-wr
               z(s110+k+1)=wi010-wi
               wr=wr111*c7-wi111*s7
               wi=wr111*s7+wi111*c7
               z(s011+k  )=wr011+wr
               z(s011+k+1)=wi011+wi
               z(s111+k  )=wr011-wr
               z(s111+k+1)=wi011-wi
            end do
         end do
      end do
      end 
************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx1m(m,p,q,z,phase)
      implicit none
      integer i,i2,j,k,m,p,q,qm2,pm
      integer i0x0,i1x0,i0x1,i1x1
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 c,s,wr0,wi0,wr1,wi1
c      write(*,*)'X:m,p,q',m,p,q
c      write(*,*)z
      qm2=2*q*m
      pm =  p*m
*POPTION INDEP
      do i=0, qm2*p-1, 2*qm2
*POPTION INDEP
         do i2=i, i+qm2-1, 2*pm
*POPTION INDEP
            do j=0, p-1, 2
               c=phase(j*q)
               s=phase(j*q+1)
               i0x0=i2+j*m
               i0x1=i0x0+pm
               i1x0=qm2+i0x0
               i1x1=qm2+i0x1
c               write(*,*)i0x0,i0x1,i1x0,i1x1
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do k=0, 2*m-1, 2
                  wr1=z(i1x1+k)*c-z(i1x1+k+1)*s
                  wi1=z(i1x1+k)*s+z(i1x1+k+1)*c
                  wr0=z(i1x0+k)*c-z(i1x0+k+1)*s
                  wi0=z(i1x0+k)*s+z(i1x0+k+1)*c

                  z(i1x1+k  )=z(i0x1+k  )-wr1
                  z(i1x1+k+1)=z(i0x1+k+1)-wi1
                  z(i1x0+k  )=z(i0x1+k  )+wr1
                  z(i1x0+k+1)=z(i0x1+k+1)+wi1

                  z(i0x1+k  )=z(i0x0+k  )-wr0
                  z(i0x1+k+1)=z(i0x0+k+1)-wi0
                  z(i0x0+k  )=z(i0x0+k  )+wr0
                  z(i0x0+k+1)=z(i0x0+k+1)+wi0
               end do
            end do
         end do
      end do
c      write(*,*)z
      end 
************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx2m(m,p,q,z,phase)
      implicit none
      integer i,i2,j,k,m,p,q,pm,qm
      integer i00x00,i10x00,i00x01,i10x01
      integer i00x10,i10x10,i00x11,i10x11
      integer i01x00,i11x00,i01x01,i11x01
      integer i01x10,i11x10,i01x11,i11x11
      real*8 z(0:2*q*p*m-1),phase(0:q*p-1)
      real*8 c1,s1,c2,s2,c3,s3
      real*8 wr,wi
      real*8 wr0000,wi0000,wr0001,wi0001,wr0010,wi0010,wr0011,wi0011
      real*8 wr0100,wi0100,wr0101,wi0101,wr0110,wi0110,wr0111,wi0111
      real*8 wr1000,wi1000,wr1001,wi1001,wr1010,wi1010,wr1011,wi1011
      real*8 wr1100,wi1100,wr1101,wi1101,wr1110,wi1110,wr1111,wi1111
c      write(*,*)'X:m,p,q',m,p,q
c      write(*,*)z
      pm=p*m
      qm=q*m
*POPTION INDEP
      do i=0, 2*qm*p-1, 4*qm
*POPTION INDEP
         do i2=i, i+qm-1, 4*pm
            do j=0, p-1, 2
               c1=phase(j*q)
               s1=phase(j*q+1)
               c2=phase(j*q/2)
               s2=phase(j*q/2+1)
               c3=phase((p+j)*q/2)
               s3=phase((p+j)*q/2+1)
               i00x00=i2+j*m
               i00x01=i00x00+pm
               i10x00=2*qm+i00x00
               i10x01=2*qm+i00x01
               i00x10=i00x00+2*pm
               i00x11=i00x01+2*pm
               i10x10=i10x00+2*pm
               i10x11=i10x01+2*pm
               i01x00=i00x00+qm
               i01x01=i00x01+qm
               i11x00=i10x00+qm
               i11x01=i10x01+qm
               i01x10=i00x10+qm
               i01x11=i00x11+qm
               i11x10=i10x10+qm
               i11x11=i10x11+qm
c               write(*,*)i0x0,i0x1,i1x0,i1x1
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
               do k=0, 2*m-1, 2
                  wr=z(i10x00+k)*c1-z(i10x00+k+1)*s1
                  wi=z(i10x00+k)*s1+z(i10x00+k+1)*c1
                  wr0000=z(i00x00+k  )+wr
                  wi0000=z(i00x00+k+1)+wi
                  wr0001=z(i00x00+k  )-wr
                  wi0001=z(i00x00+k+1)-wi

                  wr=z(i10x10+k)*c1-z(i10x10+k+1)*s1
                  wi=z(i10x10+k)*s1+z(i10x10+k+1)*c1
                  wr0010=z(i00x10+k  )+wr
                  wi0010=z(i00x10+k+1)+wi
                  wr0011=z(i00x10+k  )-wr
                  wi0011=z(i00x10+k+1)-wi

                  wr=z(i11x00+k)*c1-z(i11x00+k+1)*s1
                  wi=z(i11x00+k)*s1+z(i11x00+k+1)*c1
                  wr0100=z(i01x00+k  )+wr
                  wi0100=z(i01x00+k+1)+wi
                  wr0101=z(i01x00+k  )-wr
                  wi0101=z(i01x00+k+1)-wi

                  wr=z(i10x01+k)*c1-z(i10x01+k+1)*s1
                  wi=z(i10x01+k)*s1+z(i10x01+k+1)*c1
                  wr1000=z(i00x01+k  )+wr
                  wi1000=z(i00x01+k+1)+wi
                  wr1001=z(i00x01+k  )-wr
                  wi1001=z(i00x01+k+1)-wi

                  wr=z(i11x10+k)*c1-z(i11x10+k+1)*s1
                  wi=z(i11x10+k)*s1+z(i11x10+k+1)*c1
                  wr0110=z(i01x10+k  )+wr
                  wi0110=z(i01x10+k+1)+wi
                  wr0111=z(i01x10+k  )-wr
                  wi0111=z(i01x10+k+1)-wi

                  wr=z(i10x11+k)*c1-z(i10x11+k+1)*s1
                  wi=z(i10x11+k)*s1+z(i10x11+k+1)*c1
                  wr1010=z(i00x11+k  )+wr
                  wi1010=z(i00x11+k+1)+wi
                  wr1011=z(i00x11+k  )-wr
                  wi1011=z(i00x11+k+1)-wi

                  wr=z(i11x01+k)*c1-z(i11x01+k+1)*s1
                  wi=z(i11x01+k)*s1+z(i11x01+k+1)*c1
                  wr1100=z(i01x01+k  )+wr
                  wi1100=z(i01x01+k+1)+wi
                  wr1101=z(i01x01+k  )-wr
                  wi1101=z(i01x01+k+1)-wi

                  wr=z(i11x11+k)*c1-z(i11x11+k+1)*s1
                  wi=z(i11x11+k)*s1+z(i11x11+k+1)*c1
                  wr1110=z(i01x11+k  )+wr
                  wi1110=z(i01x11+k+1)+wi
                  wr1111=z(i01x11+k  )-wr
                  wi1111=z(i01x11+k+1)-wi

                  wr=wr0100*c2-wi0100*s2
                  wi=wr0100*s2+wi0100*c2
                  z(i00x00+k  )=wr0000+wr
                  z(i00x00+k+1)=wi0000+wi
                  z(i00x10+k  )=wr0000-wr
                  z(i00x10+k+1)=wi0000-wi

                  wr=wr0101*c3-wi0101*s3
                  wi=wr0101*s3+wi0101*c3
                  z(i00x01+k  )=wr0001+wr
                  z(i00x01+k+1)=wi0001+wi
                  z(i00x11+k  )=wr0001-wr
                  z(i00x11+k+1)=wi0001-wi

                  wr=wr0110*c2-wi0110*s2
                  wi=wr0110*s2+wi0110*c2
                  z(i01x00+k  )=wr0010+wr
                  z(i01x00+k+1)=wi0010+wi
                  z(i01x10+k  )=wr0010-wr
                  z(i01x10+k+1)=wi0010-wi

                  wr=wr0111*c3-wi0111*s3
                  wi=wr0111*s3+wi0111*c3
                  z(i01x01+k  )=wr0011+wr
                  z(i01x01+k+1)=wi0011+wi
                  z(i01x11+k  )=wr0011-wr
                  z(i01x11+k+1)=wi0011-wi

                  wr=wr1100*c2-wi1100*s2
                  wi=wr1100*s2+wi1100*c2
                  z(i10x00+k  )=wr1000+wr
                  z(i10x00+k+1)=wi1000+wi
                  z(i10x10+k  )=wr1000-wr
                  z(i10x10+k+1)=wi1000-wi

                  wr=wr1101*c3-wi1101*s3
                  wi=wr1101*s3+wi1101*c3
                  z(i10x01+k  )=wr1001+wr
                  z(i10x01+k+1)=wi1001+wi
                  z(i10x11+k  )=wr1001-wr
                  z(i10x11+k+1)=wi1001-wi

                  wr=wr1110*c2-wi1110*s2
                  wi=wr1110*s2+wi1110*c2
                  z(i11x00+k  )=wr1010+wr
                  z(i11x00+k+1)=wi1010+wi
                  z(i11x10+k  )=wr1010-wr
                  z(i11x10+k+1)=wi1010-wi

                  wr=wr1111*c3-wi1111*s3
                  wi=wr1111*s3+wi1111*c3
                  z(i11x01+k  )=wr1011+wr
                  z(i11x01+k+1)=wi1011+wi
                  z(i11x11+k  )=wr1011-wr
                  z(i11x11+k+1)=wi1011-wi
               end do
            end do
         end do
      end do
c      write(*,*)z
      end
************************************************************************
*     decimate once with bit-exchange
************************************************************************
      subroutine decx2fm(m,q,iflag,z)
      implicit none
      integer i2,k,m,q,qm
      integer i00x00,i10x00,i00x01,i10x01
      integer i00x10,i10x10,i00x11,i10x11
      integer i01x00,i11x00,i01x01,i11x01
      integer i01x10,i11x10,i01x11,i11x11
      real*8 z(0:4*q*m-1),iflag
      real*8 wr0000,wi0000,wr0001,wi0001,wr0010,wi0010,wr0011,wi0011
      real*8 wr0100,wi0100,wr0101,wi0101,wr0110,wi0110,wr0111,wi0111
      real*8 wr1000,wi1000,wr1001,wi1001,wr1010,wi1010,wr1011,wi1011
      real*8 wr1100,wi1100,wr1101,wi1101,wr1110,wi1110,wr1111,wi1111
      qm=q*m
*POPTION INDEP
         do i2=0, qm-1, 8*m
               i00x00=i2
               i00x01=i00x00+2*m
               i10x00=2*qm+i00x00
               i10x01=2*qm+i00x01
               i00x10=i00x00+4*m
               i00x11=i00x01+4*m
               i10x10=i10x00+4*m
               i10x11=i10x01+4*m
               i01x00=i00x00+qm
               i01x01=i00x01+qm
               i11x00=i10x00+qm
               i11x01=i10x01+qm
               i01x10=i00x10+qm
               i01x11=i00x11+qm
               i11x10=i10x10+qm
               i11x11=i10x11+qm
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*SOPTION NOUNROLL
*VOPTION INDEP
*POPTION INDEP
               do k=0, 2*m-1, 2
                  wr1001=z(i00x01+k  )-z(i10x01+k)
                  wi1001=z(i00x01+k+1)-z(i10x01+k+1)
                  wr1000=z(i00x01+k  )+z(i10x01+k)
                  wi1000=z(i00x01+k+1)+z(i10x01+k+1)
                  wr0001=z(i00x00+k  )-z(i10x00+k)
                  wi0001=z(i00x00+k+1)-z(i10x00+k+1)
                  wr0000=z(i00x00+k  )+z(i10x00+k)
                  wi0000=z(i00x00+k+1)+z(i10x00+k+1)

                  wr1011=z(i00x11+k  )-z(i10x11+k)
                  wi1011=z(i00x11+k+1)-z(i10x11+k+1)
                  wr1010=z(i00x11+k  )+z(i10x11+k)
                  wi1010=z(i00x11+k+1)+z(i10x11+k+1)
                  wr0011=z(i00x10+k  )-z(i10x10+k)
                  wi0011=z(i00x10+k+1)-z(i10x10+k+1)
                  wr0010=z(i00x10+k  )+z(i10x10+k)
                  wi0010=z(i00x10+k+1)+z(i10x10+k+1)

                  wr1101=z(i01x01+k  )-z(i11x01+k)
                  wi1101=z(i01x01+k+1)-z(i11x01+k+1)
                  wr1100=z(i01x01+k  )+z(i11x01+k)
                  wi1100=z(i01x01+k+1)+z(i11x01+k+1)
                  wr0101=z(i01x00+k  )-z(i11x00+k)
                  wi0101=z(i01x00+k+1)-z(i11x00+k+1)
                  wr0100=z(i01x00+k  )+z(i11x00+k)
                  wi0100=z(i01x00+k+1)+z(i11x00+k+1)

                  wr1111=z(i01x11+k  )-z(i11x11+k)
                  wi1111=z(i01x11+k+1)-z(i11x11+k+1)
                  wr1110=z(i01x11+k  )+z(i11x11+k)
                  wi1110=z(i01x11+k+1)+z(i11x11+k+1)
                  wr0111=z(i01x10+k  )-z(i11x10+k)
                  wi0111=z(i01x10+k+1)-z(i11x10+k+1)
                  wr0110=z(i01x10+k  )+z(i11x10+k)
                  wi0110=z(i01x10+k+1)+z(i11x10+k+1)

                  z(i01x10+k  )=wr0010-wr0110
                  z(i01x10+k+1)=wi0010-wi0110
                  z(i01x00+k  )=wr0010+wr0110
                  z(i01x00+k+1)=wi0010+wi0110
                  z(i00x10+k  )=wr0000-wr0100
                  z(i00x10+k+1)=wi0000-wi0100
                  z(i00x00+k  )=wr0000+wr0100
                  z(i00x00+k+1)=wi0000+wi0100

                  z(i01x11+k  )=wr0011+wi0111*iflag
                  z(i01x11+k+1)=wi0011-wr0111*iflag
                  z(i01x01+k  )=wr0011-wi0111*iflag
                  z(i01x01+k+1)=wi0011+wr0111*iflag
                  z(i00x11+k  )=wr0001+wi0101*iflag
                  z(i00x11+k+1)=wi0001-wr0101*iflag
                  z(i00x01+k  )=wr0001-wi0101*iflag
                  z(i00x01+k+1)=wi0001+wr0101*iflag

                  z(i11x10+k  )=wr1010-wr1110
                  z(i11x10+k+1)=wi1010-wi1110
                  z(i11x00+k  )=wr1010+wr1110
                  z(i11x00+k+1)=wi1010+wi1110
                  z(i10x10+k  )=wr1000-wr1100
                  z(i10x10+k+1)=wi1000-wi1100
                  z(i10x00+k  )=wr1000+wr1100
                  z(i10x00+k+1)=wi1000+wi1100

                  z(i11x11+k  )=wr1011+wi1111*iflag
                  z(i11x11+k+1)=wi1011-wr1111*iflag
                  z(i11x01+k  )=wr1011-wi1111*iflag
                  z(i11x01+k+1)=wi1011+wr1111*iflag
                  z(i10x11+k  )=wr1001+wi1101*iflag
                  z(i10x11+k+1)=wi1001-wr1101*iflag
                  z(i10x01+k  )=wr1001-wi1101*iflag
                  z(i10x01+k+1)=wi1001+wr1101*iflag
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
*     calculating displacement
            bitrev(j,i)=-j
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

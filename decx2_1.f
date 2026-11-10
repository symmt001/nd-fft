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

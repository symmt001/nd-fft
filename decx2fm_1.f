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

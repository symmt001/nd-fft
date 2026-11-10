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

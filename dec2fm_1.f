************************************************************************
*     decimate twice, only for the first decimation
************************************************************************
      subroutine dec2fm(m,q,z,phase)
      implicit none
      real*8 z(0:*),phase(0:*)
      real*8 wr,wi,iflag
      real*8 wr00,wr01,wr10,wr11,wi00,wi01,wi10,wi11
      integer k,m,q,qm
      integer s00,s01,s10,s11
      iflag=phase(3)
      qm=q*m
      s00=0
      s01=qm+s00
      s10=qm+s01
      s11=qm+s10
*VOCL LOOP,NOVREC(Z)
*VOCL LOOP,NOVDOPT
*vdir nodep
*vdir noloopchg
*VOPTION INDEP
*POPTION INDEP
      do k=0, qm-1, 2
         wr=z(s10+k)
         wi=z(s10+k+1)
         wr00=z(s00+k  )+wr
         wi00=z(s00+k+1)+wi
         wr10=z(s00+k  )-wr
         wi10=z(s00+k+1)-wi
         wr=z(s11+k)
         wi=z(s11+k+1)
         wr01=z(s01+k  )+wr
         wi01=z(s01+k+1)+wi
         wr11=z(s01+k  )-wr
         wi11=z(s01+k+1)-wi

         z(s00+k  )=wr00+wr01
         z(s00+k+1)=wi00+wi01
         z(s01+k  )=wr00-wr01
         z(s01+k+1)=wi00-wi01
         wi= wr11*iflag
         wr=-wi11*iflag
         z(s10+k  )=wr10+wr
         z(s10+k+1)=wi10+wi
         z(s11+k  )=wr10-wr
         z(s11+k+1)=wi10-wi
      end do
      end

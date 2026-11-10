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

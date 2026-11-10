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

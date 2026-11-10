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

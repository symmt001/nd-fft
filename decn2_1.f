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

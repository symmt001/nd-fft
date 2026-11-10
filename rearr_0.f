************************************************************************
*     rearrange complex data as follows
*     src(bitrev(p),q)->dst(p,q)
*     Caution! Not compatible with rearr_1.f
*     This routine must be used with fftprep1_0.f
************************************************************************
      subroutine rearr(p,q,src,dst,bitrev)
      implicit none
      integer p,q,i,j
      integer bitrev(0:p-1)
      complex*16 src(0:p-1,q),dst(0:p-1,q)
c$$$      integer p2,p3,p4,p5,p6,p7,p8,k
c$$$      if (p.lt.8) then
c     Original Scheme
*POPTION INDEP
         do j=1, q
CDIR$ IVDEP
*VOCL LOOP,NOVREC(SRC,DST)
*VOCL LOOP,NOVDOPT
*vdir noassume
*vdir noaltcode
*VOPTION INDEP
*POPTION INDEP
            do i=0, p-1
               dst(i,j)=src(bitrev(i),j)
            end do
         end do
* Not Good
c$$$      else
c$$$         p8=p/8
c$$$         p4=p8*2
c$$$         p2=p4*2
c$$$         p3=p2+p4
c$$$         p5=p2+p8
c$$$         p6=p4+p8
c$$$         p7=p3+p8
c$$$c unrolling
c$$$*POPTION INDEP
c$$$*SOPTION NOUNROLL
c$$$         do j=1, q
c$$$*VOCL LOOP,NOVREC(SRC,DST)
c$$$*VOCL LOOP,NOVDOPT
c$$$*vdir noassume
c$$$*vdir noaltcode
c$$$*VOPTION INDEP
c$$$*POPTION INDEP
c$$$            do i=0, p8-1
c$$$               k=bitrev(i)
c$$$               dst(i,j)=src(k,j)
c$$$               dst(p2+i,j)=src(k+1,j)
c$$$               dst(p4+i,j)=src(k+2,j)
c$$$               dst(p3+i,j)=src(k+3,j)
c$$$               dst(p8+i,j)=src(k+4,j)
c$$$               dst(p5+i,j)=src(k+5,j)
c$$$               dst(p6+i,j)=src(k+6,j)
c$$$               dst(p7+i,j)=src(k+7,j)
c$$$            end do
c$$$         end do
c$$$      end if
      end

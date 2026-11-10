************************************************************************
*     transpose [complex*16 src(q,p)] -> [complex*16 dst(p,q)]
*     p,q should equivalent to 0 (mod 32)
************************************************************************
      subroutine transp(q,p,src,dst)
      implicit none
      integer p,q, j,k,jj, jq, jqq, kp, kpp, NRL
      parameter(NRL=8)
      complex*16 dst(0:q*p-1),src(0:q*p-1)
c      complex*16 buf(0:NRL-1,0:NRL-1)
      do j=0, p-NRL, NRL
         jq=j*q
         do k=0, q-NRL, NRL
            kp=k*p+j
            jqq=jq+k
            do jj=0, NRL-1
               kpp=kp+jj
               dst(kpp)=src(jqq   )
               kpp=kpp+p
               dst(kpp)=src(jqq+1 )
               kpp=kpp+p
               dst(kpp)=src(jqq+2 )
               kpp=kpp+p
               dst(kpp)=src(jqq+3 )
               kpp=kpp+p
               dst(kpp)=src(jqq+4 )
               kpp=kpp+p
               dst(kpp)=src(jqq+5 )
               kpp=kpp+p
               dst(kpp)=src(jqq+6 )
               kpp=kpp+p
               dst(kpp)=src(jqq+7 )
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+8 )
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+9 )
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+10)
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+11)
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+12)
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+13)
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+14)
c$$$               kpp=kpp+p
c$$$               dst(kpp)=src(jqq+15)
               jqq=jqq+q
            end do
         end do
      end do
      end

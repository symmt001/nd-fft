************************************************************************
*     fft routine
*     Decimation-In-Time Method
************************************************************************
      subroutine fft(n,z,phase,bitrev)
      implicit none
      complex*16 z(0:*),phase(0:*),x,y
      integer bitrev(0:*),i,j,j2,k,n,p,q
      k=2**n
*     bit reversal
      do 10 i=0, k-1
         p=bitrev(i)
         if (p.gt.i) then
            y   =z(p)
            z(p)=z(i)
            z(i)=y
         end if
 10   continue
      p=1
      q=k/2
      do 20 i=1, n
         if(p.ge.q)then
            do 30 j2=0, q-1
               do 40 j=0, p-1
                  k=j2*p*2+j
                  y=z(k+p)*phase(j*q)
                  z(k+p)=z(k)-y
                  z(k)  =z(k)+y
 40            continue
 30         continue
         else
            do 50 j=0, p-1
               x=phase(j*q)
               do 60 j2=0, q-1
                  k=j2*p*2+j
                  y=z(k+p)*x
                  z(k+p)=z(k)-y
                  z(k)  =z(k)+y
 60            continue
 50         continue
         end if
         p=p*2
         q=q/2
 20   continue
      end

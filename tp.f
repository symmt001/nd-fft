c
c
      program main
      implicit none
      integer i, j, p, q, k, l, r, s, c
      parameter(p=7, q=3)
      logical fin(0:p-1,0:q-1)
      fin=.false.
      do j=0, q-1
         do i=0, p-1
            if(fin(i,j)) cycle
            k=i
            l=j
            s=j*p+i
            c=0
            do while(.true.)
               fin(k,l)=.true.
               r=k*q+l
               write(*,*) k,l
               c=c+1
               if(r.eq.s) exit
               l=r/p
               k=r-l*p
            end do
            write(*,*)'Summary: s=',s, '#=',c
         end do
      end do
      end
               
               
               

            
            
      

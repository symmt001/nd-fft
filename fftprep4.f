************************************************************************
*     preparation for fft
*     bitreversal phase table
************************************************************************
      subroutine fftprep4(nd,nmax,n,phase,bitrev)
      implicit none
      integer i,j,k,m,nd,n(nd),nmax,bitrev(0:nmax-1,nd)
      complex*16 phase(0:nmax/2-1,nd),c
c      write(*,'(2F10.4)') (phase(p,1),p=0,15)
      do i=1, nd
         k=2**(n(i)-1)
*VOCL LOOP,NOVREC(PHASE)
*VOCL LOOP,NOVDOPT
*vdir noaltcode
*vdir nodep
*VOPTION INDEP
*POPTION INDEP
         do j=0, k-1
            m=bitrev(j*2,i)
            if(m.gt.j) then
               c=phase(m,i)
               phase(m,i)=phase(j,i)
               phase(j,i)=c
            end if
         end do
      end do
c      write(*,'(I4,2F10.4)') (bitrev(i*2,1),phase(i,1),i=0,15)
      end

      program main
      
      complex*16 a,b
      a=dcmplx(cos(1.d0),sin(1.d0))
      b=dcmplx(cos(2.d0),sin(2.d0))
      write(*,*) a*b
      end

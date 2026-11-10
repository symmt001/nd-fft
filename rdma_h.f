C     Remote DMA related variables
      integer MINNP,MAXNP,ROOT,LBUF
      parameter(MINNP=2**0,MAXNP=2**10,ROOT=0)
      parameter(LBUF=NX*NY*NZ/MINNP)
      integer ierr,np,myid
      integer,dimension(2,0:MAXNP-1):: rdesc
      integer IKEY
      parameter(IKEY=111)
      integer msg(4),inode(4),desc(0:NZ*2-1),iobj,q,t,l,lchunk
      equivalence (inode(1),myid),(inode(2),np)
      complex(kind=8),dimension(0:NX-1,0:NY-1,0:NZ/MINNP-1)::w,z
      common /FFT_RDMA/msg,inode,desc,rdesc,iobj,q,t,l,lchunk
      common /FFT_CBUF/w,z
*COPTION COMBUF IKEY :: /FFT_CBUF/

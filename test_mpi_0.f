      implicit none
      include 'mpif.h'
      integer myid,ierr,np,i,itag
      integer,allocatable,dimension(:)  :: REQS,REQR
      integer,allocatable,dimension(:,:):: STAT
      integer,allocatable,dimension(:)  :: iw,jw
c      real(kind=8),dimension(0:1):: iw,jw
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, np, ierr)
      allocate(iw(0:np-1),jw(0:np-1))
      allocate(REQR(0:np-1),REQS(0:np-1))
      allocate(STAT(MPI_STATUS_SIZE,0:np-2))
      do i=0,np-1
         iw(i)=myid*np+i
         jw(i)=-1
      end do
      write(*,*) myid,':',iw
c      goto 999
      do i=0, np-1
         itag=myid*np+i
         call MPI_iSend(iw(i),1,MPI_INTEGER,i,
     $        itag, MPI_COMM_WORLD, REQS(i), ierr)
      end do
      do i=0, np-1
         itag=i*np+myid
         call MPI_iRecv(jw(i),1,MPI_INTEGER,i,
     $        itag, MPI_COMM_WORLD, REQR(i), ierr)
      end do
      call MPI_Waitall(np,REQR,STAT,ierr)
      call MPI_Waitall(np,REQS,STAT,ierr)
c$$$      call MPI_Alltoall(iw,1,MPI_INTEGER,jw,1,MPI_INTEGER,
c$$$     $     MPI_COMM_WORLD,ierr)

      write(*,*) myid,'|',jw
 999  call MPI_Finalize(ierr)
      end

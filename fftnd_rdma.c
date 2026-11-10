#include <unistd.h>
#include <hxb/combuf.h>
#include <hxb/combuf_returns.h>

typedef struct { double re,im ; } COMPLEX;
typedef struct { int myid, np ; } RDMA_INFO;

#define min(x,y) (x<y)?x:y
#define max(x,y) (x>y)?x:y

void
FFTND_RDMA_INIT(d,n,z,w,rdma_info)
int *d, n[];
COMPLEX *z, *w;
RDMA_INFO rdma_info;
{


}

void
FFTND_RDMA(d,nmax,n,z,w,phase,bitrev,rdma_info)
int *d, *nmax, n[], bitrev[];
COMPLEX z[], w[], phase[];
RDMA_INFO *rdma_info; 
{
/*
      complex(kind=8),dimension(0:nmax/2-1,d)::phase
*/
COMPLEX u;
int *REQR,*REQS,*STAT;
int i,j,jj,k,m,l,q,t,lchunk,nw,latency,lb,ir;
int ierr;

      if(d<1)
		{
		  printf("incompatible parameter");
		  exit(1);
		}

      j=0;
      for (i=0; i<d-1;i++) j=j+n[i];
      q=1<<j;
      t=1<<n[d];
      lchunk=q/np;
      nw=lchunk*t;
      l=t/np;

/*     tunable parameters */
      latency=4;
      latency=min(l,latency);

/*
      allocate(REQR(0:t-1),REQS(0:t-1))
      allocate(STAT(MPI_STATUS_SIZE*max(t,np)))
*/
      u=malloc(sizeof(COMPLEX)*q);
      
      for( i=0; i<l ; ++i)
		{
         for( j=0; j<np ; ++j)
		   {
			 jj = myid ^ j;

               m=i*np+jj
               k=jj*l+i
               ir=i*np


               call MPI_Ssend_init
     $              (z(m*nb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQS(ir+j),ierr)
               call MPI_Recv_init
     $              (w(k*nb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQR(ir+j),ierr)

               end do

         end do

         call fftnd(d-1,nmax,n,z(i*q),u,phase,bitrev)

         call MPI_Startall(npb,REQS(i*npb),ierr)
         call MPI_Startall(npb,REQR(i*npb),ierr)

         if(i.ge.latency)then
            call MPI_Waitall (npb,REQS((i-latency)*npb),STAT,ierr)
            call MPI_Waitall (npb,REQR((i-latency)*npb),STAT,ierr)
         end if

      end do

      do i=l-latency, l-1
         call MPI_Waitall (npb,REQS(i*npb),STAT,ierr)
         call MPI_Waitall (npb,REQR(i*npb),STAT,ierr)
      end do

c     tunable parameter
c
      latency=1
      latency=min(nb,latency)
c

      do ib=0, nb-1

         do i=0, l-1

            ir=ib*t+i

            do j=0, np-1

               jj=ieor(myid,j)
               k=ib*t+bitrev(jj*l+i,d)
               m=i*npb+jj*nb+ib

               call MPI_Ssend_init
     $              (w(k*lb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQS(ir+j*l),ierr)
               call MPI_Recv_init
     $              (z(m*lb),lb,MPI_DOUBLE_COMPLEX,
     $              jj,ir,COMM,REQR(ir+j*l),ierr)

            end do

         end do

         call mfftnb(n(d),lb,w(ib*t*lb),phase(0,d))

         call MPI_Startall(t,REQS(ib*t),ierr)
         call MPI_Startall(t,REQR(ib*t),ierr)

         if(ib.ge.latency)then
            call MPI_Waitall (t,REQS((ib-latency)*t),STAT,ierr)
            call MPI_Waitall (t,REQR((ib-latency)*t),STAT,ierr)
         end if

      end do

      do ib=nb-latency, nb-1
         call MPI_Waitall (t,REQS(ib*t),STAT,ierr)
         call MPI_Waitall (t,REQR(ib*t),STAT,ierr)
      end do

c      do i=1, np-1
c         call MPI_Comm_free(COMM(i),ierr)
c      end do

      deallocate(u,COMM,REQR,REQS,STAT)

      end

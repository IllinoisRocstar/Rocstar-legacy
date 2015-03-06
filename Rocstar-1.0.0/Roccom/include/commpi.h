/* *******************************************************************
 * Rocstar Simulation Suite                                          *
 * Copyright@2015, Illinois Rocstar LLC. All rights reserved.        *
 *                                                                   *
 * Illinois Rocstar LLC                                              *
 * Champaign, IL                                                     *
 * www.illinoisrocstar.com                                           *
 * sales@illinoisrocstar.com                                         *
 *                                                                   *
 * License: See LICENSE file in top level of distribution package or *
 * http://opensource.org/licenses/NCSA                               *
 *********************************************************************/
/* *******************************************************************
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   *
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES   *
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND          *
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE CONTRIBUTORS OR           *
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   *
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE    *
 * USE OR OTHER DEALINGS WITH THE SOFTWARE.                          *
 *********************************************************************/
// $Id: commpi.h,v 1.20 2008/12/06 08:43:24 mtcampbe Exp $

/** @file commpi.h
 *  Contains declarations of MPI subroutines used in Roccom.
 *  If DUMMY_MPI is not define, it includes the true mpi.h.
 *  Otherwise, it provides a dummy MPI interface for serial codes. 
 *  @see commpi.C
 */

#ifndef __COM_MPI__
#define __COM_MPI__

#ifndef DUMMY_MPI /* Whether to use the serial dummy MPI */

//#define MPICH_IGNORE_CXX_SEEK
#include <mpi.h>

#else  /* Define the serial dummy MPI */

#include "roccom_basic.h"
#include <cstdlib>
#include <sys/time.h>

typedef int      MPI_Comm;
typedef int      MPI_Request;
typedef int      MPI_Status;
typedef int      MPI_Op;
typedef int      MPI_Datatype;

enum { MPI_COMM_NULL=-1};
enum { MPI_SUM=1, MPI_PROD, MPI_MIN, MPI_MAX, MPI_BOR, 
       MPI_BAND, MPI_LOR, MPI_LAND,
       MPI_DOUBLE_INT,MPI_MINLOC,
       MPI_MAXLOC, MPI_COMM_WORLD, MPI_COMM_SELF,
       MPI_BYTE=COM_BYTE, MPI_CHAR=COM_CHAR, MPI_INT=COM_INT, 
       MPI_FLOAT=COM_FLOAT, MPI_DOUBLE=COM_DOUBLE};

/** @name Initialization and finalization 
 *  @{ */
/// Initialize the MPI execution environment
extern int MPI_Init( int*, char***);
/// Indicates  whether  MPI_Init has been called.
inline int MPI_Initialized( int *flag) { *flag=1; return 0; }
/// Terminates MPI execution environment
extern int MPI_Finalize();
/// Terminates MPI execution environment
inline int MPI_Abort( MPI_Comm, int errorcode) { std::abort(); return 0; }
//@}

/** @name Communicator 
 * @{ */
/// Determines the size of the group associ­ated with a communictor
inline int MPI_Comm_size( MPI_Comm, int *size) { *size = 1; return 0; }
/// Determines the rank of the calling pro­cess in the communicator
inline int MPI_Comm_rank( MPI_Comm, int *rank) { *rank = 0; return 0; } 

inline MPI_Comm MPI_Comm_c2f( MPI_Comm comm) { return comm; }
inline MPI_Comm MPI_Comm_f2c( MPI_Comm comm) { return comm; }
//@}

/** @name Timing
 * @{ */
/// Get wall clock time
inline double MPI_Wtime() {
  ::timeval tv;
  gettimeofday( &tv, NULL);
  
  return tv.tv_sec + tv.tv_usec*1.e-6;
}

inline void MPI_Pcontrol( int level, ...) {}

/** @name Global synchronization and communication
 * @{ */
/// Blocks until all process have reached this routine
inline int MPI_Barrier( MPI_Comm) { return 0; }

///  Broadcasts  a message from the process with
///  rank "root" to all other processes of the group.
inline int MPI_Bcast( void *buffer, int count, MPI_Datatype datatype, 
		      int root, MPI_Comm comm) { return 0; }

/// Combines  values from all processes and
/// distribute the result back to all processes
extern int MPI_Allreduce( void *sendbuf, void *recvbuf, int count,
			  MPI_Datatype datatype, MPI_Op op, MPI_Comm comm );

/// Gathers data from all tasks and deliver it to all
extern int MPI_Allgather( void *sendbuf, int sendcount, MPI_Datatype sendtype,
			  void *recvbuf, int recvcount, MPI_Datatype recvtype,
			  MPI_Comm comm );

/// Gathers data from all tasks and deliver it to all, with a displacement
extern int MPI_Allgatherv ( void *sendbuf, int sendcount, 
			    MPI_Datatype sendtype,
			    void *recvbuf, int *recvcounts, int *displs,
			    MPI_Datatype recvtype, MPI_Comm comm );

/// Sends  data  from all to all processes
extern int MPI_Alltoall( void *sendbuf, int sendcount, MPI_Datatype sendtype,
			 void *recvbuf, int recvcnt, MPI_Datatype recvtype,
			 MPI_Comm comm );

/// Sends  data  from all to all processes, with a displacement
extern int MPI_Alltoallv( void *sendbuf, int *sendcount, 
			  int *senddispls, MPI_Datatype sendtype,
			  void *recvbuf, int *recvcounts, int *recvdispls,
			  MPI_Datatype recvtype, MPI_Comm comm );
//@}

/** @name Nonblocking communicaiton
 * @{
 */
/// Begins a nonblocking send
extern int MPI_Isend( void *buf, int count, MPI_Datatype datatype, 
		      int dest, int tag,
		      MPI_Comm comm, MPI_Request *request );

/// Begins a nonblocking receive
extern int MPI_Irecv( void *buf, int count, MPI_Datatype datatype, 
		      int src, int tag,
		      MPI_Comm comm, MPI_Request *request );

///  Waits for all given communications to complete
inline int MPI_Waitall( int count,
			MPI_Request array_of_requests[],
			MPI_Status array_of_statuses[] ) 
{ return 0; }

///  Waits for any given communications to complete
inline int MPI_Waitany( int count,
			MPI_Request array_of_requests[],
			int *index,
			MPI_Status *status ) 
{ *index=0; return 0; }
//@}

#endif /* DUMMY_MPI */

inline int COMMPI_Comm_rank( MPI_Comm c) 
{ int rank; MPI_Comm_rank( c, &rank); return rank; } 

inline int COMMPI_Comm_size( MPI_Comm c) 
{ int size; MPI_Comm_size( c, &size); return size; } 

inline int COMMPI_Initialized() 
{ int flag; MPI_Initialized( &flag); return flag; } 

/** @name Nonblocking communicaiton
 * @{
 */

/// Begins a nonblocking send
extern int COMMPI_Isend( void *buf, int count, MPI_Datatype datatype, 
			 int dest, int tag,
			 MPI_Comm comm, MPI_Request *request );
//@}


/// Begins a nonblocking receive
extern int COMMPI_Irecv( void *buf, int count, MPI_Datatype datatype, 
			 int src, int tag,
			 MPI_Comm comm, MPI_Request *request );

// Not all MPI implementations support MPI_Comm_f2c and MPI_Comm_c2f.
// Assume C and F communicators are compatible if the C type is int.
template <class T>
inline int COMMPI_Comm_c2f( T t) 
{ return MPI_Comm_c2f( t); }

template <>
inline int COMMPI_Comm_c2f( int t) 
{ return t; }

template <>
inline int COMMPI_Comm_c2f( unsigned int t) 
{ return t; }

template <class T>
inline T COMMPI_Comm_f2c( int c, T) 
{ return MPI_Comm_f2c( c); }

template <>
inline int COMMPI_Comm_f2c( int t, int) 
{ return t; }

template <>
inline unsigned int COMMPI_Comm_f2c( int t, unsigned int) 
{ return t; }

#endif /* __MY_MPI__ */







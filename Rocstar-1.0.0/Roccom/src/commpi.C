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
// $Id: commpi.C,v 1.14 2009/01/22 23:56:54 gzagaris Exp $

/** @file commpi.C
 *  Contains a dummy implementation of MPI subroutines for one thread.
 *  If DUMMY_MPI is not define, it does produce anything.
 *  Note: This implementation is not thread-safe and it need not to be
 *  because it is only intended for a placeholder of MPI for one thread.
 *  It works even if MPI_Init was not called.
 *  @see commpi.h
 */

#include "commpi.h"
#include <cstring>
#include <cstdlib>
#include <iostream>
#include <map>

/// A map from message tags to data addresses
typedef std::map<int, void*>       Msg_Queue;

/// Get the size of a given MPI data type.
static int get_sizeof( MPI_Datatype i) throw(int) {
  if(i == MPI_DOUBLE)
    return(sizeof(double));
  else if(i == MPI_FLOAT)
    return(sizeof(float));
  else if(i == MPI_INT)
    return(sizeof(int));
  else if(i == MPI_BYTE)
    return(sizeof(char));
  else if(i == MPI_CHAR)
    return(sizeof(char));
  else 
    throw(-1);
}
//   switch (i) {
//   case MPI_DOUBLE: return sizeof(double);
//   case MPI_FLOAT:  return sizeof(float);
//   case MPI_INT:    return sizeof(int);
//   case MPI_BYTE:   return sizeof(char);
//   case MPI_CHAR:   return sizeof(char);
//   default: throw(-1);
//   }
// }

int COM_send( Msg_Queue &sendQ, Msg_Queue &recvQ,
	      void *buf, int count, MPI_Datatype datatype, int tag) {
  try {
    Msg_Queue::iterator it;
    if ( (it=recvQ.find( tag)) != recvQ.end()) {
      int extent=count*get_sizeof(datatype);
      std::memcpy( it->second, (char*)buf, extent);
      recvQ.erase(it);
    }
    else
      sendQ[tag] = buf;
  }
  catch (int) {
    std::cerr << "Unsupported data type " << datatype 
	      << " used for MPI_Isend" << std::endl;
    std::abort();
  }
  return 0;
}

int COM_recv( Msg_Queue &sendQ, Msg_Queue &recvQ,
	      void *buf, int count, MPI_Datatype datatype, int tag ) {
  try {
    Msg_Queue::iterator it;
    if ( (it=sendQ.find( tag)) != sendQ.end()) {
      int extent=count*get_sizeof(datatype);
      std::memcpy( buf, it->second, extent);
      sendQ.erase(it);
    }
    else
      recvQ[tag] = buf;
  }
  catch (int) {
    std::cerr << "Unsupported data type " << datatype 
	      << " used for MPI_Irecv" << std::endl;
    std::abort();
  }
  return 0;
}

static Msg_Queue  *sendQ=NULL;
static Msg_Queue  *recvQ=NULL;

/// Begins a nonblocking send
int COMMPI_Isend( void *buf, int count, MPI_Datatype datatype, 
		  int dest, int tag,
		  MPI_Comm comm, MPI_Request *request ) {
#ifndef DUMMY_MPI
  if ( COMMPI_Initialized())
    return MPI_Isend( buf, count, datatype, dest, tag, comm, request);
#endif
  try {
    if ( sendQ == NULL) sendQ = new Msg_Queue();
    if ( recvQ == NULL) recvQ = new Msg_Queue();
  }
  catch (...) {
    std::cerr << "Out of memory" << std::endl;
    std::abort();
  }
  return COM_send( *sendQ, *recvQ, buf, count, datatype, tag); 
}

/// Begins a nonblocking receive
int COMMPI_Irecv( void *buf, int count, MPI_Datatype datatype, 
		  int src, int tag,
		  MPI_Comm comm, MPI_Request *request ) { 
#ifndef DUMMY_MPI
  if ( COMMPI_Initialized())
    return MPI_Irecv( buf, count, datatype, src, tag, comm, request);
#endif
  try {
    if ( sendQ == NULL) sendQ = new Msg_Queue();
    if ( recvQ == NULL) recvQ = new Msg_Queue();
  }
  catch (...) {
    std::cerr << "Out of memory" << std::endl;
    std::abort();
  }
  return COM_recv( *sendQ, *recvQ, buf, count, datatype, tag);
}

#ifdef DUMMY_MPI

/// Begins a nonblocking send
int MPI_Isend( void *buf, int count, MPI_Datatype datatype, 
	       int dest, int tag,
	       MPI_Comm comm, MPI_Request *request ) {
  return COMMPI_Isend( buf, count, datatype, dest, tag, comm, request);
}

/// Begins a nonblocking receive
int MPI_Irecv( void *buf, int count, MPI_Datatype datatype, 
	       int src, int tag,
	       MPI_Comm comm, MPI_Request *request ) { 
  return COMMPI_Irecv( buf, count, datatype, src, tag, comm, request);
}

int MPI_Init( int*, char***) { 
  return 0;
}

int MPI_Finalize() { return 0; 
 if ( sendQ != NULL) { delete sendQ; sendQ = NULL; }
 if ( recvQ != NULL) { delete recvQ; recvQ = NULL; }
 return 0;
}

int MPI_Allreduce( void *sendbuf, void *recvbuf, int count,
		   MPI_Datatype datatype, MPI_Op op, MPI_Comm comm ) {
  try {
    std::memcpy( recvbuf, sendbuf, count*get_sizeof(datatype));
  }
  catch (int) {
    std::cerr << "Unsupported data type " << datatype 
	      << " used for MPI_Allreduce" << std::endl;
    std::abort();
  }
    return 0;
}

int MPI_Allgather( void *sendbuf, int sendcount, MPI_Datatype sendtype,
		   void *recvbuf, int recvcount, MPI_Datatype recvtype,
		   MPI_Comm comm ) {
  try {
    std::memcpy( recvbuf, sendbuf, sendcount*get_sizeof(sendtype));
  }
  catch (int) {
    std::cerr << "Unsupported data type " << sendtype 
	      << " used for MPI_Allgather" << std::endl;
    std::abort();
  }
  return 0;
}

int MPI_Allgatherv ( void *sendbuf, int sendcount, MPI_Datatype sendtype,
		     void *recvbuf, int *recvcounts, int *displs,
		     MPI_Datatype recvtype, MPI_Comm comm ) {
  try {
    std::memcpy( (char*)recvbuf+*displs*get_sizeof(sendtype), 
		 sendbuf, sendcount*get_sizeof(sendtype));
  }
  catch (int) {
    std::cerr << "Unsupported data type " << sendtype 
	      << " used for MPI_Allgatherv" << std::endl;
    std::abort();
  }
  return 0;
}

int MPI_Alltoall( void *sendbuf, int sendcount, MPI_Datatype sendtype,
		  void *recvbuf, int recvcnt, MPI_Datatype recvtype,
		  MPI_Comm comm ) {
  return MPI_Allgather( sendbuf, sendcount, sendtype,
			recvbuf, recvcnt, recvtype, comm);
}

int MPI_Alltoallv( void *sendbuf, int *sendcounts, 
		   int *senddispls, MPI_Datatype sendtype,
		   void *recvbuf, int *recvcounts, int *recvdispls,
		   MPI_Datatype recvtype, MPI_Comm comm ) {
  return MPI_Allgather( (char*)sendbuf+*senddispls, *sendcounts, sendtype,
			(char*)recvbuf+*recvdispls, *recvcounts, recvtype, comm);
}

#endif /* DUMMY_MPI */








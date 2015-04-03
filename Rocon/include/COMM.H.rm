/// \file
/// \ingroup support
/// \brief Communication utilities
///
/// Encapsulates the entire communication substrate.  Nothing
/// outside these objects should know about MPI.
#ifndef _COMM_H_
#define _COMM_H_
#include "mpi.h"
#include <vector>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <string>

/// \namespace Comm
/// \brief Communication stuff
///
namespace Comm {
  enum DataTypes {DTDOUBLE,DTFLOAT,DTINT,DTUINT,DTSIZET,DTCHAR,DTUCHAR,DTBYTE,DTUBYTE};
  enum Ops {MAXOP, MINOP, SUMOP, PRODOP, MINLOCOP,MAXLOCOP};
  /// \brief provides communication for complex objects
  ///
  /// The MobileObject provides buffering and an interface
  /// for complex objects to use communication routines.
  class MobileObject 
  {
  protected:
    void *buf;
    bool _mine;
  public:
    MobileObject(): buf(NULL), _mine(false) {};
    void **GetBufPtr() { return(buf ? &buf : NULL); };
    const void *GetBuffer() const { return (buf); };
    void *GetBuffer() {return (buf); };
    virtual int PrepareBuffer(size_t bsize);
    virtual int Pack(void **inbuf = NULL){ return 1;};
    virtual int UnPack(const void *outbuf = NULL){ return 1;};
    virtual void DestroyBuffer() 
    { 
      if(buf && _mine) 
	delete [] (char*)buf; 
      buf = NULL;
    };
    virtual ~MobileObject() 
    { 
      DestroyBuffer();
    };
  };

  /// \brief Main encapsulation of MPI
  ///
  /// The CommunicatorObject provides a full interface to MPI.
  /// This is for convenience, consistency, and to provide an
  /// easy access point for replacing the communication substrate
  /// with something that's not MPI.
  class CommunicatorObject
  {
  private:
    int _rank;
    MPI_Comm _comm;
    bool _master;
    bool _own_comm;
    bool _initd;
    int _error;
    int _rc;
    int _nproc;
    std::vector<MPI_Request> _send_requests;
    std::vector<MPI_Request> _recv_requests;
    std::vector<int> _send_tags;
    std::vector<int> _recv_tags;
    std::vector<MPI_Status> _status;
  public:
    MPI_Comm GetCommunicator(){ return (_comm); };
    MPI_Datatype IntegerTypeID;// = MPI_INTEGER;
    CommunicatorObject();
    CommunicatorObject(MPI_Comm &incomm);
    CommunicatorObject(int* narg,char*** args);
    MPI_Datatype ResolveDataType(const Comm::DataTypes &dt);
    MPI_Op ResolveOp(const Comm::Ops &op);
    int Split(int color,int key,CommunicatorObject &newcomm);
    int WaitRecv(int recvid);
    // wait on any and all messages
    int WaitAll();
    // clear up any persistent requests
    void ClearRequests();
    int Initialize(CommunicatorObject &incomm);
    int Initialize(int* narg = NULL,char*** args = NULL);
    int SetExit(int errin){return(_error = errin);};
    int SetErr(int errin){return(_error = errin);};
    void ClearErr(){_error = 0;};
    int Check(Comm::Ops op=Comm::MAXOP);
    int Rank();
    MPI_Comm World(){return(_comm);};
    int Finalize();
    int Barrier(){return(MPI_Barrier(_comm));};
    int Size();
    ~CommunicatorObject();
    int StartSend(unsigned int rid);
    int SendAll();
    int StartRecv(unsigned int rid);
    int RecvAll();
    int BroadCast(std::string &sval,int root_rank = 0);
    int BroadCast(MobileObject *mo,int root_rank);
    int _BroadCastMOV(std::vector<MobileObject *> &mos,int root_rank);
    int _ASend(void *buf,int sendsize,unsigned int remote_rank,int tag = 0);
    int _SetSend(void *buf,int sendsize,unsigned int remote_rank,int tag = 0);
    int _ARecv(void *buf,int recvsize,unsigned int remote_rank,int tag = 0);
    int _SetRecv(void *buf,int recvsize,unsigned int remote_rank,int tag = 0);
    int _AllGatherv(void *sendbuf,int mysendcnt,int datasize,void *recvbuf);
    int _Send(void *buf,int sendsize,unsigned int remote_rank,int tag = 0);
    int _Recv(void *buf,int recvsize,unsigned int remote_rank,int tag = 0);
    template<typename T>
    int BroadCastMobileObject(T &mo,int root_rank = 0)
    {
      return(BroadCast(dynamic_cast<MobileObject *>(&mo),root_rank));
    };

    template<typename T>
    int BroadCastMOVector(std::vector<T> &mov,int root_rank = 0)
    {
      std::vector<MobileObject *> moc;
      moc.resize(mov.size());
      std::vector<MobileObject *>::iterator moci = moc.begin();
      typename std::vector<T>::iterator oi = mov.begin();
      while(moci != moc.end())
	*moci++ = dynamic_cast<MobileObject *>(&(*oi++));
      return(_BroadCastMOV(moc,root_rank));
    };

    template<typename DataType>
    int ASend(std::vector<DataType> &sendbuf,unsigned int remote_rank,
	      int tag = 0)
    {
      int sizeofdata = sizeof(DataType);
      int sendcnt = sendbuf.size();
      return (_ASend(&sendbuf[0],sizeofdata*sendcnt,remote_rank,tag));
    };

    template<typename DataType>
    int SetSend(std::vector<DataType> &sendbuf,unsigned int remote_rank,
		int tag = 0)
    {
      int sizeofdata = sizeof(DataType);
      int sendcnt = sendbuf.size();
      return(_SetSend(&sendbuf[0],sendcnt*sizeofdata,remote_rank,tag));
    };

    template<typename DataType>
    int SetRecv(std::vector<DataType> &recvbuf,unsigned int remote_rank,
		int tag = 0)
    {
      int sizeofdata = sizeof(DataType);
      int recvcnt = recvbuf.size();
      return(_SetRecv(&recvbuf[0],recvcnt*sizeofdata,remote_rank,tag));
    };

    template<typename DataType>
    int ARecv(std::vector<DataType> &recvbuf,unsigned int remote_rank,
	      int tag=0)
    {
      int sizeofdata = sizeof(DataType);
      int recvcnt = recvbuf.size();
      return(_ARecv(&recvbuf[0],sizeofdata*recvcnt,remote_rank,tag));
    };

    template<typename DataType>
    int BroadCast(DataType &buf,int root_rank)
    {
      int sizeofdata = sizeof(DataType);
      return((_rc = MPI_Bcast(&buf,sizeofdata,MPI_CHAR,root_rank,_comm)));
    };

    template<typename DataType>
    int BroadCast(std::vector<DataType> &buf,int root_rank)
    {
      int sizeofdata = sizeof(DataType);
      int bufsize = buf.size();
      _rc = MPI_Bcast(&bufsize,1,MPI_INT,root_rank,_comm);
      if(_rank != root_rank)
	buf.resize(bufsize);
      _rc = MPI_Bcast(&buf[0],buf.size()*sizeofdata,MPI_CHAR,root_rank,_comm);
      return(_rc);
    };
    
    // doesn't work, duh
    template<typename DataType>
    int Reduce(DataType &send,DataType &recv,
	       const Comm::DataTypes &dt,const Comm::Ops &op,int root)
    {
      _rc = MPI_Reduce(&send,&recv,1,ResolveDataType(dt),
		       ResolveOp(op),root,_comm);
      assert(_rc == 0);
      return(_rc);
    };

    // doesn't work, duh
    template<typename DataType>
    int Reduce(std::vector<DataType> &send,std::vector<DataType> &recv,
	       const Comm::DataTypes &dt,const Comm::Ops &op,int root)
    {
      int count = send.size();
      //    size_t datasize = sizeof(DataType);
      //    MPI_Datatype mpi_data_type = MPI_DOUBLE;
      //    if(datasize == sizeof(int))
      //      mpi_data_type = MPI_INTEGER;
      if(_rank == root)
	recv.resize(count);
      _rc = MPI_Reduce(&send[0],&recv[0],count,ResolveDataType(dt),
		       ResolveOp(op),root,_comm);
      assert(_rc == 0);
      return(_rc);
    };

    // doesn't work, duh
    template<typename DataType>
    int AllReduce(std::vector<DataType> &send,std::vector<DataType> &recv,
		  const Comm::DataTypes &dt,const Comm::Ops &op)
    {
      int count = send.size();
      recv.resize(count);
      _rc = MPI_Allreduce(&send[0],&recv[0],count,ResolveDataType(dt),
			  ResolveOp(op),_comm);
      assert(_rc == 0);
      return(_rc);
    };


    // doesn't work, duh
    template<typename DataType>
    int AllReduce(DataType &send,DataType &recv,
		  const Comm::DataTypes &dt,const Comm::Ops &op)
    {
      _rc = MPI_Allreduce(&send,&recv,1,ResolveDataType(dt),
			  ResolveOp(op),_comm);
      assert(_rc == 0);
      return(_rc);
    };

    template<typename DataType>
    int AllGather(std::vector<DataType> &sendvec,std::vector<DataType> &recvvec,
		  int sndcnt=0,int recvcnt=0)
    {
      size_t datasize = sizeof(DataType);
      if(sndcnt == 0)
	sndcnt = sendvec.size();
      if(recvcnt == 0)
	recvcnt = sndcnt;
      _rc = MPI_Allgather((void *)(&(sendvec[0])),sndcnt*datasize,MPI_CHAR,
			  (void *)(&(recvvec[0])),recvcnt*datasize,MPI_CHAR,_comm);
      assert(_rc == 0);
      return(_rc);
    };

    template<typename DataType>
    int Gather(std::vector<DataType> &sendvec,std::vector<DataType> &recvvec,
	       int sndcnt=0,int recvcnt=0,int root=0)
    {
      size_t datasize = sizeof(DataType);
      if(sndcnt == 0)
	sndcnt = sendvec.size();
      if(recvcnt == 0)
	recvcnt = sndcnt;
      if(_rank == root)
	recvvec.resize(recvcnt*_nproc);
      _rc = MPI_Gather((void *)(&(sendvec[0])),sndcnt*datasize,MPI_CHAR,
		       (void *)(&(recvvec[0])),recvcnt*datasize,MPI_CHAR,
		       root,_comm);
      assert(_rc == 0);
      return(_rc);
    };

    template<typename DataType>
    int AllGather(DataType &sendval,std::vector<DataType> &recvvec)
    {
      size_t messagesize = sizeof(DataType);
      recvvec.resize(_nproc);
      _rc = MPI_Allgather((void *)&sendval,messagesize,MPI_CHAR,
			  (void *)&recvvec[0],messagesize,MPI_CHAR,_comm);
      assert(_rc == 0);
      return(_rc);
    };

    template<typename DataType>
    int Gather(DataType &sendval,std::vector<DataType> &recvvec,int root=0)
    {
      size_t messagesize = sizeof(DataType);
      if(_rank == root)
	recvvec.resize(_nproc);
      _rc = MPI_Gather((void *)&sendval,messagesize,MPI_CHAR,
		       (void *)&recvvec[0],messagesize,MPI_CHAR,
		       root,_comm);
      assert(_rc == 0);
      return(_rc);
    };

    template<typename DataType>
    int AllGatherv(std::vector<DataType> &sendvec,std::vector<DataType> &recvvec)
    {
      int datasize = sizeof(DataType);
      int mysendcnt = sendvec.size();
      int totalcnt = 0;
      AllReduce(mysendcnt,totalcnt,DTINT,SUMOP);
      recvvec.resize(totalcnt);
      return(_AllGatherv(&sendvec[0],mysendcnt,datasize,&recvvec[0]));
    };
    
  };
  
  class ParallelObject {
  protected:
    CommunicatorObject _communicator;
  public:
    virtual ~ParallelObject(){};
    virtual CommunicatorObject &Communicator(){return(_communicator);};
  };

};
#endif

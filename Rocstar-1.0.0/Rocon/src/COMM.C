///
/// \file
/// \ingroup support
/// \brief Communication utils implementation
///
#include "COMM.H"

namespace Comm {
  int MobileObject::PrepareBuffer(size_t bsize)
  {
    if(buf && _mine)
      delete [] (char *)buf;
    buf = new char [bsize];
    if(!buf)
      return 0;
    _mine = true;
    return bsize;
  };

  CommunicatorObject::CommunicatorObject() : IntegerTypeID(MPI_INTEGER)
    
  {
    _rank = -1;
    _comm = MPI_COMM_WORLD;
    _master = false;
    _own_comm = false;
    _initd  = false;
    _rc = 0;
    _nproc = 0;
    _error = 0;
  };
  CommunicatorObject::CommunicatorObject(MPI_Comm &incomm) : IntegerTypeID(MPI_INTEGER)
  {
    _rc = MPI_Comm_rank(incomm,&_rank);
    if(_rc)
      _error = _rc;
    _comm = incomm;
    _master = false;
    _own_comm = false;
    _initd = true;
    _rc = MPI_Comm_size(_comm,&_nproc);
  };
  
  CommunicatorObject::CommunicatorObject(int* narg,char*** args) : IntegerTypeID(MPI_INTEGER)
  {
    _nproc = 0;
    _rc = Initialize(narg,args);
    assert(_rc == 0);
    _rank = -1;
    _error = 0;
    _own_comm = false;
    _master = true;
    _initd = true;
  };
  
  int CommunicatorObject::Split(int color,int key,CommunicatorObject &newcomm)
  {
    _rc = MPI_Comm_split(_comm,color,key,&(newcomm._comm));
    if(_rc)
      return(_rc);
    newcomm._master = false;
    newcomm._own_comm = true;
    newcomm._initd = true;
    _rc = MPI_Comm_rank(newcomm._comm,&(newcomm._rank));
    if(_rc)
      return(_rc);
    _rc = MPI_Comm_size(newcomm._comm,&(newcomm._nproc));
    return(_rc);
  };
  
  int CommunicatorObject::WaitRecv(int recvid)
  {
    _status.resize(1);
    _rc = MPI_Wait(&_send_requests[recvid],&_status[0]);
    assert(_rc == 0);
    return(_rc);
  };
  
  // clear up any persistent requests
  int CommunicatorObject::WaitAll()
  {
    std::vector<MPI_Request> requests;
    std::vector<MPI_Request>::iterator ri = _send_requests.begin();
    while(ri != _send_requests.end())
      requests.push_back(*ri++);
    ri = _recv_requests.begin();
    while(ri != _recv_requests.end())
      requests.push_back(*ri++);
    int count = requests.size();
    _status.resize(count);
    _rc = MPI_Waitall(count,&requests[0],&_status[0]);
    assert(_rc == 0);
    ri = requests.begin();
    while(ri != requests.end())
      assert(*ri++ == MPI_REQUEST_NULL);
    ClearRequests();
    return(_rc);
  };

  // clear up any persistent requests
  void CommunicatorObject::ClearRequests()
  {
    std::vector<MPI_Request>::iterator ri = _send_requests.begin();
    //      while(ri != _send_requests.end())
    //	MPI_Request_free(&(*ri++));
    //      ri = _recv_requests.begin();
    //      while(ri != _recv_requests.end())
    //	MPI_Request_free(&(*ri++));
    _send_requests.resize(0);
    _recv_requests.resize(0);
    _send_tags.resize(0);
    _recv_tags.resize(0);
  };
  
  //    int StartSend(unsigned int rid);
  //    int SendAll();
  //    int StartRecv(unsigned int rid);
  //    int RecvAll();
  
  int CommunicatorObject::Initialize(CommunicatorObject &incomm)
  {
    _comm = incomm._comm;
    _master = false;
    _own_comm = false;
    _initd = true;
    int flag = 0;
    MPI_Initialized(&flag);
    assert(flag != 0);
    if(flag == 0)
      _initd = false;
    else{
      _rc = MPI_Comm_rank(_comm,&_rank);
      if(_rc){
	return(_rc);
      }
      _rc = MPI_Comm_size(_comm,&_nproc);
      if(_rc)
	return(_rc);
    }
    return(0);
  };
  
  int CommunicatorObject::Initialize(int* narg,char*** args)
  {
    int flag = 0;
    MPI_Initialized(&flag);
    _comm = MPI_COMM_WORLD;
    if(flag == 0){
      _master = true;
      _own_comm = false;
      return(MPI_Init(narg,args));
    }
    _master = false;
    _initd = true;
    return(0);
  };
  
  int CommunicatorObject::Check(Comm::Ops op)
  {
    int errcheck = 0;
    // i guess for now we'll reduce max and make sure it's zero
    _rc = MPI_Allreduce(&_error,&errcheck,1,MPI_INTEGER,ResolveOp(op),_comm);
    return(errcheck);
  };
  
  int CommunicatorObject::Rank()
  {
    if(_rank < 0)
      MPI_Comm_rank(_comm,&_rank);
    return(_rank);
  };
  
  int CommunicatorObject::Finalize()
  {
    if(_master){
      int flag = 0;
      MPI_Finalized(&flag);
      if(!flag){
	flag = 0;
	MPI_Initialized(&flag);
	if(flag){
	  MPI_Finalize();
	}
      }
    }
    _initd = false;
    return(0);
  };
  int CommunicatorObject::Size()
  {
    if(_nproc <= 0)
      _rc = MPI_Comm_size(_comm,&_nproc);
    return(_nproc);
  };
  
  CommunicatorObject::~CommunicatorObject()
  {
    int flag;
    MPI_Finalized(&flag);
    if(flag == 0 && _own_comm && _initd){
      MPI_Comm_free(&_comm);
    }
    if(flag == 0 && _master && _initd){
      MPI_Finalize();
    }
  };
  
  int CommunicatorObject::StartSend(unsigned int rid)
  {
    if(_send_requests.empty())
      return(0);
    if(rid >= _send_requests.size()){
      _error = 4;
      return(1);
    }
    _rc = MPI_Start(&_send_requests[rid]);
    assert(_rc == 0);
    return(_rc);
  };

  int CommunicatorObject::SendAll()
  {
    if(_send_requests.empty())
      return(0);
    _rc = MPI_Startall(_send_requests.size(),&_send_requests[0]);
    assert(_rc == 0);
    return(_rc);
  };
  
  int CommunicatorObject::StartRecv(unsigned int rid)
  {
    if(_recv_requests.empty())
      return(0);
    if(rid >= _recv_requests.size())
      return(1);
    _rc = MPI_Start(&_recv_requests[rid]);
    assert(_rc == 0);
    return(_rc);
  };
  
  int CommunicatorObject::RecvAll()
  {
    if(_recv_requests.empty())
      return(0);
    _rc = MPI_Startall(_recv_requests.size(),&_recv_requests[0]);
    assert(_rc == 0);
    return(_rc);
  };
  
  int CommunicatorObject::BroadCast(std::string &sval,int root_rank)
  {
    int sizeofobject = 0;
    if(_rank == root_rank)
      sizeofobject = sval.size();
    _rc = MPI_Bcast(&sizeofobject,1,MPI_INT,root_rank,_comm);
    if(sizeofobject <= 0)
      return 1;
    if(!_rc){
      char *sbuf = new char [sizeofobject];
      if(_rank == root_rank)
	std::strncpy(sbuf,sval.c_str(),sizeofobject);
      _rc = MPI_Bcast(sbuf,sizeofobject,MPI_CHAR,root_rank,_comm);
      if(_rank != root_rank)
	sval.assign(std::string(sbuf));
      delete [] sbuf;
    }
    return(_rc);
  };


  int CommunicatorObject::BroadCast(MobileObject *mo,int root_rank)
  {
    int sizeofobject = 0;
    if(_rank == root_rank)
      sizeofobject = mo->Pack();
    _rc = MPI_Bcast(&sizeofobject,1,MPI_INT,root_rank,_comm);
    if(sizeofobject <= 0)
      return 1;
    if(!_rc){
      if(_rank != root_rank)
	mo->PrepareBuffer(sizeofobject);
      _rc = MPI_Bcast(mo->GetBuffer(),sizeofobject,MPI_CHAR,root_rank,_comm);
      if(_rank != root_rank)
	_rc = mo->UnPack();
    }
    return(_rc);
  };
  
  // Note, the mos have to be of the right size on every processor - otherwise we
  // have no way to generically size an array of unspecified (actual) type.
  int CommunicatorObject::_BroadCastMOV(std::vector<MobileObject *> &mos,int root_rank)
  {
    int nobjs = mos.size();
    _rc = 0;
    // This broadcast is superfluous since nobjs should already be 
    // identical on every processor.
    if((_rc = MPI_Bcast(&nobjs,1,MPI_INT,root_rank,_comm)))
      return(1);
    std::vector<int> sizeofobject(nobjs,0);
    int total_size = 0;
    if(_rank == root_rank){
      std::vector<int>::iterator si = sizeofobject.begin();
      std::vector<MobileObject *>::iterator moi = mos.begin();
      while(moi != mos.end()){
	sizeofobject[si - sizeofobject.begin()] = (*moi)->Pack();
	moi++;
	si++;
      }
    }
    if((_rc = MPI_Bcast(&sizeofobject[0],nobjs,MPI_INT,root_rank,_comm)))
      return(1);
    std::vector<int>::iterator si = sizeofobject.begin();
    std::vector<MobileObject *>::iterator moi = mos.begin();
    while(si != sizeofobject.end()){
      if(_rank != root_rank){
	int bsize = (*moi++)->PrepareBuffer(*si);
	assert(bsize == *si);
      }
      total_size += *si++;
    }
    assert(total_size > 0);
    if(total_size <= 0)
      return(1);
    char *bufferspace = new char [total_size];
    if(_rank == root_rank){
      // pack the bufferspace with all the buffers
      char *cur_pos = bufferspace;
      si  = sizeofobject.begin();
      moi = mos.begin();
      while(moi != mos.end()){
	std::memcpy(cur_pos,(*moi)->GetBuffer(),*si);
	cur_pos += *si++;
	moi++;
      }
    }
    if((_rc = MPI_Bcast(bufferspace,total_size,MPI_CHAR,root_rank,_comm))){
      delete [] bufferspace;
      return(1);
    }
 
    // Now everyone has all the data
    if(_rank == root_rank){
      // root can just destroy his buffers
      moi = mos.begin();
      while(moi != mos.end())
	(*moi++)->DestroyBuffer();
    }
    else{   // everyone else needs to unpack
      char *cur_pos = bufferspace;
      si  = sizeofobject.begin();
      moi = mos.begin();
      while(moi != mos.end()){
	assert((*moi)->GetBuffer() != NULL);
	std::memcpy((*moi)->GetBuffer(),cur_pos,*si);
	_rc += (*moi)->UnPack();
	assert(_rc == 0);
	cur_pos += *si++;
	moi++;
      }
    }
    delete [] bufferspace;
    return(_rc);
  };
  
  int CommunicatorObject::_ASend(void *buf,int sendsize,
				 unsigned int remote_rank,int tag)
  {
    MPI_Request request;
    int request_id = _send_requests.size();
    _send_requests.push_back(request);
    if(tag == 0)
      tag = _send_tags.size() + 1;
    _send_tags.push_back(tag);
    _rc = MPI_Isend(buf,sendsize,MPI_CHAR,remote_rank,
		    tag,_comm,&_send_requests[request_id]);
    assert(_rc == 0);
    return(request_id);
  };

  int CommunicatorObject::_Send(void *buf,int sendsize,
				unsigned int remote_rank,int tag)
  {
    _rc = MPI_Send(buf,sendsize,MPI_CHAR,remote_rank,
		   tag,_comm);
    assert(_rc == 0);
    return(0);
  };
  
  int CommunicatorObject::_SetRecv(void *recvbuf,int recvsize, 
				   unsigned int remote_rank,int tag)
  {
    MPI_Request request;
    int request_id = _recv_requests.size();
    _recv_requests.push_back(request);
    if(tag == 0)
      tag = MPI_ANY_TAG;
    _recv_tags.push_back(tag);
    _rc = MPI_Recv_init(recvbuf,recvsize,MPI_CHAR,remote_rank,
			tag,_comm,&_recv_requests[request_id]);
    assert(_rc == 0);
    return(request_id);
  };


    int CommunicatorObject::_ARecv(void *recvbuf,int recvsize,
				   unsigned int remote_rank,int tag)
    {
      MPI_Request request;
      int request_id = _recv_requests.size();
      _recv_requests.push_back(request);
      if(tag == 0)
	tag = MPI_ANY_TAG;
      _recv_tags.push_back(tag);
      _rc = MPI_Irecv(recvbuf,recvsize,MPI_CHAR,remote_rank,
		      tag,_comm,&_recv_requests[request_id]);
      assert(_rc == 0);
      return(request_id);
    };

    int CommunicatorObject::_Recv(void *recvbuf,int recvsize,
				  unsigned int remote_rank,int tag)
    {
      MPI_Status status;
      if(tag == 0)
	tag = MPI_ANY_TAG;
      _rc = MPI_Recv(recvbuf,recvsize,MPI_CHAR,remote_rank,
		     tag,_comm,&status);
      assert(_rc == 0);
      return(0);
    };

  int CommunicatorObject::_SetSend(void *sendbuf,int sendsize,
				   unsigned int remote_rank,int tag)
  {
    MPI_Request request;
    int request_id = _send_requests.size();
    _send_requests.push_back(request);
    if(tag == 0)
      tag = _send_tags.size() + 1;
    _send_tags.push_back(tag);
    _rc = MPI_Send_init(sendbuf,sendsize,MPI_CHAR,remote_rank,
			tag,_comm,&_send_requests[request_id]);
    assert(_rc == 0);
    return(request_id);
  };

  int CommunicatorObject::_AllGatherv(void *sendbuf,int mysendcnt,int datasize,void *recvbuf)
  {
    std::vector<int> allsizes(_nproc,0);
    AllGather(mysendcnt,allsizes);
    std::vector<int>::iterator asi = allsizes.begin();
    while(asi != allsizes.end()){
      *asi = *asi*datasize;
      asi++;
    }
    std::vector<int> displacements(_nproc,0);
    for(int i = 1;i < _nproc;i++)
      displacements[i] = displacements[i-1]+allsizes[i-1];
    _rc = MPI_Allgatherv(sendbuf,allsizes[_rank],MPI_CHAR,
			 recvbuf,&allsizes[0],&displacements[0],
			 MPI_CHAR,_comm);
    assert(_rc == 0);
    return(_rc);
  };

  MPI_Datatype CommunicatorObject::ResolveDataType(const Comm::DataTypes &dt)
  {
    switch(dt){
    case Comm::DTDOUBLE:
      return(MPI_DOUBLE);
    case Comm::DTFLOAT:
      return(MPI_FLOAT);
    case Comm::DTINT:
      return(MPI_INTEGER);
    case Comm::DTUBYTE:
    case Comm::DTUCHAR:
      return(MPI_UNSIGNED_CHAR);
    case Comm::DTCHAR:
    case Comm::DTBYTE:
      return(MPI_CHAR);
    case Comm::DTSIZET:
      return(MPI_LONG_LONG_INT);
    case Comm::DTUINT:
      return(MPI_UNSIGNED);
    default:
      return(static_cast<MPI_Datatype>(MPI_DATATYPE_NULL));
    }
    // should never get here
    return(static_cast<MPI_Datatype>(MPI_DATATYPE_NULL));
  };

  MPI_Op CommunicatorObject::ResolveOp(const Comm::Ops &op)
  {
    switch(op){
    case Comm::MINOP:
      return(MPI_MIN);
    case Comm::MAXOP:
      return(MPI_MAX);
    case Comm::SUMOP:
      return(MPI_SUM);
    case Comm::PRODOP:
      return(MPI_PROD);
    case Comm::MAXLOCOP:
      return(MPI_MAXLOC);
    case Comm::MINLOCOP:
      return(MPI_MINLOC);
    default:
      return(static_cast<MPI_Op>(MPI_OP_NULL));
    }
    return(static_cast<MPI_Op>(MPI_OP_NULL));
  };
}

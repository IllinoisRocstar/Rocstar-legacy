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
/**
 ** @file HDF4.cpp
 ** @brief Definition of the HDF4 class.
 ** @author Johnny C. Norris II
 **/

#include <algorithm>
#include <functional>
#include <iostream>
#include <limits>
#include <vector>
#include <mfhdf.h>
#include "HDF4.h"

const int HDF_DEBUG = 0;

#ifdef USE_PTHREADS

/**
 ** A struct used to pass HDF4 commands to the I/O thread. 
 **
 ** This struct encapsulates all of the information on one HDF4
 ** command.  It also facilitates blocking.
 **/
struct HDF4Cmd {
  HDF4Cmd(int cmd) : m_command(cmd) { m_result.asInt32 = 0; }

  /** The supported HDF4 commands.
   **
   ** Only the HDF4 commands that I actually use are supported.
   **/
  enum Label {
    Hishdf,
    SDstart,
    SDend,
    SDfileinfo,
    SDcreate,
    SDselect,
    SDendaccess,
    SDfindattr,
    SDgetinfo,
    SDsetdatastrs,
    SDgetdatastrs,
    SDsetrange,
    SDgetrange,
    SDwritedata,
    SDreaddata,
    DFSDsetdims,
    DFSDsetNT,
    DFSDsetdatastrs,
    DFSDsetrange,
    DFSDadddata,
    DFSDputdata,
    Terminate
  };
  int m_command;        ///< The HDF4 command to call.

  /// The supported HDF4 datatypes.
  union ArgUnion {
    int32 asInt32;
    intn asIntn;
    char* asCharP;
    int32* asInt32P;
    VOIDP asVOIDP;
  };
  std::vector<ArgUnion> m_args; ///< The argument list for the command.
  union {
    int32 asInt32;
    intn asIntn;
  } m_result;            ///< The returned result of the HDF4 command.
  Semaphore m_semaphore; ///< A semaphore to implement caller blocking.
};

Semaphore HDF4::sm_pending;
std::list<HDF4Cmd*> HDF4::sm_cmdQueue;
Mutex HDF4::sm_cs;
pthread_t HDF4::sm_id = 0;
int HDF4::sm_counter = 0;

#endif // USE_PTHREADS

/**
 ** Make sure that an I/O thread isn't running already.  If it is, then don't
 ** create another one.
 **/
void HDF4::init()
{
  if (HDF_DEBUG)
   std::cout << "HDF4::init()" << std::endl;

#ifdef USE_PTHREADS
  ++sm_counter;

  // Make sure that a thread isn't already running.
  if (sm_id != 0)
    return;

  pthread_attr_t attr;
  pthread_attr_init(&attr);
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
  pthread_create(&sm_id, &attr, HDF4::Entry, NULL);
  pthread_attr_destroy(&attr);
#endif // USE_PTHREADS
}

void HDF4::finalize()
{
  if (HDF_DEBUG)
    std::cout << "HDF4::finalize()" << std::endl;

#ifdef USE_PTHREADS
  --sm_counter;
  if (sm_counter > 0)
    return;

  HDF4::Terminate();

  void* retval;
  pthread_join(sm_id, &retval);

  sm_id = 0;
#endif // USE_PTHREADS
}

//@{
/**
 ** Each wrapper function adds a command request to the end of the command
 ** queue and signals the I/O thread that a command is pending.  See the HDF4
 ** documentation for details on parameters and return values for each
 ** function.
 **/
intn HDF4::Hishdf(const char* filename)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::Hishdf" << std::endl;

#ifdef USE_PTHREADS
  // Sprecify the command.
  HDF4Cmd c(HDF4Cmd::Hishdf);

  // Add the arguments to the argument list.
  c.m_args.resize(1);
  c.m_args[0].asCharP = const_cast<char*>(filename);

  // Push the command request onto the command queue.
  PostCommand(&c);

  // Return the result.
  return c.m_result.asIntn;
#else
 return ::Hishdf(filename);
#endif // USE_PTHREADS
}

int32 HDF4::SDstart(const char* filename, int32 accessMode)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDstart( filename == " << filename << ", accessMode == " << accessMode << " )" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDstart);

  c.m_args.resize(2);
  c.m_args[0].asCharP = const_cast<char*>(filename);
  c.m_args[1].asInt32 = accessMode;

  if (HDF_DEBUG){
    // std::cout << "  Posting Command" << std::endl << "    m_args[0] = " << c.m_args[0].asCharP << std::endl << "    m_args[1] = " << c.m_args[1].asInt32 << std::endl;
  }
  PostCommand(&c);

  return c.m_result.asInt32;
#else
 return ::SDstart(filename, accessMode);
#endif // USE_PTHREADS
}

intn HDF4::SDend(int32 sd_id)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDend" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDend);

  c.m_args.resize(1);
  c.m_args[0].asInt32 = sd_id;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDend(sd_id);
#endif // USE_PTHREADS
}

intn HDF4::SDfileinfo(int32 id, int32* dsCount, int32* nAttrs)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDfileinfo" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDfileinfo);

  c.m_args.resize(3);
  c.m_args[0].asInt32 = id;
  c.m_args[1].asInt32P = dsCount;
  c.m_args[2].asInt32P = nAttrs;

  if(HDF_DEBUG)
    std::cout << "  Posting command" << std::endl;
  PostCommand(&c);

  if(HDF_DEBUG)
    std::cout << "  Returning" << std::endl;
  return c.m_result.asIntn;
#else
  return ::SDfileinfo(id, dsCount, nAttrs);
#endif // USE_PTHREADS
}

int32 HDF4::SDcreate(int32 sds_id, const char* name, int32 dType, int32 rank,
                     int32* size)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDcreate" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDcreate);

  c.m_args.resize(5);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asCharP = const_cast<char*>(name);
  c.m_args[2].asInt32 = dType;
  c.m_args[3].asInt32 = rank;
  c.m_args[4].asInt32P = size;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDcreate(sds_id, name, dType, rank, size);
#endif // USE_PTHREADS
}

int32 HDF4::SDselect(int32 sd_id, int32 index)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDselect( sd_id == " << sd_id << ", index == " << index << " )" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDselect);

  c.m_args.resize(2);
  c.m_args[0].asInt32 = sd_id;
  c.m_args[1].asInt32 = index;

  PostCommand(&c);

  return c.m_result.asInt32;
#else
  return ::SDselect(sd_id, index);
#endif // USE_PTHREADS
}

intn HDF4::SDendaccess(int32 sds_id)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDendaccess" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDendaccess);

  c.m_args.resize(1);
  c.m_args[0].asInt32 = sds_id;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDendaccess(sds_id);
#endif // USE_PTHREADS
}

int32 HDF4::SDfindattr(int32 id, const char* attrName)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDfindattr" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDfindattr);

  c.m_args.resize(2);
  c.m_args[0].asInt32 = id;
  c.m_args[1].asCharP = const_cast<char*>(attrName);

  PostCommand(&c);

  return c.m_result.asInt32;
#else
  return ::SDfindattr(id, attrName);
#endif // USE_PTHREADS
}

intn HDF4::SDgetinfo(int32 sds_id, char* name, int32* rank, int32* size,
                     int32* dType, int32* nAttrs)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDgetinfo" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDgetinfo);

  c.m_args.resize(6);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asCharP = name;
  c.m_args[2].asInt32P = rank;
  c.m_args[3].asInt32P = size;
  c.m_args[4].asInt32P = dType;
  c.m_args[5].asInt32P = nAttrs;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDgetinfo(sds_id, name, rank, size, dType, nAttrs);
#endif // USE_PTHREADS
}

intn HDF4::SDsetdatastrs(int32 sds_id, const char* label, const char* units,
                         const char* format, const char* coordsys)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDsetdatastrs" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDsetdatastrs);

  c.m_args.resize(5);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asCharP = const_cast<char*>(label);
  c.m_args[2].asCharP = const_cast<char*>(units);
  c.m_args[3].asCharP = const_cast<char*>(format);
  c.m_args[4].asCharP = const_cast<char*>(coordsys);

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDsetdatastrs(sds_id, label, units, format, coordsys);
#endif // USE_PTHREADS
}

intn HDF4::SDgetdatastrs(int32 sds_id, char* label, char* units, char* format,
                     char* coordsys, intn length)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDgetdatastrs" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDgetdatastrs);

  c.m_args.resize(6);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asCharP = label;
  c.m_args[2].asCharP = units;
  c.m_args[3].asCharP = format;
  c.m_args[4].asCharP = coordsys;
  c.m_args[5].asIntn = length;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDgetdatastrs(sds_id, label, units, format, coordsys, length);
#endif // USE_PTHREADS
}

intn HDF4::SDsetrange(int32 sds_id, VOIDP max, VOIDP min)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDsetrange" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDsetrange);

  c.m_args.resize(3);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asVOIDP = max;
  c.m_args[2].asVOIDP = min;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDsetrange(sds_id, max, min);
#endif // USE_PTHREADS
}

intn HDF4::SDgetrange(int32 sds_id, VOIDP max, VOIDP min)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDgetrange" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDgetrange);

  c.m_args.resize(3);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asVOIDP = max;
  c.m_args[2].asVOIDP = min;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDgetrange(sds_id, max, min);
#endif // USE_PTHREADS
}

intn HDF4::SDwritedata(int32 sds_id, int32* start, int32* stripe, int32* end,
                       VOIDP data)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDwritedata" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDwritedata);

  c.m_args.resize(5);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asInt32P = start;
  c.m_args[2].asInt32P = stripe;
  c.m_args[3].asInt32P = end;
  c.m_args[4].asVOIDP = data;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDwritedata(sds_id, start, stripe, end, data);
#endif // USE_PTHREADS
}

intn HDF4::SDreaddata(int32 sds_id, int32* start, int32* stripe, int32* end,
                      VOIDP data)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SDreaddata" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::SDreaddata);

  c.m_args.resize(5);
  c.m_args[0].asInt32 = sds_id;
  c.m_args[1].asInt32P = start;
  c.m_args[2].asInt32P = stripe;
  c.m_args[3].asInt32P = end;
  c.m_args[4].asVOIDP = data;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::SDreaddata(sds_id, start, stripe, end, data);
#endif // USE_PTHREADS
}

intn HDF4::DFSDsetdims(intn rank, int32 dimsizes[])
{
  if (HDF_DEBUG)
    std::cout << "HDF4::DFSDsetdims" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::DFSDsetdims);

  c.m_args.resize(2);
  c.m_args[0].asIntn = rank;
  c.m_args[1].asInt32P = dimsizes;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::DFSDsetdims(rank, dimsizes);
#endif // USE_PTHREADS
}

intn HDF4::DFSDsetNT(int32 numbertype)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::DFSDsetNT" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::DFSDsetNT);

  c.m_args.resize(1);
  c.m_args[0].asInt32 = numbertype;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::DFSDsetNT(numbertype);
#endif // USE_PTHREADS
}

intn HDF4::DFSDsetdatastrs(const char* label, const char* unit,
                           const char* format, const char* coordsys)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::DFSDsetdatastrs" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::DFSDsetdatastrs);

  c.m_args.resize(4);
  c.m_args[0].asCharP = const_cast<char*>(label);
  c.m_args[1].asCharP = const_cast<char*>(unit);
  c.m_args[2].asCharP = const_cast<char*>(format);
  c.m_args[3].asCharP = const_cast<char*>(coordsys);

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::DFSDsetdatastrs(label, unit, format, coordsys);
#endif // USE_PTHREADS
}

intn HDF4::DFSDsetrange(VOIDP maxi, VOIDP mini)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::DFSDsetrange" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::DFSDsetrange);

  c.m_args.resize(2);
  c.m_args[0].asVOIDP = maxi;
  c.m_args[1].asVOIDP = mini;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::DFSDsetrange(maxi, mini);
#endif // USE_PTHREADS
}

intn HDF4::DFSDadddata(const char* filename, intn rank, int32 dimsizes[],
                       VOIDP data)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::DFSDadddata" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::DFSDadddata);

  c.m_args.resize(4);
  c.m_args[0].asCharP = const_cast<char*>(filename);
  c.m_args[1].asIntn = rank;
  c.m_args[2].asInt32P = dimsizes;
  c.m_args[3].asVOIDP = data;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::DFSDadddata(filename, rank, dimsizes, data);
#endif // USE_PTHREADS
}

intn HDF4::DFSDputdata(const char* filename, intn rank, int32 dimsizes[], 
                       VOIDP data)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::DFSDputdata" << std::endl;

#ifdef USE_PTHREADS
  HDF4Cmd c(HDF4Cmd::DFSDputdata);

  c.m_args.resize(4);
  c.m_args[0].asCharP = const_cast<char*>(filename);
  c.m_args[1].asIntn = rank;
  c.m_args[2].asInt32P = dimsizes;
  c.m_args[3].asVOIDP = data;

  PostCommand(&c);

  return c.m_result.asIntn;
#else
  return ::DFSDputdata(filename, rank, dimsizes, data);
#endif // USE_PTHREADS
}
//@}

/**
 ** This adds a "Terminate" request to the command queue.  Any commands
 ** already in the queue will be acted on, but any commands added afterward
 ** will be ignored.
 **/
void HDF4::Terminate()
{
  if (HDF_DEBUG)
    std::cout << "HDF4::Terminate()" << std::endl;

#ifdef USE_PTHREADS
  if (sm_counter > 0)
    return;

  HDF4Cmd c(HDF4Cmd::Terminate);

  c.m_args.clear();

  PostCommand(&c);
#endif // USE_PTHREADS
}

#ifdef USE_PTHREADS
void HDF4::PostCommand(HDF4Cmd* pCmd)
{
  sm_cs.Lock();
  sm_cmdQueue.push_back(pCmd);
  sm_cs.Unlock();
 
  //std::cout << "  Signal I/O thread that it has work" << std::endl;
  // Signal the I/O thread that it has something to do.
  sm_pending.Post();

  //std::cout << "  Wait for completion of I/O thread" << std::endl;  
  // The I/O thread will signal completion by posting to the semaphore.
  pCmd->m_semaphore.Wait();
}
#endif // USE_PTHREADS

/**
 ** The "single file" SD routines add several "fake" datasets, which
 ** severely clutter up the file and complicate searches.  This more powerful
 ** version of SDselect adds the functionality of SDgetinfo and ignores
 ** fake datasets.
 **
 ** @param index The index of the dataset to select.  The difference from
 **              SDselect is that this parameter will be assigned the index
 **              of the first non-"fake" dataset (possibly the initial value).
 ** @param dsCount The number of datasets in the file.  If this isn't
 **                provided, the number of datasets is determined by calling
 **                SDfileinfo.
 ** @return An HDF4 dataset id on success, or FAIL on failure.
 ** @note See the documentation for SDselect and SDgetinfo for the
 **       descriptions of the other parameters.
 **/
int32 HDF4::Select(int32 sd_id, int32& index, char* name, int32* rank,
                   int32* size, int32* dType, int32* nAttrs, int32 dsCount)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::Select" << std::endl;
  int32 sds_id;
  intn status;

  // If the caller didn't specify the number of datasets in the file, then
  // we have to call SDfileinfo().  <sigh>
  if (dsCount == -1) {
    int32 numAttrs;
    status = HDF4::SDfileinfo(sd_id, &dsCount, &numAttrs);
    if (status == FAIL)
      dsCount = MAX_NC_VARS;
  }

  while (index < dsCount) {
    // Open the next dataset.
    sds_id = HDF4::SDselect(sd_id, index);
    if (sds_id == FAIL) {
      std::cerr << "HDF4::Select(): unable to select dataset " << index
                << '.' << std::endl;
      return FAIL;
    }

    // Get the dataset info.
    status = HDF4::SDgetinfo(sds_id, name, rank, size, dType, nAttrs);
    if (status == FAIL) {
      std::cerr << "HDF4::Select(): SDgetinfo() failed for dataset "
                << index << '.' << std::endl;
      return FAIL;
    }

    // Make sure that this isn't a "fakeDim" dataset (they're so annoying).
    if (strncmp(name, "fakeDim", 7) != 0)
      return sds_id;

    // If we got this far, then it was a "fakeDim" dataset.  Close it and
    // try the next one.
    HDF4::SDendaccess(sds_id);
    ++index;
  }

  // Whoops, we're fresh out of datasets.  Sorry.
  return FAIL;
}


/**
 ** Like the "sizeof" operator, except that this takes a HDF4 datatype id.
 **
 ** @param dType The HDF4 datatype id.
 ** @return The size, in bytes, of the datatype.
 **/
int HDF4::SizeOf(int32 dType)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::SIZEOF" << std::endl;
  switch (dType) {
    case DFNT_CHAR8:
    case DFNT_UCHAR8:
    case DFNT_INT8:
    case DFNT_NINT8:
    case DFNT_LINT8:
    case DFNT_UINT8:
    case DFNT_NUINT8:
    case DFNT_LUINT8:
      return 1;

    case DFNT_INT16:
    case DFNT_NINT16:
    case DFNT_LINT16:
    case DFNT_UINT16:
    case DFNT_NUINT16:
    case DFNT_LUINT16:
      return 2;

    case DFNT_INT32:
    case DFNT_NINT32:
    case DFNT_LINT32:
    case DFNT_UINT32:
    case DFNT_NUINT32:
    case DFNT_LUINT32:
    case DFNT_FLOAT32:
    case DFNT_NFLOAT32:
    case DFNT_LFLOAT32:
      return 4;

    case DFNT_FLOAT64:
    case DFNT_NFLOAT64:
    case DFNT_LFLOAT64:
      return 8;
  }

  return 0;
}

#if USE_PTHREADS

/**
 ** This is the function that's executed by the thread.  Wait for a signal
 ** that there's a command pending, then pop the command off of the command
 ** queue and act on it.  Then signal the calling thread that its command
 ** has been processed.  Repeat until a Terminate command is received.
 **
 ** @return The thread exit code.
 **/
void* HDF4::Entry(void*)
{
  if (HDF_DEBUG)
    std::cout << "HDF4::ENTRY" << std::endl;
  if (HDF_DEBUG)
    std::cout << "HDF4 I/O thread started!" << std::endl;

  HDF4Cmd* c = NULL;
  do {
    // Wait for the signal that there's something to do.
    if (!sm_pending.Wait()) {
      std::cerr << "HDF4::Entry(): semaphore error." << std::endl;
      return NULL;
    }

    if (sm_cmdQueue.empty())
      continue;

    // Get the command request.
    sm_cs.Lock();
    c = sm_cmdQueue.front();
    sm_cmdQueue.pop_front();
    sm_cs.Unlock();

    // Act on the command.
    switch (c->m_command) {
      case HDF4Cmd::Hishdf:
        c->m_result.asIntn = ::Hishdf(c->m_args[0].asCharP);
        break;

      case HDF4Cmd::SDstart:
        c->m_result.asInt32 = ::SDstart(c->m_args[0].asCharP,
                                        c->m_args[1].asInt32);
        break;

      case HDF4Cmd::SDend:
        c->m_result.asIntn = ::SDend(c->m_args[0].asInt32);
        break;

      case HDF4Cmd::SDfileinfo:
        c->m_result.asIntn = ::SDfileinfo(c->m_args[0].asInt32,
                                          c->m_args[1].asInt32P,
                                          c->m_args[2].asInt32P);
        break;

      case HDF4Cmd::SDcreate:
        c->m_result.asIntn = ::SDcreate(c->m_args[0].asInt32,
                                        c->m_args[1].asCharP,
                                        c->m_args[2].asInt32,
                                        c->m_args[3].asInt32,
                                        c->m_args[4].asInt32P);
        break;

      case HDF4Cmd::SDselect:
        c->m_result.asInt32 = ::SDselect(c->m_args[0].asInt32,
                                         c->m_args[1].asInt32);
        break;

      case HDF4Cmd::SDendaccess:
        c->m_result.asIntn = ::SDendaccess(c->m_args[0].asInt32);
        break;

      case HDF4Cmd::SDfindattr:
        c->m_result.asInt32 = ::SDfindattr(c->m_args[0].asInt32,
                                           c->m_args[1].asCharP);
        break;

      case HDF4Cmd::SDgetinfo:
        c->m_result.asIntn = ::SDgetinfo(c->m_args[0].asInt32,
                                         c->m_args[1].asCharP,
                                         c->m_args[2].asInt32P,
                                         c->m_args[3].asInt32P,
                                         c->m_args[4].asInt32P,
                                         c->m_args[5].asInt32P);
        break;

      case HDF4Cmd::SDsetdatastrs:
        c->m_result.asIntn = ::SDsetdatastrs(c->m_args[0].asInt32,
                                             c->m_args[1].asCharP,
                                             c->m_args[2].asCharP,
                                             c->m_args[3].asCharP,
                                             c->m_args[4].asCharP);
        break;

      case HDF4Cmd::SDgetdatastrs:
        c->m_result.asIntn = ::SDgetdatastrs(c->m_args[0].asInt32,
                                             c->m_args[1].asCharP,
                                             c->m_args[2].asCharP,
                                             c->m_args[3].asCharP,
                                             c->m_args[4].asCharP,
                                             c->m_args[5].asIntn);
        break;

      case HDF4Cmd::SDsetrange:
        c->m_result.asIntn = ::SDsetrange(c->m_args[0].asInt32,
                                          c->m_args[1].asVOIDP,
                                          c->m_args[2].asVOIDP);
        break;

      case HDF4Cmd::SDgetrange:
        c->m_result.asIntn = ::SDgetrange(c->m_args[0].asInt32,
                                          c->m_args[1].asVOIDP,
                                          c->m_args[2].asVOIDP);
        break;

      case HDF4Cmd::SDwritedata:
        c->m_result.asIntn = ::SDwritedata(c->m_args[0].asInt32,
                                           c->m_args[1].asInt32P,
                                           c->m_args[2].asInt32P,
                                           c->m_args[3].asInt32P,
                                           c->m_args[4].asVOIDP);
        break;

      case HDF4Cmd::SDreaddata:
        c->m_result.asIntn = ::SDreaddata(c->m_args[0].asInt32,
                                          c->m_args[1].asInt32P,
                                          c->m_args[2].asInt32P,
                                          c->m_args[3].asInt32P,
                                          c->m_args[4].asVOIDP);
        break;

      case HDF4Cmd::DFSDsetdims:
        c->m_result.asIntn = ::DFSDsetdims(c->m_args[0].asIntn,
                                           c->m_args[1].asInt32P);
        break;

      case HDF4Cmd::DFSDsetNT:
        c->m_result.asIntn = ::DFSDsetNT(c->m_args[0].asInt32);
        break;

      case HDF4Cmd::DFSDsetdatastrs:
        c->m_result.asIntn = ::DFSDsetdatastrs(c->m_args[0].asCharP,
                                               c->m_args[1].asCharP,
                                               c->m_args[2].asCharP,
                                               c->m_args[3].asCharP);
        break;

      case HDF4Cmd::DFSDsetrange:
        c->m_result.asIntn = ::DFSDsetrange(c->m_args[0].asVOIDP,
                                            c->m_args[1].asVOIDP);
        break;

      case HDF4Cmd::DFSDadddata:
        c->m_result.asIntn = ::DFSDadddata(c->m_args[0].asCharP,
                                           c->m_args[1].asIntn,
                                           c->m_args[2].asInt32P,
                                           c->m_args[3].asVOIDP);
        break;

      case HDF4Cmd::DFSDputdata:
        c->m_result.asIntn = ::DFSDputdata(c->m_args[0].asCharP,
                                           c->m_args[1].asIntn,
                                           c->m_args[2].asInt32P,
                                           c->m_args[3].asVOIDP);
        break;

      case HDF4Cmd::Terminate:
        break;

      default:
        std::cerr << "HDF4::Entry: ignoring unrecognised command ("
                  << c->m_command << ')' << std::endl;
        break;
    }

    // Signal the caller that processing is complete.
    c->m_semaphore.Post();

    // Exit if we get a Terminate command or if Delete() has been called.
  } while (c->m_command != HDF4Cmd::Terminate);

  if (HDF_DEBUG)
    std::cout << "HDF4 I/O thread exiting!" << std::endl;

  return NULL;
}
#endif // USE_PTHREADS

std::string HDF4::error_msg() {
  return HEstring((hdf_err_code_t)HEvalue(1));
}







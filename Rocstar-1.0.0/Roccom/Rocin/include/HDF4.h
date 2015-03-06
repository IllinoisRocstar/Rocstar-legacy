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
 ** @file HDF4.h
 ** @brief Declaration of the HDF4 class.
 ** @author Johnny C. Norris II
 **/

#if !defined(_HDF4_H)
# define _HDF4_H

# include <list>
# include <string>
# ifdef USE_PTHREADS
#  include <pthread.h>
#  include "Sync.h"
# endif // USE_PTHREADS
# include <time.h>
# include <hdf.h>

#ifndef MAX_NC_NAME
#define MAX_NC_NAME  H4_MAX_NC_NAME
#define MAX_NC_VARS  H4_MAX_NC_VARS
#endif

class HDF4Cmd;

/**
 ** A class to serialize HDF calls for multithreaded apps.
 **
 ** Since the HDF4 libraries aren't thread-safe, it's necessary to make sure
 ** that two threads don't use HDF routines concurrently.  This class
 ** starts an I/O thread and passes HDF commands to it.  It also provides a
 ** few utility functions, and supports caching of open file descriptors.
 **/
class HDF4
{
  public:
     virtual ~HDF4() = 0;

    /// Create and start the I/O thread.
    static void init();

    /// Destroy the I/O thread.
    static void finalize();

    //@{
    /// HDF4 wrapper functions.
    /// Multi-file interface.
    static intn Hishdf(const char* filename);
    static int32 SDstart(const char* filename, int32 accessMode);
    static intn SDend(int32 sd_id);
    static intn SDfileinfo(int32 id, int32* dsCount, int32* nAttrs);
    static int32 SDcreate(int32 sd_id, const char* name, int32 dType,
                          int32 rank, int32* size);
    static int32 SDselect(int32 sd_id, int32 index);
    static intn SDendaccess(int32 sds_id);
    static int32 SDfindattr(int32 id, const char* attrName);
    static intn SDgetinfo(int32 sds_id, char* name, int32* rank, int32* size,
                          int32* dType, int32* nAttrs);
    static intn SDsetdatastrs(int32 sds_id, const char* label,
                              const char* units, const char* format,
                              const char* coordsys);
    static intn SDgetdatastrs(int32 sds_id, char* label, char* units,
                              char* format, char* coordsys, intn length);
    static intn SDsetrange(int32 sds_id, VOIDP max, VOIDP min);
    static intn SDgetrange(int32 sds_id, VOIDP max, VOIDP min);
    static intn SDwritedata(int32 sds_id, int32* start, int32* stride,
                            int32* end, VOIDP data);
    static intn SDreaddata(int32 sds_id, int32* start, int32* stride,
                           int32* end, VOIDP data);

    /// Single file interface.
    static intn DFSDsetdims(intn rank, int32 dimsizes[]); 
    static intn DFSDsetNT(int32 numbertype);
    static intn DFSDsetdatastrs(const char* label, const char* unit,
                                const char* format, const char* coordsys); 
    static intn DFSDsetrange(VOIDP maxi, VOIDP mini);
    static intn DFSDadddata(const char* filename, intn rank, int32 dimsizes[],
                            VOIDP data);
    static intn DFSDputdata(const char* filename, intn rank, int32 dimsizes[],
                            VOIDP data);
    //@}

    //@{
    /// "Open" and "close" HDF files efficiently.
    // static int32 Start(const std::string& pathname);
    // static intn End(int32 sd_id);
    //@}

    /// Select the first non-"fakeDim" dataset, starting at index.
    static int32 Select(int32 sd_id, int32& index, char* name, int32* rank,
                        int32* size, int32* dType, int32* nAttrs,
                        int32 dsCount = -1);

    /// Get the size in bytes of an HDF data type.
    static int SizeOf(int32 dType);

    /// Terminate the I/O thread.
    static void Terminate();

    /// return error message 
    static std::string error_msg();

# if USE_PTHREADS
  private:
    /// The thread entry point.
    static void* Entry(void* arg);

    /// Common code when posting HDF commands.
    static void PostCommand(HDF4Cmd* pCmd);

    static Semaphore sm_pending;  ///< A semaphore to track pending commands.
    static std::list<HDF4Cmd*> sm_cmdQueue; ///< The command queue.
    static Mutex sm_cs;                     ///< Avoid concurrent accesses.
    static pthread_t sm_id;                 ///< The I/O thread id.
    static int sm_counter;                  ///< The number of HDF4 objects.
# endif // USE_PTHREADS
};

#endif // !defined(_HDF4_H)







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
#if !defined(_SYNC_H)
# define _SYNC_H

# include <pthread.h>

class Mutex
{
  public:
    Mutex();
    ~Mutex();

    inline bool IsOk() const { return m_isOk; }

    int Lock();
    int TryLock();
    int Unlock();

    friend class Condition;

  private:
    pthread_mutex_t m_mutex;
    bool m_isOk;
};

class Condition
{
  public:
    Condition(Mutex& mutex);
    ~Condition();

    inline bool IsOk() const { return (m_isOk && m_mutex.IsOk()); }

    int Wait();
    int Signal();
    int Broadcast();

  private:
    Mutex& m_mutex;
    pthread_cond_t m_cond;
    bool m_isOk;
};

class Semaphore
{
  public:
    Semaphore(int initialcount = 0, int maxcount = 0);
    ~Semaphore();

    inline bool IsOk() const { return m_isOk; }

    bool Wait();
    bool TryWait();
    bool Post();

  private:
    Mutex m_mutex;
    Condition m_cond;
    int m_count;
    int m_maxcount;
    bool m_isOk;
};

#endif // !defined(_SYNC_H)







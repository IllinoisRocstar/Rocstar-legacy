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
#include <iostream>
#include <pthread.h>
#include <errno.h>
#include "Sync.h"
#include "commpi.h"

Mutex::Mutex()
{
  int err = pthread_mutex_init(&m_mutex, NULL);
  m_isOk = (err == 0);
  if (!m_isOk) {
    std::cerr << "Mutex::Mutex(): pthread_mutex_init() returned " << err
              << std::endl;
  }
}

Mutex::~Mutex()
{
  if (m_isOk) {
    int err = pthread_mutex_destroy(&m_mutex);
    if (err != 0 && !COMMPI_Initialized()) {
      std::cerr << "Mutex::~Mutex(): pthread_mutex_destroy() returned " << err
                << std::endl;
    }
  }
}

int Mutex::Lock()
{
  int err = pthread_mutex_lock(&m_mutex);
  switch (err) {
    case 0:
      break;

    case EDEADLK:
      std::cerr << "Mutex::Lock(): mutex deadlock prevented." << std::endl;
      break;

    case EINVAL:
      std::cerr << "Mutex::Lock(): mutex not initialized." << std::endl;
      break;

    default:
      std::cerr << "Mutex::Lock(): pthread_mutex_lock() returned " << err
                << std::endl;
      break;
  }

  return err;
}

int Mutex::TryLock()
{
    int err = pthread_mutex_trylock(&m_mutex);
    switch (err) {
      case 0:
      case EBUSY:
         break;

      case EINVAL:
        std::cerr << "Mutex::TryLock(): mutex not initialized." << std::endl;
        break;

      default:
        std::cerr << "Mutex::TryLock(): pthread_mutex_trylock() returned "
                  << err << std::endl;
        break;
    }

    return err;
}

int Mutex::Unlock()
{
  int err = pthread_mutex_unlock(&m_mutex);
  switch (err) {
    case 0:
    case EPERM:
      break;

    case EINVAL:
      std::cerr << "Mutex::Unlock(): mutex not initialized." << std::endl;
      break;

    default:
      std::cerr << "Mutex::Unlock(): pthread_mutex_unlock() returned " << err
                << std::endl;
      break;
  }

  return err;
}

Condition::Condition(Mutex& mutex)
: m_mutex(mutex)
{
  int err = pthread_cond_init(&m_cond, NULL);

  m_isOk = (err == 0);

  if (!m_isOk) {
    std::cerr << "Condition::Condition(): pthread_cond_init() returned " << err
              << std::endl;
  }
}

Condition::~Condition()
{
  if (m_isOk) {
    int err = pthread_cond_destroy(&m_cond);
    if (err != 0 && !COMMPI_Initialized()) {
      std::cerr << "Condition::~Condition(): pthread_cond_destroy() returned "
                << err << std::endl;
    }
  }
}

int Condition::Wait()
{
  int err = pthread_cond_wait(&m_cond, &(m_mutex.m_mutex));
  if (err != 0)
    std::cerr << "Condition::Wait(): pthread_cond_wait() returned " << err
              << std::endl;

  return err;
}

int Condition::Signal()
{
  int err = pthread_cond_signal(&m_cond);
  if (err != 0)
    std::cerr << "Condition::Signal(): pthread_cond_signal() returned " << err
              << std::endl;

  return err;
}

int Condition::Broadcast()
{
  int err = pthread_cond_broadcast(&m_cond);
  if (err != 0)
    std::cerr << "Condition::Broadcast(): pthread_cond_broadcast() returned "
              << err << std::endl;

  return err;
}

Semaphore::Semaphore(int initialcount, int maxcount)
: m_cond(m_mutex)
{
  if ((initialcount < 0 || maxcount < 0)
      || ((maxcount > 0) && (initialcount > maxcount))) {
    std::cerr << "Semaphore::Semaphore(): invalid initial or maximal count."
              << std::endl;

    m_isOk = false;
  } else {
    m_maxcount = maxcount;
    m_count = initialcount;
  }

  m_isOk = m_mutex.IsOk() && m_cond.IsOk();
}

Semaphore::~Semaphore()
{
}

bool Semaphore::Wait()
{
  m_mutex.Lock();

  while (m_count == 0) {
    if (m_cond.Wait() != 0) {
      m_mutex.Unlock();
      return false;
    }
  }

  --m_count;
  m_mutex.Unlock();

  return true;
}

bool Semaphore::TryWait()
{
  m_mutex.Lock();

  if (m_count == 0) {
    m_mutex.Unlock();
    return false;
  }

  --m_count;
  m_mutex.Unlock();

  return true;
}

bool Semaphore::Post()
{
  m_mutex.Lock();

  if (m_maxcount > 0 && m_count == m_maxcount) {
    m_mutex.Unlock();
    return false;
  }

  ++m_count;

  int result = m_cond.Signal();
  m_mutex.Unlock();

  return (result == 0);
}







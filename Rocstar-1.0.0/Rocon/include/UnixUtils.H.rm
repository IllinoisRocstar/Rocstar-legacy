///
/// \file
/// \ingroup support
/// \brief Unix System Tools interface
///
#ifndef _UNIX_UTIL_H_
#define _UNIX_UTIL_H_

//#include <ostream>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <ctime>
#include <istream>
#include <ostream>
#include <streambuf>
#include <cstdio>
#include <cstring>

#include "primitive_utilities.H"

namespace Sys {
  std::string LogTime();
  void TokenizePath(std::vector<std::string> rv,const std::string &path);
  int TempFileName(std::string &stub);
  int OpenTemp(std::string &stub);
  const std::string Hostname(bool longname = false);
  const std::string StripDirs(const std::string &);
  const std::string CWD();
  void SafeRemove(const std::string &fname,const std::string &ext);
  int ChDir(const std::string &path);
  bool FILEEXISTS(const std::string &fname);
  bool ISDIR(const std::string &fname);
  bool ISLINK(const std::string &fname);
  int CreateDirectory(const std::string &fname);
  const std::string ResolveLink(const std::string &path);
  int Remove(const std::string &fname)
  {
    return(unlink(fname.c_str()));
  }
  class Directory : public std::vector<std::string>
  {
  private:
    std::string _path;
    bool   _good;
    DIR    *_dir;
  public:
    Directory(const std::string &s = "");
    ~Directory();
    int open(const std::string &s = "");
    void close();
    operator void* ();
    bool operator ! ();
  };


  class Environment : public std::vector< std::pair<std::string,std::string> >
  {
  public:
    Environment();
    int SetEnv(const std::string &,const std::string &,bool);
    void UnSetEnv(const std::string &);
#ifndef DARWIN
    int ClearEnv();
#endif
    const std::string GetEnv(const std::string &) const;
    std::string &GetEnv(const std::string &);
    int PutEnv(char *);
    void Refresh();
    char **GetRawEnv();
  private:
    void init();
    std::string empty_string;
  };


  // Josuttis' fdstream class 
  
  /************************************************************
   * fdostream
   * - a stream that writes on a file descriptor
   ************************************************************/
  
  
  class fdoutbuf : public std::streambuf {
  protected:
    int fd;    // file descriptor
  public:
    fdoutbuf() { fd = -1; };
    // constructor
    fdoutbuf (int _fd) : fd(_fd) {
    }
    void Init(int _fd){ fd = _fd; };
    bool Ready(){ return(fd >= 0); };
    virtual int FD() { return fd; };
  protected:
    // write one character
    virtual int_type overflow (int_type c) {
      if (c != EOF) {
	char z = c;
	if (write (fd, &z, 1) != 1) {
	  return EOF;
	}
      }
      return c;
    }
    // write multiple characters
    virtual
    std::streamsize xsputn (const char* s,
                            std::streamsize num) {
      return write(fd,s,num);
    }
  };
  
  class fdostream : public std::ostream {
  protected:
    fdoutbuf buf;
  public:
    fdostream () : std::ostream(0) {};
    fdostream (int fd) : std::ostream(0), buf(fd) {
      rdbuf(&buf);
    }
    int FD() { return(buf.FD()); };
    int Init(int _fd) { buf.Init(_fd); rdbuf(&buf); };
  };
  
  
  /************************************************************
   * fdistream
   * - a stream that writes on a file descriptor
   ************************************************************/
  
  class fdinbuf : public std::streambuf {
  protected:
    int fd;    // file descriptor
  protected:
    /* data buffer:
     * - at most, pbSize characters in putback area plus
     * - at most, bufSize characters in ordinary read buffer
     */
    static const int pbSize = 4;        // size of putback area
    static const int bufSize = 1024;    // size of the data buffer
    char buffer[bufSize+pbSize];        // data buffer
    
  public:
    /* constructor
     * - initialize file descriptor
     * - initialize empty data buffer
     * - no putback area
     * => force underflow()
     */
    fdinbuf (int _fd) : fd(_fd) {
      setg (buffer+pbSize,     // beginning of putback area
	    buffer+pbSize,     // read position
	    buffer+pbSize);    // end position
    }
    
  protected:
    // insert new characters into the buffer
    virtual int_type underflow () {
      using std::memcpy;
      
      // is read position before end of buffer?
      if (gptr() < egptr()) {
	return *gptr();
      }
      
      /* process size of putback area
       * - use number of characters read
       * - but at most size of putback area
       */
      int numPutback;
      numPutback = gptr() - eback();
      if (numPutback > pbSize) {
	numPutback = pbSize;
      }
      
      /* copy up to pbSize characters previously read into
       * the putback area
       */
      memcpy (buffer+(pbSize-numPutback), gptr()-numPutback,
	      numPutback);
      
      // read at most bufSize new characters
      int num;
      num = read (fd, buffer+pbSize, bufSize);
      if (num <= 0) {
	// ERROR or EOF
	return EOF;
      }
      
      // reset buffer pointers
      setg (buffer+(pbSize-numPutback),   // beginning of putback area
	    buffer+pbSize,                // read position
	    buffer+pbSize+num);           // end of buffer
      
      // return next character
      return *gptr();
    }
  };
  
  class fdistream : public std::istream {
  protected:
    fdinbuf buf;
  public:
    fdistream (int fd) : std::istream(0), buf(fd) {
      rdbuf(&buf);
    }
  };
  
  
};
#endif

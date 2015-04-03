///
/// \file
/// \ingroup support
/// \brief Unix System tools implementation
///
#include <cstdio>
#include <sstream>
#include <cstring>
#include <cstdlib>

#include "UnixUtils.H"

namespace Sys {
  const std::string Hostname(bool longname)
  {
    char buf[64];
    gethostname(buf,64);
    return(std::string(buf));
  }
  std::string LogTime()
  {
    time_t t_ptr;
    time(&t_ptr);
    char timesbuf[64];
    std::string format("[%m/%d/%y%%%X]: ");
    strftime(timesbuf,64,format.c_str(),gmtime(&t_ptr));
    std::string retval(timesbuf);
    return(retval);
  }
  void SafeRemove(const std::string &fname,const std::string &ext)
  {
    if(!FILEEXISTS(fname))
      return;
    if(ISLINK(fname))
      unlink(fname.c_str());
    std::string savename_base(fname+"."+ext);
    std::string savename(savename_base);
    unsigned int n = 1;
    while(FILEEXISTS(savename)){
      std::ostringstream Ostr;
      Ostr << savename_base << n++;
      savename.assign(Ostr.str());
    }
    rename(fname.c_str(),savename.c_str());
  }

  bool FILEEXISTS(const std::string &fname)
  {
    struct stat fstat;
    if(lstat(fname.c_str(),&fstat))
      return false;
    return true;
  }

  bool ISDIR(const std::string &fname)
  {
    struct stat fstat;
    if(stat(fname.c_str(),&fstat))
      return false;
    if(S_ISDIR(fstat.st_mode))
      return true;
    return false;
  }

  bool ISLINK(const std::string &fname)
  {
    struct stat fstat;
    if(lstat(fname.c_str(),&fstat))
      return false;
    if(S_ISLNK(fstat.st_mode))
      return true;
    return(false);
  }

  int CreateDirectory(const std::string &fname)
  {
    return(mkdir(fname.c_str(),S_IRGRP | S_IXGRP  | S_IRWXU));
  }

  const std::string ResolveLink(const std::string &path)
  {
    std::string retVal;
    char buf[1024];
    size_t s = readlink(path.c_str(),buf,1024);
    if(!(s <= 0)){
      buf[s] = '\0';
      retVal.assign(buf);
    }
    std::string::size_type x = retVal.find_last_of("/");
    if(x != std::string::npos)
      retVal.erase(x);
    return (retVal);
  }


  Directory::Directory(const std::string &path)
  {
    _good = false;
    _dir = NULL;
    _path.assign(path);
    if(open(path))
      std::cerr << "Directory::Error: Could not open " << path
		<< " as a directory." << std::endl;
  }

  Directory::~Directory()
  {
    if (_good)
      closedir(_dir);
  }

  Directory::operator void* ()
  {
    return(_good ? this : NULL);
  }

  bool Directory::operator ! ()
  {
    return(!_good);
  }

  void Directory::close()
  {
    if(_good)
      closedir(_dir);
  }

  int Directory::open(const std::string &path)
  {
    if(_good){
      this->close();
      _path = path;
    }
    if(path.empty())
      return(1);
    if(!(_dir = opendir(path.c_str())))
      return(1);
    _path = path;
    _good = true;
    struct dirent *entry;
    // Skip . and ..
    entry = readdir(_dir);
    entry = readdir(_dir);
    while((entry = readdir(_dir)) != NULL)
      this->push_back(entry->d_name);
    return(0);
  }

  const std::string CWD()
  {
    char buf[1024];
    return(std::string(getcwd(buf,1024)));
  }

  int ChDir(const std::string &path)
  {
    return(chdir(path.c_str()));
  }

  const std::string
  StripDirs(const std::string &pname)
  {
    std::string retval;
    std::string::size_type x = pname.find("/");
    if(x == std::string::npos)
      return(pname);
    return(pname.substr(pname.find_last_of("/")+1));
  }

  int
  TempFileName(std::string &stub)
  {
    int fd;
    std::string tstub(stub);
    tstub += "XXXXXX";
    char *name = new char [tstub.length() + 1];
    std::strcpy(name,tstub.c_str());
    name[tstub.length()] = '\0';
    fd = mkstemp(name);
    if(!fd)
      stub = name;
    delete [] name;
    return (fd);
  }

  int OpenTemp(std::string &stub)
  {
    int fd;
    std::string tstub(stub);
    tstub += "XXXXXX";
    char *name = new char [tstub.length() + 1];
    std::strcpy(name,tstub.c_str());
    name[tstub.length()] = '\0';
    fd = mkstemp(name);
    stub = name;
    delete [] name;
    return(fd);
  }
  // Tokenize Path
  // Takes an input path and makes string tokens of each
  // directory and the final argument
  void
  TokenizePath(std::vector<std::string> rv,const std::string &path)
  {
    rv.resize(0);
    std::istringstream Istr(path);
    std::string tok;
    while(std::getline(Istr,tok,'/'))
      if(!tok.empty())
	rv.push_back(tok);
  }

  Environment::Environment()
  {
    this->init();
  }

  void
  Environment::init()
  {
    this->clear();
    std::vector<std::string> raw_env;
    Vectorize(raw_env,(const char **)environ);
    std::vector<std::string>::iterator rei = raw_env.begin();
    while(rei != raw_env.end()){
      unsigned int x = (*rei).find("=");
      std::string var((*rei).substr(0,x));
      std::string val((*rei).substr(x+1));
      this->push_back(make_pair(var,val));
      rei++;
    }
  }

  int
  Environment::SetEnv(const std::string &var,const std::string &val,bool ow)
  {
    int retVal = setenv(var.c_str(),val.c_str(),(int)ow);
    this->init();
    return(retVal);
  }

  void
  Environment::UnSetEnv(const std::string &var)
  {
    unsetenv(var.c_str());
    this->init();
  }

#ifndef DARWIN
  int
  Environment::ClearEnv()
  {
    clearenv();
    this->init();
    return(1);
  }
#endif

  const std::string
  Environment::GetEnv(const std::string &var) const
  {
    Environment::const_iterator ei = this->begin();
    while(ei != this->end()){
      if((*ei).first == var)
	return((*ei).second);
      ei++;
    }
    return(std::string(""));
  }

  std::string &
  Environment::GetEnv(const std::string &var)
  {
    Environment::iterator ei = this->begin();
    while(ei != this->end()){
      if((*ei).first == var)
	return((*ei).second);
      ei++;
    }
    empty_string.clear();
    return(empty_string);
  }

  int
  Environment::PutEnv(char *envs)
  {
    int retVal = putenv(envs);
    this->init();
    return(retVal);
  }

  void
  Environment::Refresh()
  {
    this->init();
  }

  char **
  Environment::GetRawEnv()
  {
    return(environ);
  }

  std::ostream& operator<<(std::ostream &output,const Environment &env)
  {
    std::vector< std::pair<std::string,std::string> >::const_iterator ei = env.begin();
    while(ei != env.end()){
      output << (*ei).first << " = " << (*ei).second << std::endl;
      ei++;
    }
    return(output);
  }

};

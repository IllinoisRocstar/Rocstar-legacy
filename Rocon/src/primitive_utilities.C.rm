///
/// \file
/// \ingroup support
/// \brief Primitive utils implementation
///
#include <sstream>
#include <cstring>

#include "primitive_utilities.H"


std::string GetNextContent(std::istream &In)
{
  std::string line;
  std::string ret;
  while(ret.empty() && std::getline(In,line)){
    std::istringstream Istr(line);
    std::string token;
    Istr >> token;
    if(!token.empty() && token[0] != '#')
      ret.assign(line);
  }
  return(ret);
}

void GetContentUntil(std::istream &In,
		     std::string ret,
		     const std::string &etag)
{
  std::ostringstream Ostr;
  std::string line;
  while(std::getline(In,line) && ret.empty()){
    std::istringstream Istr(line);
    std::string token;
    Istr >> token;
    if(!token.empty()){
      if(token == etag)
	ret = Ostr.str();
      else if (token[0] != '#'){
	if(!Ostr.str().empty())
	  Ostr << "\n";
	Ostr << line;
      }
    }
  }
}

int String2Buf(const std::string &instr,void **buf)
{
  if(instr.empty())
    return 0;
  int rval = instr.size();
  *buf = new char [rval];
  void *resl = std::memcpy(*buf,instr.c_str(),rval);
  if(!resl)
    return(0);
  return(rval);
}

void Trim(std::string &instr,bool preserve_newline)
{
  std::istringstream Istr(instr);
  std::string line;
  std::ostringstream Ostr;
  while(std::getline(Istr,line)){
    std::istringstream Line(line);
    std::string token;
    Line >> token;
    Ostr << token;
    while(Line >> token)
      if(!token.empty())
	Ostr << " " << token;
    if(preserve_newline)
      Ostr << "\n";
  }
  instr = Ostr.str();
}

const std::string Trimmed(const std::string &instr,bool preserve_newline)
{
  std::istringstream Istr(instr);
  std::string line;
  std::ostringstream Ostr;
  while(std::getline(Istr,line)){
    std::istringstream Line(line);
    std::string token;
    Line >> token;
    Ostr << token;
    while(Line >> token)
      if(!token.empty())
	Ostr << " " << token;
    if(preserve_newline)
      Ostr << "\n";
  }
  return(Ostr.str());
}

const std::string 
stripdirs(const std::string &pname)
{
  std::string retval;
  std::string::size_type x = pname.find("/");
  if(x == std::string::npos)
    return(pname);
  return(pname.substr(pname.find_last_of("/")+1));
}

void
TokenizeString(std::vector<std::string> &tokens,const std::string &source)
{
  tokens.resize(0);
  std::istringstream Istr(source);
  std::string token;
  while(Istr >> token)
    tokens.push_back(token);
}

void
TokenizeString(std::vector<std::string> &tokens,const std::string &source,const char delim)
{
  tokens.resize(0);
  std::string::size_type ssize = source.size();
  std::string::size_type x = 0;
  std::ostringstream Ostr;
  std::string token;
  while(x < ssize){
    if(source[x] != delim)
      token += source[x];
    else if(!token.empty())
      tokens.push_back(token);
    x++;
  }
}

int OpenFile(std::ifstream &Inf,const std::string &filename)
{
  Inf.open(filename.c_str());
  if(!Inf)
    return(-1);
  return 0;
}

// Utilities
void
Vectorize(std::vector<std::string> &retVal,const char **in)
{
  int i = 0;
  while(in[i] != NULL)
    retVal.push_back(in[i++]);
}

void
Vectorize(std::vector<std::string> &retVal,const char **in,int n)
{
  //  retVal.resize(0);
  if(n <= 0) return;
  int i = 0;
  while((in[i] != NULL) && i < n)
    retVal.push_back(in[i++]);
}


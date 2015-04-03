///
/// \file
/// \ingroup support
/// \brief Parameters object implementation
///
#include "Parameters.H"


namespace Util {
  int Util::Parameters::ReadFromStream(std::istream &Is)
  {
    std::string line;
    int n = 0;
    while(Is){
      std::getline(Is,line);
      n++;
      // Removes any leading whitespace
      std::string::size_type x = line.find('#');
      line = line.substr(0,x);
      if(!line.empty()){
	x = line.find('=');
	if(x == std::string::npos)
	  return(n);
	Util::ParamType param;
	std::istringstream Instr(line.substr(0,x));
	Instr >> param.Key();
	std::vector<std::string> tokens;
	line = line.substr(x+1,line.size());
	TokenizeString(tokens,line);
	std::ostringstream Ostr;
	std::vector<std::string>::iterator ti = tokens.begin();
	if(ti != tokens.end())
	  Ostr << *ti++;
	while(ti != tokens.end())
	  Ostr << " " << *ti++;
	param.Value() = Ostr.str();
	this->push_back(param);
      }
    }
    return(0);
  }

  std::string Util::Parameters::Param(const std::string &key) const
  {
    std::string empty;
    Util::Parameters::const_iterator pi = this->begin();
    while(pi != this->end()){
      if(pi->first == key)
	return(pi->second);
      pi++;
    }
    return(empty);
  }
  
  std::ostream &operator<<(std::ostream &oSt,
			   const Util::Parameters &pv)
  {
    Util::Parameters::const_iterator pi = pv.begin();
    while(pi != pv.end()){
      oSt << *pi++;
      oSt << (pi == pv.end() ? "" : "\n");
    }
    return(oSt);
  }

  std::istream &operator>>(std::istream &iSt,
			   Util::Parameters &pv)
  {
    pv.ReadFromStream(iSt);
    return(iSt);
  }

}
std::ostream &operator<<(std::ostream &Ostr,const Util::ParamType &param)
{
  Ostr << param.Key() << " = " << param.Value();
  return(Ostr);
}


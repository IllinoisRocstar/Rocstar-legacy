///
/// \file
/// \ingroup support
/// \brief Parameters object interface (for config files, etc)
///
#ifndef _PARAMETERS_H_
#define _PARAMETERS_H_

#include <sstream>

#include "primitive_utilities.H"

namespace Util {

  typedef KeyValuePairObj<std::string,std::string> ParamType;
  class Parameters : public std::vector<Util::ParamType>
  
  {
    friend std::ostream &operator<<(std::ostream &oSt,
				    const Util::Parameters &pv);
    friend std::istream &operator>>(std::istream &iSt,
				    Util::Parameters &pv);
  public:
    template<typename T>
    T GetValue(const std::string &key) const
    {
      T retval;
      std::string value(this->Param(key));
      if(!value.empty()){
	std::istringstream Istr(value);
	Istr >> retval;
      }
      return(retval);
    };
    
    template<typename T>
    std::vector<T> GetValueVector(const std::string &key) const
    {
      std::vector<T> retval;
      std::string value(this->Param(key));
      if(!value.empty()){
	std::istringstream Istr(value);
	T tmpval;
	while(Istr >> tmpval)
	  retval.push_back(tmpval);
      }
      return(retval);
    };
    int ReadFromStream(std::istream &Is);
    std::string Param(const std::string &Key) const;
  };
}
std::ostream &operator<<(std::ostream &Ostr,const Util::ParamType &param);    
#endif

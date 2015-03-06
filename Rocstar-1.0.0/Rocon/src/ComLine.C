///
/// \file
/// \ingroup support
/// \brief ComLineObject implementation
///
/// Implements functions belonging to the ComLineObject class.
///
#include <sstream>

#include "ComLine.H"
#include "primitive_utilities.H"

std::istream &operator>>(std::istream &In,ComLineObject &cl){
  std::string line;
  std::string::size_type x;
  line = GetNextContent(In);
  x = line.find("<description>");
  if(x != std::string::npos){
    GetContentUntil(In,cl._description,"</description>");
    line = GetNextContent(In);
  }
  x = line.find("<options>");
  if(x == std::string::npos){
    cl._error_messages.push_back("Config file format error - options section misplaced?.");
    return(In);
  }
  std::string options_content;
  GetContentUntil(In,options_content,"</options>");
  std::istringstream Istr(options_content);
  while(std::getline(Istr,line)){
    std::istringstream OpIn(line);
    std::string token;
    char opchar;
    int optype;
    OpIn >> opchar;
    OpIn >> token;
    OpIn >> optype;
    cl.push_back(std::make_pair(opchar,token));
    cl._type[opchar] = optype;
    if(optype > 0){
      std::string token;
      OpIn >> token;
      cl._argname[opchar] = token;
    }
    std::getline(Istr,line);
    x = line.find("<help>");
    if(x == std::string::npos){
      cl._error_messages.push_back("Configuration input format error in option help section.");
      return(In);
    }
    cl._help[opchar] = "";
    GetContentUntil(Istr,cl._help[opchar],"</help>");
  }
  line = GetNextContent(In);
  if(line.empty())
    return(In);
  x = line.find("<arguments>");
  if(x == std::string::npos){
    cl._error_messages.push_back("Configuration intput format error in argument section.");
    return(In);
  }
  std::string argsection;
  GetContentUntil(In,argsection,"</arguments>");
  Istr.str(argsection);
  while(std::getline(Istr,line)){
    std::istringstream ArgIn(line);
    std::string argname;
    int argtype;
    ArgIn >> argname >> argtype;
    cl._arghelp[argname] = argtype;
    std::string tag;
    ArgIn >> tag;
    std::string::size_type x = tag.find("<help>");
    if(x == std::string::npos){
      cl._error_messages.push_back("Configuration input format error in arghelp section.");
      return(In);
    }
    cl._arghelp[argname] = "";
    GetContentUntil(In,cl._arghelp[argname],"</help>");
  }
  return(In);
}

std::ostream &operator<<(std::ostream &Out,const ComLineObject &cl)
{
  if(!cl._description.empty())
    Out << "<description>\n" << cl._description << "</description>\n";
  Out << "<options>\n";
  ComLineObject::const_iterator cli = cl.begin();
  while(cli != cl.end()){
    char opchar = cli->first;
    std::map<char,int>::const_iterator ti = cl._type.find(opchar);
    Out << cli->first << " " << cli->second << " " 
	<< (ti == cl._type.end() ? 0 : ti->second);
    if(ti != cl._type.end()){
      std::map<char,std::string>::const_iterator ai = cl._argname.find(opchar);
      Out << " " << (ai == cl._argname.end() ? "(unspecified)" : ai->second);
    }
    std::map<char,std::string>::const_iterator hi = cl._help.find(opchar);
    Out << "\n<help>\n" << (hi == cl._help.end() ? "(unspecified)" : hi->second) 
	<< "\n</help>\n";
    cli++;
  }
  Out << "</options>\n";
  if(!cl._args.empty()){
    Out << "<arguments>\n";
    std::vector<std::pair<std::string,int> >::const_iterator ai = cl._args.begin();
    while(ai != cl._args.end()){
      std::map<std::string,std::string>::const_iterator ahi = cl._arghelp.find(ai->first);
      Out << ai->first << " " << ai->second
	  << "\n<help>\n" << (ahi == cl._arghelp.end() ? "(unspecified)" : ahi->second) 
	  << "\n</help>\n";
      ai++;
    }
    Out << "</arguments>\n";
  }
  return(Out);
}


std::string ComLineObject::LongUsage(){
  std::ostringstream Ostr;
  if(!_description.empty())
    Ostr << _description << std::endl << std::endl;
  Ostr << _program_name << " Usage: " << std::endl
       << std::endl << ShortUsage() << std::endl << std::endl;
  std::vector<std::pair<char,std::string> >::const_iterator ti = this->begin();
  while(ti != this->end()){
    Ostr << "\t" << "-" << ti->first << ",--" << ti->second 
	 << (_type[ti->first] > 0 ? (_type[ti->first]==1 ? " [" : " <") : "")
	 << _argname[ti->first]
	 << (_type[ti->first] > 0 ? (_type[ti->first]==1 ? "]" : ">") : "")
	 << std::endl << "\t\t" << _help[ti->first];
    ti++;
    if(ti != this->end())
      Ostr << std::endl << std::endl;
  }
  Ostr << std::endl << std::endl;
  std::vector<std::pair<std::string,int> >::const_iterator ai = _args.begin();
  while(ai != _args.end()){
    Ostr << "\t" << (ai->second ? "<" : "[") << ai->first  
	 << (ai->second ? ">" : "]") << std::endl << "\t\t" 
	 << _arghelp[ai->first];
    ai++;
    if(ai != _args.end())
      Ostr << std::endl << std::endl;
  }
  if(!_notes.empty()) 
    Ostr << std::endl << _notes;
  return(Ostr.str());
}

std::string ComLineObject::GetLong(const char &s)
{
  std::vector<std::pair<char,std::string> >::iterator ti = this->begin();
   while(ti != this->end()){
     if(ti->first == s)
       return(ti->second);
     ti++;
   }
   return("");
}

std::string ComLineObject::GetOpStringByType(int mintype,int maxtype)
{
  std::ostringstream Ostr;
  std::vector<std::pair<char,std::string> >::iterator oi = this->begin();
  while(oi != this->end()){
    if(_type[oi->first] >= mintype && _type[oi->first] <= maxtype)
      Ostr << oi->first;
    oi++;
  }
  return(Ostr.str());
}

std::string ComLineObject::ShortUsage(){
  std::ostringstream Ostr;
  std::string flagstring = GetOpStringByType(0,0); 
  Ostr << _program_name << " ";
  if(!flagstring.empty())
    Ostr << "[-" << flagstring << "] ";
  std::string optionals = GetOpStringByType(1,2);
  if(!optionals.empty()){
    std::string::iterator oi = optionals.begin();
    Ostr << "[";
    while(oi != optionals.end()){
      Ostr << "-" << *oi 
	   << (_type[*oi] == 1 ? " [" : " <")
	   << _argname[*oi]  
	   << (_type[*oi] == 1 ? "] " : "> ");
      oi++;
    }
    Ostr << "] ";
  }
  std::string reqd = GetOpStringByType(3,3);
  if(!reqd.empty()){
    std::string::iterator oi = reqd.begin();
    Ostr << "<";
    while(oi != reqd.end()){
      Ostr << "-" << *oi << " <" << _argname[*oi]  
	   << "> ";
      oi++;
    }
    Ostr << "> ";
  }
  std::vector<std::pair<std::string,int> >::iterator ai = _args.begin();
  while(ai != _args.end()){
    Ostr << (ai->second > 0 ? "<" : "[") << ai->first 
	 << (ai->second > 0 ? "> ": "] ");
    ai++;
  }
  return(Ostr.str());
}

char ComLineObject::GetShort(const std::string &l){
  std::vector<std::pair<char,std::string> >::iterator ti = this->begin();
  while(ti != this->end()){
    if(ti->second == l)
      return(ti->first);
    ti++;
  }
  return('\0');
}

void ComLineObject::AddOption(char s,const std::string &l,int atype){
  this->push_back(std::make_pair<char,std::string>(s,l));
  _options[s] = std::string("");
  _type[s] = atype;
  if(atype > 0)
    _argname[s] = "arg";
  _help[s] = std::string("");
}

void ComLineObject::AddOption(char s,const std::string &l,int type,const std::string argname){
  this->push_back(std::make_pair<char,std::string>(s,l));
  _options[s] = std::string("");
  _type[s] = type;
  _argname[s] = argname;
  _help[s] = std::string("");
}

void ComLineObject::Record(const char *args[]){
  int i = 1;
  std::ostringstream Ostr;
  _program_name.assign(stripdirs(args[0]));
  while(args[i]){
    Ostr << args[i++];
    if(args[i])
      Ostr << " ";
  }
  _line.assign(Ostr.str());
}

int ComLineObject::ProcessOptions() {
  bool end_of_ops = false;
  int errs = 0;
  std::vector<std::string> args;
  TokenizeString(args,_line);
  std::vector<std::string>::iterator ai = args.begin();
  while(ai != args.end()){
    std::string this_argument = *ai++;
    std::string next_argument;
    if(ai != args.end())
      next_argument = *ai--;
    std::string::iterator si = this_argument.begin();
    if(*si == '-' && !end_of_ops){
      si++;
      if(si == this_argument.end())
	end_of_ops = true;
      if(*si != '-') { // then we are processing a short option
	while(si != this_argument.end()){
	  char flag_char = *si++;
	  std::vector<std::pair<char,std::string> >::iterator oi = this->begin();
	  bool found = false;
	  while((oi != this->end()) && !found){
	    if(flag_char == oi->first){
	      found = true;
	      if(si == this_argument.end()){  // means it's not a string of flags
		if(_type[flag_char] != 0){
		  if(!next_argument.empty() && next_argument[0] != '-'){
		    _options[flag_char] = next_argument;
		    ai++;
		  }
		  else if(_type[flag_char] > 1){ // next arg wasn't valid, err if reqd
		    errs++;
		    std::ostringstream Ostr;
		    Ostr << "Option -" << flag_char << " requires an argument.";
		    _error_messages.push_back(Ostr.str());
		  }
		  else
		    _options[flag_char] = ".true.";
		}
		else
		  _options[flag_char] = ".true.";
	      }
	      else{ // it's a string of flags
		if(_type[flag_char] > 1){ // it requires an argument, can't be in a flag string
		  errs++;
		  std::ostringstream Ostr;
		  Ostr << "Option -" << flag_char << " requires an argument.";
		  _error_messages.push_back(Ostr.str());
		}
		else
		  _options[flag_char] = ".true.";
	      }
	    }
	    oi++;
	  }
	  if(!found){
	    errs++;
	    std::ostringstream Ostr;
	    Ostr << "Option -" << flag_char << " is unrecognized.";
	    _error_messages.push_back(Ostr.str());
	  }
	}
      }
      else { // we are processing a long option
	std::string opstring = this_argument.substr(2);
	char flag_char = GetShort(opstring);
	if(flag_char != '\0'){
	  if(_type[flag_char] != 0){
	    if(!next_argument.empty() && next_argument[0] != '-'){
	      _options[flag_char] = next_argument;
	      ai++;
	    }
	    else if(_type[flag_char] > 1){
	      errs++;
	      std::ostringstream Ostr;
	      Ostr << "Option --" << GetLong(flag_char)
		   << " requires an argument.";
	      _error_messages.push_back(Ostr.str());
	    }
	    else
	      _options[flag_char] = ".true.";
	  }
	  else
	    _options[flag_char] = ".true.";
	}
	else{
	  errs++;
	  std::ostringstream Ostr;
	  Ostr << "Option --" << opstring << " is unrecognized.";
	  _error_messages.push_back(Ostr.str());
	}
      }
    }
    else { // non option arguments
      _nonops.push_back(this_argument);
    }
    if(ai != args.end())
      ai++;
  }
  // After finished parsing all arguments, go back and determine
  // whether any required input was missing.
  // 
  // First, process the options
  std::vector<std::pair<char,std::string> >::iterator oi = this->begin();
  while(oi != this->end()){
    if(GetOption(oi->first).empty() && _type[oi->first]==3){
      errs++;
      std::ostringstream Ostr;
      Ostr << "Required option: -" << oi->first << ",--" << oi->second 
	   << " <" << _argname[oi->first] << "> was not specified.";
      _error_messages.push_back(Ostr.str());
    }
    oi++;
  }
  // Next, make sure there were enough nonop args
  unsigned int nreqd_args = 0;
  std::vector<std::pair<std::string,int> >::iterator aai = _args.begin();
  while(aai != _args.end()){
    if(aai->second > 0)
      nreqd_args++;
    aai++;
  }
  if(nreqd_args > _nonops.size()){
    errs++;
    std::ostringstream Ostr;
    Ostr << "Missing required arguments.";
    _error_messages.push_back(Ostr.str());
  }
  return(errs);
}

std::string ComLineObject::ErrorReport()
{
  std::ostringstream Ostr;
  Ostr << _program_name << " command line errors: " << std::endl;
  DumpContents(Ostr,_error_messages);
  return(Ostr.str());
}

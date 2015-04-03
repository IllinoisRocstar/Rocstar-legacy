/// Profile analysis
#include <sstream>
#include <fstream>
#include <cassert>
#include <cmath>
#include <iomanip>
#include <cstdlib>

using namespace std;

#include "Profiler.H"
#include "ComLine.H"


///
/// \brief ComLineObject for profane
///
class ProfaneComLine : public ComLineObject
{
public:
  ProfaneComLine(const char *args[])
    : ComLineObject(args)
  {};
  void Initialize(){
    AddOption('h',"help");
    AddOption('s',"scalability");
    AddOption('f',"fixed");
    AddOption('v',"verb",1,"level");
    AddOption('c',"config",2,"configfile");
    AddOption('o',"out",2,"outputfile");
    AddArgument("input_files",1);
    AddHelp("help","Prints this long version of help.");
    AddHelp("verb","Makes the test more verbose. Default level is 1.");
    AddHelp("config","Specifies the name of the configuration file.");
    AddHelp("out","Specifies the name of the output file.");
    AddArgHelp("input_files","Space delimited list of input profiles.");
    std::ostringstream Ostr;
    Ostr << "Use fixed problem size in scalability analysis.  Only makes"
	 << "\n\t\tsense when scalability mode is enabled.";
    AddHelp("fixed",Ostr.str());
    AddHelp("scalability","Enable scalability mode.");
    Ostr.str("");
    Ostr << "Performance analysis tool for analyzing profiles produced"
	 << "\nby the Profiler utility.";
    _description.assign(Ostr.str());
  };
};


int
main(int argc,char *argv[])
{
  ProfaneComLine comline((const char **)argv);
  comline.Initialize();
  int clerr = comline.ProcessOptions();
  std::string cfname = comline.GetOption("config");
  std::string sverb = comline.GetOption("verb");
  bool scalamode = !comline.GetOption("scalability").empty();
  std::string stat_summary_name = comline.GetOption("out");
  bool write_summary_outfile = !stat_summary_name.empty();
  bool is_fixed = !comline.GetOption("fixed").empty();
  if(!comline.GetOption("help").empty()){
    std::cout << comline.LongUsage() << std::endl;
    return(0);
  }
  if(clerr){
    std::cerr << comline.ErrorReport() << std::endl
	      << std::endl << comline.ShortUsage() << std::endl;
    return(1);
  }
  int verblevel = 0;
  if(!sverb.empty() && sverb != ".true."){
    std::istringstream Istr(sverb);
    Istr >> verblevel;
  }
  std::ofstream Ouf;
  if(write_summary_outfile){
    Ouf.open(stat_summary_name.c_str());
    if(!Ouf){
      std::cerr << comline.ProgramName() << "::Error: Could not"
		<< " open output file, " << stat_summary_name
		<< ", for output. Continuing with stdout."
		<< std::endl;
    }
  }
  std::vector<std::string> infiles = comline.GetArgs();
  if(infiles.size()==0) {
    std::cerr << "Profane::Error: No input files specified." << std::endl
	      << std::endl << comline.ShortUsage() << std::endl;
    return(1);
  }
  Profiler::ProfilerObj profiler;
  if(verblevel > 1){
    profiler.SetOut(&std::cout);
    profiler.SetErr(&std::cerr);
  }
  if(cfname.empty()){
    std::string::size_type x = infiles[0].find("prof_");
    cfname.assign(infiles[0].substr(0,x));
    cfname += "rpconfig";
    std::ifstream Inf;
    Inf.open(cfname.c_str());
    if(!Inf)
      cfname.assign("profiler.rpconfig");
    else
      Inf.close();
  }
  if(profiler.ReadConfig(cfname) && !cfname.empty())
    std::cerr << comline.ProgramName()
	      << "::Warning: unable to read config file, " << cfname
	      << ". Continuing without configuration." << std::endl;
  if(!scalamode){
    if(infiles.size() == 1){
      if(profiler.ReadEventsFromFile(infiles[0]))
 	exit(1);
      profiler.SummarizeSerialExecution(std::cout);
    }
    else{
      Profiler::PEventList parallel_event_list;
      if(profiler.ReadParallelEventFiles(infiles,parallel_event_list))
	exit(1);
      profiler.SummarizeParallelExecution(std::cout,Ouf,parallel_event_list);
    }
  }
  else{
    //     if(infiles.size() == 1){
    //       if(!read_scalability_from_file(*ai))
    // 	exit(1);
    //       scalability_summary();
    //     }
    //     else{
    Profiler::ScalaMap scala_map;
    if(profiler.ReadSummaryFiles(infiles,scala_map)){
      std::cerr << comline.ProgramName() << ": Error: Could not process "
		<< "summary files." << std::endl;
      return(1);
    }
    Profiler::ScalaStatMap scala_statmap;
    if(profiler.PopulateScalaMap(scala_map,scala_statmap,!is_fixed)){
      std::cerr << comline.ProgramName()
		<< ": Error: Unable to calculate scalability information."
		<< std::endl;
      return(1);
    }
    if(profiler.ScalabilitySummary(scala_statmap,std::cout)){
      std::cerr << comline.ProgramName()
		<< ": Error: Unabled to perform scalability summary."
		<< std::endl;
      return(1);
    }
    //     }
  }
  return(0);
}

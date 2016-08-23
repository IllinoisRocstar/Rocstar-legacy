#ifndef __SOLVER_MODULE_DRIVER_H__
#define __SOLVER_MODULE_DRIVER_H__

#include "SolverAgent.H"
#include "InterfaceLayer.H"

class SolverModuleDriver : public SolverUtils::FEM::SolverAgent, public COM_Object
{
  std::vector<double> tNext;
  std::stringstream ss;
  double var;
  bool isNum, isFSI, changeLoads;
  double* Disp;
  int DispSize;
  std::vector<double> DispPass;
  double* Coord;
  int* Conn;
  int coord_handle;
  int CoordSize;
  bool coordData;
  bool connCorrect;
  int ConnSize;
  std::vector<unsigned int> elems;
  double* Loads;
  int LoadsSize;
  std::vector<double> LoadsPass;
  int runs;

public:

  SolverModuleDriver(){};
  virtual ~SolverModuleDriver(){};
  void usage(char *exec);     
  int init(int argc, char *argv[]);
  int run();
  int finalize();
};
  
#endif

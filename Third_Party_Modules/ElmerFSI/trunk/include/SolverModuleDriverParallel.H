#ifndef __SOLVER_MODULE_DRIVER_H__
#define __SOLVER_MODULE_DRIVER_H__

#include "SolverAgent.H"
#include "InterfaceLayer.H"

namespace COM {
   class SolverModuleDriver : public SolverUtils::FEM::SolverAgent, public COM_Object
   {
     std::stringstream ss;
     bool isFSI, changeLoads;
     int runs;
     int color;
     MPI_Comm Comm;
     SolverAgent myAgent;
     bool myAgentIsInit;

   public:
     // member data
     int myRank;
     int myPaneId;
     std::vector<double> tNext;
     double* Disp; // displacements
     int nDisp;
     char locDisp;
     int typeDisp;
     double* Coord;
     int nNodes;
     int* Conn;
     int nConn;
     double* Loads; // loads
     int nLoads;    // number of components 
     char locLoads; // location on mesh
     int typeLoads; // COM type 
     double* Press; // pressures
     int nPress;
     char locPress;
     int typePress;
     int nElem;
     int nElemNodes;
     std::ofstream outfile;
     std::string prefix;

     // member functions
     SolverModuleDriver(): myAgentIsInit(false), isFSI(false), changeLoads(false), Disp(NULL), nDisp(0),
                           Loads(NULL), nLoads(0), Press(NULL), nPress(0), Coord(NULL), Conn(NULL),
                           prefix("win")
                           {}
     virtual ~SolverModuleDriver(){};
     void usage(char *exec);    
     bool isStreamer(){return(myRank==0);} 
     int init(int argc, char *argv[]);
     int run();
     int updateSolution();
     int finalize();
     int getColor(){return color;}
     void setColor(int colorValue){color = colorValue;}
     void setRank(int rankValue){myRank = rankValue;}
     void setComm(MPI_Comm newComm){Comm = newComm;}
     int vtkDump(double timeMark);
     bool isFSISim(){return isFSI;}
     bool isChangeLoad(){return changeLoads;}
   
   };
};
  
#endif

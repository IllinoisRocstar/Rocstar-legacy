
// includes for IMPACT
#include "com.h"
#include "com_devel.hpp"
#include "SolverAgent.H"
#include "InterfaceLayer.H"

#include "fvCFD.H"
#include "dynamicFvMesh.H"

#include "constitutiveModel.H"
#include "solidTractionFvPatchVectorField.H"
#include "volPointInterpolation.H"
#include "pointPatchInterpolation.H"

#include "patchToPatchInterpolation.H"
#include "movingWallVelocityFvPatchVectorField.H"

#include "tetFemMatrices.H"
#include "tetPointFields.H"
#include "faceTetPolyPatch.H"
#include "tetPolyPatchInterpolation.H"
#include "fixedValueTetPolyPatchFields.H"
#include "fixedValuePointPatchFields.H"

#include "OFstream.H"
#include "EulerDdtScheme.H"
#include "backwardDdtScheme.H"

#include "pointFields.H"
#include "fixedGradientFvPatchFields.H"
#include "primitivePatchInterpolation.H"
#include "twoDPointCorrector.H"
#include "scalarIOField.H"
#include "leastSquaresVolPointInterpolation.H"
#include "symmetryPolyPatch.H"

class fsifoam_module : public COM_Object , public SolverUtils::FEM::SolverAgent
{
private:
  std::string my_window_name; /// Tracks *this* window name.
  std::string srfWin; /// Tracks interface data.
  std::string volWin; /// Tracks volumetric data.
  MPI_Comm my_window_comm; /// Tracks window's communicator.

protected:
  // Communicator data
  int rank;
  int nproc;

  // ====== Data to register with IMPACT =======
  //  Surface Mesh
  int numPointsSurface;
  int numElementsSurface;
  std::vector<double> surfaceCoordinates;
  std::vector<unsigned int> surfaceConnectivity;
  std::map<int, int> surfaceNodeMap;
  std::map<int, int> interfaceToFoamNodeMap;
  std::map<int, int> foamGlobalToPatchNodeMap;
  std::vector<double> surfacePressure;
  std::vector<double> surfaceTraction;
  std::vector<double> surfaceTractionOld;
  std::vector<double> surfaceDisplacement;   // current timestep displacements
  std::vector<double> solidDisplacement;     // previous timestep displacements
  std::vector<double> solidDisplacementOld;
  std::vector<double> time;
  std::vector<double> deltaTime;
  int verbosity;

  // Surface Mesh data for Rocstar
  std::vector<double> pf;         // cell pressure, 1
  std::vector<double> du_alp;     // points displacement, 3
  std::vector<double> rhof_alp;   // cell density, 1
  std::vector<double> nf_alp;     // cell normal, 3
  std::vector<double> tf;         // cell tractions, 3
  std::vector<double> mdot_alp;   // cell mass flux, 1
  std::vector<double> rhofvf_alp; // cell ?, 3


  

  // Volume Mesh
  int numPointsVol;
  int numElementsVol;
  std::vector<double> volumeCoordinates;
  std::vector<unsigned int> volumeConnectivity;
  std::map<int, int> volumeNodeMap;


  // function status variables
  std::vector<int> initStatus;
  std::vector<int> runStatus;

  // timing variables
  std::vector<double> endTime;

  // ====== OpenFOAM stuff ========
  //
  //    General, init stuff
  Foam::argList *argsPtr;
  Foam::Time *runTimePtr;
  //
  //    Fluid data
  autoPtr<dynamicFvMesh> meshPtr;
  IOdictionary *transportPropertiesPtr;
  dimensionedScalar *nuPtr;
  dimensionedScalar *rhoFluidPtr;
  volScalarField *pPtr;
  volVectorField *UPtr;
  surfaceScalarField *phiPtr;
  label pRefCell;
  scalar pRefValue;
  //
  //   Structures data
  fvMesh *stressMeshPtr;
  volVectorField *DUPtr;
  volTensorField *gradDUPtr;
  volVectorField *UsolidPtr;
  volVectorField *DVPtr;
  volVectorField *VsPtr;
  volSymmTensorField *sigmaPtr;
  volSymmTensorField *DSigmaPtr;
  constitutiveModel *rheologyPtr;
  volScalarField *rhoPtr;
  volScalarField *muPtr;
  volScalarField *lambdaPtr;
  volTensorField *FPtr;
  volTensorField *DFPtr;
  volScalarField *JPtr;
  word *solidDdtSchemePtr;
  pointMesh *pStressMeshPtr;
  pointVectorField *pointDUPtr;
  // label pRefCell;
  // scalar pRefValue;

  //
  //   Coupling data
  IOdictionary *couplingPropertiesPtr;
  word solidPatchName;
  label solidPatchID;
  word solidZoneName;
  label solidZoneID;
  word fluidPatchName;
  label fluidPatchID;
  word fluidZoneName;
  label fluidZoneID;
  bool feMotionSolver;
  bool fvMotionSolver;
  IOobject *accumulatedFluidInterfaceDisplacementHeaderPtr;
  vectorField *accumulatedFluidInterfaceDisplacementPtr;
  //  solidTractionFvPatchVectorField *tForcePtr;
  zoneToZoneInterpolation *interpolatorFluidSolidPtr;
  zoneToZoneInterpolation *interpolatorSolidFluidPtr;
  pointPatchInterpolation *patchPointInterpolatorPtr;

  //
  // Stepper junk
  scalar sumLocalContErr;
  scalar globalContErr;
  scalar cumulativeContErr;
  labelList globalFaceZones;
  labelListList globalToLocalFaceZonePointMap;
  int nCorr;
  int nNonOrthCorr;
  bool momentumPredictor;
  bool transonic;
  int nOuterCorr;
  //  label outerCorr;
  scalar CoNum;
  scalar meanCoNum;
  scalar velMag;
  word couplingScheme;
  scalar interfaceDeformationLimit;
  IOdictionary *dynamicMeshDictPtr;
  scalar fsiRelaxationFactor;
  scalar fsiRelaxationFactorMin;
  scalar outerCorrTolerance;
  Switch fsi;

public:

  fsifoam_module() : argsPtr(NULL), runTimePtr(NULL), transportPropertiesPtr(NULL),
                     nuPtr(NULL), rhoFluidPtr(NULL), pPtr(NULL), UPtr(NULL), phiPtr(NULL),
                     stressMeshPtr(NULL), DUPtr(NULL), gradDUPtr(NULL), UsolidPtr(NULL), DVPtr(NULL),
                     VsPtr(NULL), sigmaPtr(NULL), DSigmaPtr(NULL), rheologyPtr(NULL), 
                     rhoPtr(NULL), muPtr(NULL), lambdaPtr(NULL), FPtr(NULL), DFPtr(NULL),
                     JPtr(NULL), solidDdtSchemePtr(NULL), pStressMeshPtr(NULL),pointDUPtr(NULL),
                     couplingPropertiesPtr(NULL),
                     accumulatedFluidInterfaceDisplacementHeaderPtr(NULL),
                     accumulatedFluidInterfaceDisplacementPtr(NULL),
                     interpolatorFluidSolidPtr(NULL), interpolatorSolidFluidPtr(NULL),
                     patchPointInterpolatorPtr(NULL), 
                     dynamicMeshDictPtr(NULL), numPointsSurface(0), numElementsSurface(0),
                     srfWin("sOF"), volWin("vOF")
  {};

  fsifoam_module(int argc,char *argv[]) : argsPtr(NULL), runTimePtr(NULL),
                                          transportPropertiesPtr(NULL),
                                          nuPtr(NULL), rhoFluidPtr(NULL), pPtr(NULL), 
                                          UPtr(NULL), phiPtr(NULL), stressMeshPtr(NULL), 
                                          DUPtr(NULL), gradDUPtr(NULL), UsolidPtr(NULL), 
                                          DVPtr(NULL), VsPtr(NULL), sigmaPtr(NULL), 
                                          DSigmaPtr(NULL), rheologyPtr(NULL), rhoPtr(NULL), 
                                          muPtr(NULL), lambdaPtr(NULL), FPtr(NULL), DFPtr(NULL),
                                          JPtr(NULL), solidDdtSchemePtr(NULL), 
                                          pStressMeshPtr(NULL),pointDUPtr(NULL),
                                          couplingPropertiesPtr(NULL),
                                          accumulatedFluidInterfaceDisplacementHeaderPtr(NULL),
                                          accumulatedFluidInterfaceDisplacementPtr(NULL),
                                          interpolatorFluidSolidPtr(NULL), interpolatorSolidFluidPtr(NULL),
                                          patchPointInterpolatorPtr(NULL), 
                                          dynamicMeshDictPtr(NULL), numPointsSurface(0),
                                          numElementsSurface(0)
  {Initialize(argc,argv);};
  
  int Initialize(int argc,char *argv[]);
  int InitFluidMesh();
  int InitTransportProperties();
  int CreateFluidFields();
  int InitStructuresMesh();
  int CreateStructuresFields();
  int ReadCouplingProperties();
  int CreateInterZoneInterpolators();
  int FindGlobalFaceZones();
  int ReadPISOControls();
  int ReadFSIControls();
  int Step();
  int StepFluidAlone();
  int StepFluidNonItr();
  int StepFluidItr();
  int Dump();
  int Rank(){ return(rank);} ;
  int Size(){ return(nproc);} ;


  /// function to register through IMPACT
  // ElmerFoamFSI API
  // void InitFoam(int *pargc, void **pargv, int *verbIn);
  // Rocstar API
  void InitFoam(double* time_rocstar, MPI_Comm* rocstar_comm, int* rocstar_ic_handle,
                 std::string surf_in_win, std::string vol_in_win, int* rocstar_obtain_attr_handle);
  void InitFoamOFPrep(int *pargc, void **pargv, int *verbIn);
  void RunFoam();
  void StepFoam();
  void StepFluid();
  void StepFluidRocstarMod(double* time_rocstar, double* dt_rocstar, 
                           int* rocstar_bc_handle, int* rocstar_gm_handle);
  void FinalizeFoam(){};
  //void ModifyEndTime(const double &endTime);
  static void Load(const std::string &name);
  static void Unload(const std::string &name);

  Foam::Time &RunTime(){return(*runTimePtr);};
  Foam::argList &ArgList(){return(*argsPtr);};
  //dynamicFvMesh &Mesh(){return(meshPtr());};
  dynamicFvMesh &FluidMesh(){return(meshPtr());};
  IOdictionary &TransportProperties(){return(*transportPropertiesPtr);};
  dimensionedScalar &nu(){return(*nuPtr);};
  dimensionedScalar &rhoFluid(){return(*rhoFluidPtr);};
  volScalarField &p(){return(*pPtr);};
  volVectorField &U(){return(*UPtr);};
  surfaceScalarField &phi(){return(*phiPtr);};
  fvMesh &StructuresMesh(){return(*stressMeshPtr);};
  volVectorField &DU(){return(*DUPtr);};
  volTensorField &gradDU(){return(*gradDUPtr);};
  volVectorField &Usolid(){return(*UsolidPtr);};
  volVectorField &DV(){return(*DVPtr);};
  volVectorField &Vs(){return(*VsPtr);};
  volSymmTensorField &sigma(){return(*sigmaPtr);};
  volSymmTensorField &DSigma(){return(*DSigmaPtr);};
  constitutiveModel &Rheology(){return(*rheologyPtr);};
  volScalarField &rhoSolid(){return(*rhoPtr);};
  volScalarField &mu(){return(*muPtr);};
  volScalarField &lambda(){return(*lambdaPtr);};
  volTensorField &F(){return(*FPtr);};
  volTensorField &DF(){return(*DFPtr);};
  volScalarField &J(){return(*JPtr);};
  word &SolidDdtScheme(){return(*solidDdtSchemePtr);};
  pointMesh &pStressMesh(){return(*pStressMeshPtr);};
  pointVectorField &PointDU(){return(*pointDUPtr);};
  IOdictionary &CouplingProperties(){return(*couplingPropertiesPtr);};
  label SolidPatchID(){return(solidPatchID);};
  word  SolidPatchName(){return(solidPatchName);};
  label FluidPatchID(){return(fluidPatchID);};
  word FluidPatchName(){return(fluidPatchName);};
  word FluidZoneName(){return(fluidZoneName);};
  word SolidZoneName(){return(solidZoneName);};
  label FluidZoneID(){return(fluidZoneID);};
  label SolidZoneID(){return(solidZoneID);};
  bool FEMotion(){return(feMotionSolver);};
  bool FVMotion(){return(fvMotionSolver);};
  vectorField &AccumulatedFluidInterfaceDisplacements(){
    return(*accumulatedFluidInterfaceDisplacementPtr);};
  zoneToZoneInterpolation &interpFluidSolid(){return(*interpolatorFluidSolidPtr);};
  zoneToZoneInterpolation &interpSolidFluid(){return(*interpolatorSolidFluidPtr);};
  solidTractionFvPatchVectorField &tForce(){
    return(refCast<solidTractionFvPatchVectorField>(DUPtr->boundaryField()[solidPatchID]));};

  scalar &LocalContErr(){return(sumLocalContErr);};
  scalar &GlobalContErr(){return(globalContErr);};
  scalar &CumulativeContErr(){return(cumulativeContErr);};
  labelList &GlobalFaceZones(){return(globalFaceZones);};
  labelListList &GlobalToLocalFaceZonePointMap(){return(globalToLocalFaceZonePointMap);};
  word CouplingScheme(){return(couplingScheme);};
  //  label &OuterCorr(){return(outerCorr);};
  scalar &FSIRelaxationFactor(){return(fsiRelaxationFactor);};
  scalar &FSIRelaxationFactorMin(){return(fsiRelaxationFactorMin);};
  Switch FSIEnabled(){return(fsi);};
  int &NCorrPISO(){return(nCorr);};
  int &NNonOrthCorr(){return(nNonOrthCorr);};
  scalar &InterfaceDeformationLimit(){return(interfaceDeformationLimit);};
  scalar &OuterCorrTolerance(){return(outerCorrTolerance);};
  int &NOuterCorr(){return(nOuterCorr);};

  protected:
  // ===== Functions specific to IMPACT FSI =====
  /// 
  /// Initialization of the surface mesh data structures from 
  /// the OpenFOAM mesh data structures
  ///
  /// @param none
  /// @returns void
  /// @test None
  ///
  void CreateFSISurfaceMesh();

  /// 
  /// Initialization of the fluids volume mesh data structures from 
  /// the OpenFOAM mesh data structures
  ///
  /// @param none
  /// @returns void
  /// @test None
  ///
  void CreateFluidsVolMesh();

  /// 
  /// Update of the surface mesh data coordinates
  ///
  /// @param none
  /// @returns void
  /// @test None
  ///
  void UpdateFSISurfaceMesh();

  /// 
  /// Update the nodal coordinates of the IMAPCT and OpenFoam FSI surfaces
  /// from the IMPACT displacement data
  ///
  /// @param displacements array to populate
  /// @returns void
  /// @test None
  ///
  int UpdateFSISurface(Foam::vectorField &solidPatchPointsDispl);

  /// 
  /// Update the data registered on the FSI surface.
  ///
  /// @param none
  /// @returns void
  /// @test None
  ///
  void UpdateFSISurfaceData();

  /// 
  /// Update the time control data registered with OpenFOAM
  /// i.e. runtime, endtime, dt, etc.
  ///
  /// @param none
  /// @returns void
  /// @test None
  ///
  void UpdateTime();
};

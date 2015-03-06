/**
 *  @brief This utility converts a set of HDF files to a single
 *  multi-zone TECPLOT file to visualize within tecplot.
 *  @note Based on the initial hdf2plt base-code.
 *  @file hdf2pltV2.C
 *  @date Aug 3, 2009
 *  @author George Zagaris (gzagaris@illinois.edu)
 */

/* Global Variables */
#include <string>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <sstream>
#include <cstring>
#include <cstdlib>
#include <cassert>
#include <vector>
#include <map>
#include <utility>
#include <algorithm>

#include "TAIL.H"
#include "PrimitiveTypes.H"
#include "roccom.h"

#include "PaneConnectivity.hpp"					/* Gives high-level dynamic pane connectivity access */
#include "MeshPointIdToGlobalIdMap.hpp" /* Data-structure for mapping (mesh,point) pairs to assigned global IDs */

/* External Dynamically Linked Modules */
COM_EXTERN_MODULE( Rocin );		// Used to read in the HDF files.
COM_EXTERN_MODULE( Rocmap );	// Used to compute the pane connectivity.

using namespace Rocstar::Rocin::Utilities;

/**
 * @brief The data item type.
 */
enum DataItemType { NODEDATA, CELLDATA };

/**
 * @brief A struct that holds all the global program data.
 * @struct Program
 */
struct
{
	/* These are the program parameters */
	bool readControlFile;																								/*!< @brief Data is read from the control file    											*/
	bool withGhost;																											/*!< @brief Ghost nodes will be used 					    											*/
	bool blocks;																												/*!< @brief Input data is block-structured grid   											*/
	bool vtk;																														/*!< @brief Writes vtk files 																						*/
	std::string cntrlfile;																							/*!< @brief Control file string 								  											*/
	std::string fileregx;																								/*!< @brief File regular expression 						  											*/
	std::string outputfile;																							/*!< @brief Output file to write the file 			  				  						*/

	/* HDF2Plot Persistent Data */
	std::string prgmname;																								/*!< @brief The program name 					 						 									 				 */
	int numberOfPanes;																									/*!< @brief The number of panes, aka number of sub-domains 					 				 */
	std::vector< int > paneIds;																					/*!< @brief Array of paneIds 																				 				 */
	std::vector< Mesh::UnstructuredMesh > 							 meshes;				/*!< @brief The list of grids to write  					 										  		 */
	std::vector< std::vector< bool > >							 		 mask;					/*!< @brief Masks the points that are shared (true) share (false) not shared */
	std::vector< SolnMetaData >					  							 smdv;					/*!< @brief The solution Metadata			 						 				 					 				 */
	std::vector< std::vector< std::vector< double > > >  solution;			/*!< @brief The solution data 					 					 									 				 */
	std::vector< PaneConnectivity > 										 pconns;				/*!< @brief Inter-zone (pane) connectivity information 											 */
	std::map< int, int >															paneIds2index;		/*!< @brief Maps the pane ids to the index in the corresponding vectors 		 */



	std::vector< std::vector< double > > globalSolution;							  /*!< @brief Contains the global solution */
	std::vector< bool >									 solutionignore;								/*!< @brief Masks the data to be ignored */
	std::vector< double >								 globalNodeList;								/*!< @brief The global node list 				 */
	std::vector< std::vector< int > >		 globalElementList;							/*!< @brief The global element list 		 */


	/**
	 * @brief This method will return the TecplotZone Type.
	 * @return S a string corresponding to the Tecplot Zone type.
	 * @pre globalElementList.size( ) > 0.
	 * @post S != ""
	 */
	inline std::string getTecplotZoneType( )
	{
		#ifdef ASSERT_ON
			assert( globalElementList.size( ) > 0 );
		#endif

		std::string retString = "";

		int maxElementSize = 0;
		for( int i=0; i < globalElementList.size( ); ++i )
		{
			if( globalElementList[ i ].size( ) > maxElementSize )
				maxElementSize = globalElementList[ i ].size( );
		}

		switch( maxElementSize )
		{
			case 8:
				retString = "FEBRICK";
				break;
			case 4:
				retString = "FETETRAHEDRON";
				break;
			case 5:
			case 6:
				retString = "FEPOLYHEDRAL";
				break;
			default:
				std::cerr << "Undefined element size: " << maxElementSize << std::endl;
				std::cerr << "File: " << __FILE__ << std::endl;
				std::cerr << "Line: " << __LINE__ << std::endl;
				assert( false );
		}
		return retString;
	}

	/**
	 * @brief Returns the data item type at the given meta data index.
	 * @param metaDataIdx the meta data index.
	 * @return T the type: {NODEDATA, CELLDATA}.
	 * @pre metaDataIdx >= 0 && metaDataIdx < Program.smdv.size( ).
	 * @post T == NODEDATA || T == CELLDATA
	 */
	inline DataItemType getDataItemType( const int metaDataIdx )
	{
		#ifdef ASSERT_ON
			assert( metaDataIdx >= 0 && metaDataIdx < smdv.size( ) );
		#endif

		if( smdv[ metaDataIdx ].loc == 'e' )
			return CELLDATA;
		else if( smdv[ metaDataIdx ].loc == 'n' )
			return NODEDATA;
		else {
			std::cerr << "Undefined location: " << smdv[ metaDataIdx ].loc << std::endl;
			std::cerr << "File: " << __FILE__ << std::endl;
			std::cerr << "Line: " << __LINE__ << std::endl;
			assert( false );
		}
	}

	/**
	 * @brief Checks if the node associated with the given mesh index and point index is shared.
	 * @param meshidx the mesh index of the node in query.
	 * @param pointidx the point index of the node in query.
	 * @return Status true if shared else false.
	 * @pre meshidx >= 0 && meshidx < numberOfPanes
	 * @pre pointidx >= 0 && pointidx < meshes[ meshidx ].nc.Size( )
	 */
	inline bool isNodeShared( const int meshidx, const int pointidx )
	{
		#ifdef ASSERT_ON
			assert( meshidx >= 0 && meshidx < numberOfPanes );
			assert( pointidx >= 0 && pointidx < meshes[ meshidx ].nc.Size( ) );
		#endif
		return mask[ meshidx ][ pointidx ];
	}

	/**
	 * @brief Returns the pane index associated with the given paneId.
	 * @param paneId the pane id of the pane whose index is queried.
	 * @return idx the corresponding index to the pane.
	 * @note idx is used to access pane data stored in arrays.
	 * @pre Program.hasPane( paneId ) == true.
	 * @post idx >= 0 && idx < Program.numberOfPanes.
	 */
	inline int getPaneIndex( const int paneId )
	{
		#ifdef ASSERT_ON
			assert( hasPane( paneId ) );
		#endif

		int idx = paneIds2index[ paneId ];

		#ifdef ASSERT_ON
			assert( idx >= 0 && idx < numberOfPanes );
		#endif

		return idx;
	}

	/**
	 * @brief Checks if pane exists.
	 * @param paneId the pane id to check.
	 * @return Status true if the pane exists, else false.
	 */
	inline bool hasPane( const int paneId )
	{
		if( paneIds2index.find( paneId ) != paneIds2index.end( ) )
			return true;
		else
			return false;
	}

	/**
	 * @brief Returns the pane connectivity information of the mesh at the given index.
	 * @param meshIndex the mesh index.
	 * @return paneConnPtr pointer to the associated PaneConnectivity object.
	 * @see Rocstar::Rocin::Utilities::PaneConnectivity
	 */
	inline PaneConnectivity *getPaneConnectivity( const int meshIndex )
	{
		#ifdef ASSERT_ON
			assert( meshIndex >= 0 && meshIndex < meshes.size( ) );
			assert( meshes.size( ) == pconns.size( ) );
		#endif
		return( &pconns[ meshIndex ] );
	}

	/**
	 * @brief Loads all required modules.
	 */
	inline void loadModules( )
	{
		COM_LOAD_MODULE_STATIC_DYNAMIC( Rocin,  "IN" 	);
		COM_LOAD_MODULE_STATIC_DYNAMIC( Rocmap, "MAP" );
	}

	/**
	 * @brief Unloads all required modules.
	 */
	inline void unloadModules( )
	{
		COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocin,  "IN" 	);
		COM_UNLOAD_MODULE_STATIC_DYNAMIC( Rocmap, "MAP" );
	}

	/**
	 * @brief Prints the current program configuration
	 * to the standard output.
	 */
	inline void printInfo( )
	{

		std::cout << "Program: " << prgmname << std::endl;
		std::cout << "Date: " << __DATE__ << std::endl;
		if( readControlFile )
		{
			std::cout << "Read control file: yes\n";
			std::cout << "Control file: " << cntrlfile << std::endl;
		}
		else
		{
			std::cout << "Read control file: no\n";
			std::cout << "File(s): " << fileregx << std::endl;
		}

		std::cout << "Use ghost nodes: ";
		(withGhost)? std::cout << "yes\n" : std::cout << "no\n";

		if( outputfile == "" )
			std::cout << "Output file: standard out\n";
		else
			std::cout << "Output file: " << outputfile << std::endl;
	}

} Program;

/******************************************** Function Prototypes ************************************************************************************/

/**
 * @brief Displays usage information to Standard-out about the
 * different switches and command line arguments that can be
 * used with this program.
 */
void showUsage( );

/**
 * @brief Parses the command line arguments.
 * @param argc the argument counter.
 * @param argv the argument vector.
 * @pre argc >= 1.
 * @pre argv != NULL.
 */
void parseArguments( int argc, char **argv );

/**
 * @brief Loads the set of HDF files into a single window.
 * Each sub-domain, or block, or zone is stored on a separate
 * pane of the window. Moreover, the data layout is changed
 * to contiguous from the default stacked layout of HDF files.
 * @return WindowName the name of the window.
 */
std::string getWindow( );

/**
 * @brief Generates a single zone plot file from the stitched mesh.
 * @pre Program.globalNodeList.size( ) 	  > 0.
 * @pre Program.globalElementList.size( ) > 0.
 * @pre Program.smdv.size( ) > 0.
 * @pre Program.globalSolution.size( ) == Program.smdv.size( ).
 * @pre Program.solutionignore.size( ) == Program.smdv.size( ).
 * @pre Program.outputfile != ""
 * @post A VTK formatted file is written in Program.outputfile.vtk
 * @note The extension in the outputfile will be appended.
 */
void writePlotFile( );

/**
 * @brief Prints the window with the given name.
 * @param windowName the name of the window.
 */
void printWindow( const std::string &windowName );

/**
 * @brief Converts the window from block-structured format to unstructured format.
 * This function is called iff -block-structured is supplied to the command line.
 * @param wname the name of the window.
 * @return newWindowName the name of the window that contains the grids in an unstructured format.
 */
std::string convertWindow( const std::string &wname );

/**
 * @brief Writes the set of meshes into separate VTK files that can be
 * visualized with Paraview. The format of the files is {outputFile}.paneId.vtk.
 * @note This method is mainly used for debugging.
 * @param m the set of meshes.
 */
void writeVtkFiles( std::vector< Mesh::UnstructuredMesh > &m );

/**
 * @brief This method calls Rocmap to query the pane connectivity information for
 * the given window. A PaneConnectivity object is constructed for each mesh and the
 * shared nodes are flaged for each mesh.
 * @param wname the window name.
 * @see Rocstar::Rocin::Utilities::PaneConnectivity.hpp
 */
void getPaneConnnectivity( const std::string &wname );

/**
 * @brief This method is called from getPaneConnectivity() to mask
 * the flag the mesh points that are shared on each grid.
 * @pre Program.mask.size( ) == 0.
 * @pre Program.numberOfPanes >= 1.
 * @pre Program.pconns.size( ) == Program.numberOfPanes.
 * @pre Program.paneIds.size( ) == Program.numberOfPanes.
 * @post Program.mask.size( ) == Program.numberOfPanes.
 * @post Program.mask[ i ] is created for each mesh, m_i, 0 <= i <= Program.numberOfPanes.
 */
void maskPoints( );

/**
 * @brief This method will stitch the grids into a single, global grid.
 * @pre Program.meshes.size( ) == Program.numberOfPanes
 * @pre Program.pconns.size( ) == Program.numberOfPanes
 * @post Program.globalMesh is constructed.
 */
void stitchGrids( );

/**
 * @brief This method will stitch the partitioned solution for the single grid.
 * @param hash a hash of the local mesh points to the corresponding global Id.
 * @pre hash != NULL
 */
void stitchSolutionData( MeshPointIdToGlobalIdMap *hash );

/**
 * @brief This method adds the vertex with the given x,y,z coordinates to the global node list and returns the assigned global id (index).
 * @param x	the x--coordinate of the vector being added.
 * @param y the y--coordinate of the vector being added.
 * @param z the z--coordinate of the vector being added.
 * @param nodelist the global node list where the new node will be inserted.
 * @return GlobalID the global id of the vertex added.
 * @post nodelist.size( ) is incremented by 3.
 * @post GlobalID >= 0.
 */
int addToGlobalNodeList( const double x, const double y, const double z, std::vector< double > &nodelist );

/**
 * @brief This method updates a MeshPointIdToGlobalIdMap hash with the mesh panes and point IDs mapping to the global ID.
 * @param meshPaneId the pane id of the mesh that the local point id belongs to.
 * @param pointId the point id of the point assigned a new global ID.
 * @param globalId the global Id to be assigned to the given point.
 * @param hash pointer to the has data-structure to be updated.
 * @pre Program.hasPane(meshPaneId ) == true.
 * @pre pointId >= 0 && pointId < Program.meshes[ Program.getPaneIndex( meshPaneId ) ].nc.Size( )
 * @pre globalId >= 0.
 * @pre hash != NULL.
 */
void updateHash( const int meshPaneId, const int pointId, const int globalId, MeshPointIdToGlobalIdMap *hash );

/**
 * @brief Writes the merged grid and its data in VTK UNSTRUCTURED_GRID file format.
 * @pre Program.globalNodeList.size( ) 	  > 0.
 * @pre Program.globalElementList.size( ) > 0.
 * @pre Program.smdv.size( ) > 0.
 * @pre Program.globalSolution.size( ) == Program.smdv.size( ).
 * @pre Program.solutionignore.size( ) == Program.smdv.size( ).
 * @pre Program.outputfile != ""
 * @post A VTK formatted file is written in Program.outputfile.vtk
 * @note The extension in the outputfile will be appended.
 */
void writeVtkData( );

/**
 * @brief This method prints the global grid in VTK file format.
 * @note Mostly used for debugging.
 * @param vlist the global vertex list.
 * @param elist the element list.
 */
void printVtk( std::vector< double > &vlist, std::vector< std::vector< int > > &elist );

/******************************************** End Function Prototypes ************************************************************************************/

/**
 * @brief Program main.
 * @param argc the argument counter.
 * @param argv the argument vector.
 * @return 0 if success, else, false.
 */
int main( int argc, char **argv )
{
	/* Parse the user-supplied command line */
	parseArguments( argc, argv );

	/* Print program information to STDOUT */
	Program.printInfo( );

	/* Initialize Roccom Environment */
	COM_init( &argc, &argv );

	/* Load all required modules */
	Program.loadModules( );

  /* Get window */
	std::string windowname = getWindow( );

	std::cout << "Read window: " << windowname << std::endl;
	std::cout << "Number of panes: " << Program.numberOfPanes << std::endl;

	/* Print window */
	printWindow( windowname );

	/* Unload modules */
	Program.unloadModules( );

	/* Finalize the COM Environment */
	COM_finalize( );

	return 0;
}

//----------------------------------------------------------------------------------------------------------------------------------------------------------
//																		F U N C T I O N    P R O T O T Y P E    I M P L E M E N T A T I O N
//----------------------------------------------------------------------------------------------------------------------------------------------------------

/**
 * @brief Generates a single zone plot file from the merged mesh.
 * @pre Program.globalNodeList.size( ) 	  > 0.
 * @pre Program.globalElementList.size( ) > 0.
 * @pre Program.smdv.size( ) > 0.
 * @pre Program.globalSolution.size( ) == Program.smdv.size( ).
 * @pre Program.solutionignore.size( ) == Program.smdv.size( ).
 * @pre Program.outputfile != ""
 * @post A Tecplot formatted file is written in Program.outputfile.plt
 * @note The extension in the outputfile will be appended.
 */
void writePlotFile( )
{
	std::cout << "Writing TECPLOT file...";
	std::cout.flush( );

	#ifdef ASSERT_ON
		assert( Program.globalNodeList.size( ) >  0 	);
		assert( Program.globalElementList.size( ) > 0 );
		assert( Program.smdv.size( ) > 0 );
		assert( Program.globalSolution.size( ) == Program.smdv.size( ) );
		assert( Program.solutionignore.size( ) == Program.smdv.size( ) );
		assert( Program.outputfile != "" );
	#endif

		std::ofstream ofs;
		ofs.open( std::string(Program.outputfile + ".dat").c_str( ) );

		if( !ofs.is_open( ) )
		{
			std::cerr << "Cannot write TECPLOT file: " << Program.outputfile+".vtk" << std::endl;
			std::cerr << "File: " << __FILE__ 		 << std::endl;
			std::cerr << "Line: " << __LINE__ 		 << std::endl;
			assert( false );
		}

		/* Write title */
		ofs << "TITLE=\"" << Program.outputfile << " " << __DATE__ << "\"" << std::endl;

		/* Write Variables */
		int variableCount = 3;
		std::vector< int > ecenteredidx;

		ofs << "VARIABLES= \"X\", \"Y\", \"Z\"";

		for( int i=0; i < Program.smdv.size( ); ++i )
		{
			if( !Program.solutionignore[ i ] )
			{

				if( Program.smdv[ i ].ncomp > 1 )
				{
					 for( int component=0; component < Program.smdv[ i ].ncomp; ++component )
					 {
						 ofs << ", \"";
						 ofs << component+1 << "-" << Program.smdv[ i ].name;
						 ofs << "\"";
						 ++variableCount;

						 if( Program.getDataItemType( i ) == CELLDATA )
							 ecenteredidx.push_back( variableCount );
					 }
				}
				else
				{
					ofs << ", \"" << Program.smdv[ i ].name << "\"";
					++variableCount;
					if( Program.getDataItemType( i ) == CELLDATA )
						ecenteredidx.push_back( variableCount );
				}

			} /* End if solution is not ignored */

		} /* End for all variables */

		/* Write Zone header */
		ofs << std::endl;
		ofs << "ZONE " << "NODES=" << Program.globalNodeList.size( )/3 << " ";
		ofs << "ELEMENTS=" << Program.globalElementList.size( ) << " ";
		ofs << "ZONETYPE=" << Program.getTecplotZoneType( ) << " ";
		ofs << "DATAPACKING=BLOCK ";

		if( !ecenteredidx.empty( ) )
		{
			ofs << "VARLOCATION=([";
			ofs << ecenteredidx[ 0 ];
			for( int i=1; i < ecenteredidx.size( ); ++i )
				ofs << "," << ecenteredidx[ i ];
			ofs << "]=CELLCENTERED)";
		}
		ofs << std::endl;

		/* Write nodes */

		for( int i=0; i < 3; ++i )
		{
			switch( i )
			{
				case 0:
					ofs << "# Begin X\n";
					break;
				case 1:
					ofs << "# Begin Y\n";
					break;
				case 2:
					ofs << "# Begin Z\n";
					break;
				default:
					std::cerr << "Error: Code should not reach here!\n";
					std::cerr << "File: " << __FILE__ << std::endl;
					std::cerr << "Line: " << __LINE__ << std::endl;
					std::cerr << "core dumping...\n";
					assert( false );
			}

			for( int node=0; node < Program.globalNodeList.size( )/3; ++node )
			{
				ofs << Program.globalNodeList[ node*3 + i] << " ";
				if( node % 10 == 9 )
					ofs << std::endl;
			}
			ofs << std::endl;

		}/* End for all dimensions */

		/* End Write nodes */


		/* Write the solution */
		for( int i=0; i < Program.globalSolution.size( ); ++i )
		{
			if( ! Program.solutionignore[ i ] )
			{
				if( Program.smdv[ i ].ncomp > 1 )
				{
					for( int component=0; component < Program.smdv[ i ].ncomp; ++component )
					{
						ofs << "\n# Begin " << component+1 << "-" << Program.smdv[ i ].name << std::endl;

						int stride   = Program.smdv[ i ].ncomp;
						int NumItems = Program.globalSolution[ i ].size( )/stride;
						for( int dataidx=0; dataidx < NumItems; ++dataidx )
						{
							ofs << Program.globalSolution[ i ][ dataidx*stride+component ] << " ";
							if( dataidx % 10 == 9 )
								ofs << std::endl;
						}

					}
				}
				else
				{
					ofs << "\n# Begin " << Program.smdv[ i ].name << std::endl;
					for( int dataidx=0; dataidx < Program.globalSolution[ i ].size( ); ++dataidx )
					{
						ofs << Program.globalSolution[ i ][ dataidx ] << " ";
						if( dataidx % 10 == 9 )
							ofs << std::endl;
					}

				}

				ofs << std::endl;

			} /* End if solution is not ignored */

		}
		/* End write solution */

		/* Write the element connectivity */
		ofs << std::endl << "# Begin Element Connectivity\n";
		for( int i=0; i < Program.globalElementList.size( ); ++i )
		{
			for( int j=0; j < Program.globalElementList[ i ].size( ); ++j )
			{
				ofs << Program.globalElementList[ i ][ j ]+1 << " ";
			}
			ofs << std::endl;
		}
		/* End write the element connectivity */

		ofs.close( );

		std::cout << "[DONE]\n";
}

/**
 * @brief Writes the merged grid and its data in VTK UNSTRUCTURED_GRID file format.
 * @pre Program.globalNodeList.size( ) 	  > 0.
 * @pre Program.globalElementList.size( ) > 0.
 * @pre Program.smdv.size( ) > 0.
 * @pre Program.globalSolution.size( ) == Program.smdv.size( ).
 * @pre Program.solutionignore.size( ) == Program.smdv.size( ).
 * @pre Program.outputfile != ""
 * @post A VTK formatted file is written in Program.outputfile.vtk
 * @note The extension in the outputfile will be appended.
 */
void writeVtkData( )
{

	std::cout << "Writing VTK data...";
	std::cout.flush( );

	#ifdef ASSERT_ON
		assert( Program.globalNodeList.size( ) >  0 	);
		assert( Program.globalElementList.size( ) > 0 );
		assert( Program.smdv.size( ) > 0 );
		assert( Program.globalSolution.size( ) == Program.smdv.size( ) );
		assert( Program.solutionignore.size( ) == Program.smdv.size( ) );
		assert( Program.outputfile != "" );
	#endif

	std::ofstream ofs;
	ofs.open( std::string(Program.outputfile + ".vtk").c_str( ) );

	if( !ofs.is_open( ) )
	{
		std::cerr << "Cannot write VTK file: " << Program.outputfile+".vtk" << std::endl;
		std::cerr << "File: " << __FILE__ 		 << std::endl;
		std::cerr << "Line: " << __LINE__ 		 << std::endl;
		assert( false );
	}

	/* STEP 1: Write the header */
	ofs << "# vtk DataFile Version 3.0\n";
	ofs << "Global Mesh Output" << std::endl;
	ofs << "ASCII\n";
	ofs << "DATASET UNSTRUCTURED_GRID\n";
	ofs << "POINTS " << Program.globalNodeList.size( )/3 << " double\n";

	/* STEP 2: Write the nodes */
	for( int i=0; i < Program.globalNodeList.size( )/3; ++i )
	{
		ofs << Program.globalNodeList[ i*3 	 ] << " ";
		ofs << Program.globalNodeList[ i*3+1 ] << " ";
		ofs << Program.globalNodeList[ i*3+2 ] << std::endl;
	}

	/* STEP 3: Write the max element size */
	int maxnodes = 0; /* the maximum number of nodes per element */
	for( int i=0; i < Program.globalElementList.size( ); ++i )
	{
		if( maxnodes < Program.globalElementList[ i ].size( ) )
			maxnodes = Program.globalElementList[ i ].size( );
	}

	/* STEP 4: Write the element connectivity */
	ofs << "CELLS " << Program.globalElementList.size( ) << " " << Program.globalElementList.size( )*( maxnodes+1) << "\n";
	for( int i=0; i < Program.globalElementList.size( ); ++i  )
	{
		ofs <<  Program.globalElementList[ i ].size( ) << " ";
		for( int j=0; j < Program.globalElementList[ i ].size( ); ++j )
			ofs <<  Program.globalElementList[ i ][ j ]<< " ";
		ofs << std::endl;
	}

	/* STEP 5: Write the cell types */
	ofs << "CELL_TYPES " << Program.globalElementList.size( ) << std::endl;
	for( int i=0; i < Program.globalElementList.size( ); ++i )
	{

		if( Program.globalElementList[ i ].size( ) == 8 )
			ofs << "12\n";
		else if( Program.globalElementList[ i ].size( ) == 4 )
			ofs << "10\n";
		else {
			std::cerr << "Undefined element type!\n";
			std::cerr << "File: " << __FILE__ << std::endl;
			std::cerr << "Line: " << __LINE__ << std::endl;
		}

	}

	/* STEP 6: Write the solution data attached to the points */
	bool wroteHeader = false;
	for( int i=0; i < Program.globalSolution.size( ); ++i )
	{

		if( ! Program.solutionignore[ i ] && Program.getDataItemType( i ) == NODEDATA  )
		{

			int N = Program.globalNodeList.size( )/3;
			int NC      = Program.smdv[ i ].ncomp;

			if(!wroteHeader)
			{
				ofs << "POINT_DATA " << N << std::endl;
				wroteHeader = true;
			}

			if( NC == 1 )
			{
					ofs << "SCALARS " << Program.smdv[ i ].name << " double" << std::endl;
					ofs << "LOOKUP_TABLE default\n";
			}
			else if( NC == 3 )
			{
				ofs << "VECTORS " << Program.smdv[ i ].name << " double" << std::endl;
			}
			else
			{
				ofs << "TENSORS " << Program.smdv[ i ].name << " double" << std::endl;
			}

			for( int j=0; j < N; ++j )
			{
				for( int component=0; component < NC; ++component )
				{
					ofs << Program.globalSolution[ i ][ j*NC+component ] << " ";
				}
				ofs << std::endl;
			}

		} /* End if the solution is not ignored */

	} /* End for all solution data */


	/* STEP 6: Write the solution data attached to the cells */
	wroteHeader = false;
	for( int i=0; i < Program.globalSolution.size( ); ++i )
	{

		if( ! Program.solutionignore[ i ] && Program.getDataItemType( i ) == CELLDATA )
		{

			int N 			= Program.globalElementList.size();
			int NC      = Program.smdv[ i ].ncomp;

			if( !wroteHeader )
			{
				ofs << "CELL_DATA " << N << std::endl;
				wroteHeader = true;
			}

			if( NC == 1 )
			{
					ofs << "SCALARS " << Program.smdv[ i ].name << " double" << std::endl;
					ofs << "LOOKUP_TABLE default\n";
			}
			else if( NC == 3 )
			{
				ofs << "VECTORS " << Program.smdv[ i ].name << " double" << std::endl;
			}
			else
			{
				ofs << "TENSORS " << Program.smdv[ i ].name << " double" << std::endl;
			}

			for( int j=0; j < N; ++j )
			{
				for( int component=0; component < NC; ++component )
				{
					ofs << Program.globalSolution[ i ][ j*NC+component ] << " ";
				}
				ofs << std::endl;
			}

		} /* End if the solution is not ignored */

	} /* End for all solution data */

	ofs.close( );

	std::cout << "[DONE]\n";

}

/**
 * @brief This method prints the global grid in VTK file format.
 * @note Mostly used for debugging.
 * @param vlist the global vertex list.
 * @param elist the element list.
 */
void printVtk( std::vector< double > &vlist, std::vector< std::vector< int > > &elist )
{
	 std::ofstream ofs;
	 ofs.open( std::string( "global_mesh.vtk" ).c_str( ) );

	 if( !ofs.is_open( ) )
		{
			std::cerr << "Cannot write global VTK file! " << std::endl;
			std::cerr << "File: " << __FILE__ 						<< std::endl;
			std::cerr << "Line: " << __LINE__ 						<< std::endl;
			assert( false );
		}

		ofs << "# vtk DataFile Version 3.0\n";
		ofs << "Global Mesh Output" << std::endl;
		ofs << "ASCII\n";
		ofs << "DATASET UNSTRUCTURED_GRID\n";
		ofs << "POINTS " << vlist.size( )/3 << " double\n";

		for( int i=0; i < vlist.size( )/3; ++i )
		{
			ofs << vlist[ i*3   ] << " ";
			ofs << vlist[ i*3+1 ] << " ";
			ofs << vlist[ i*3+2 ] << "\n";
		}

		int maxnodes = 0; /* the maximum number of nodes per element */
		for( int i=0; i < elist.size( ); ++i )
		{
			if( maxnodes < elist[ i ].size( ) )
				maxnodes = elist[ i ].size( );
		}

		ofs << "CELLS " << elist.size( ) << " " << elist.size( )*( maxnodes+1) << "\n";
		for( int i=0; i < elist.size( ); ++i  )
		{
			ofs <<  elist[ i ].size( ) << " ";
			for( int j=0; j < elist[ i ].size( ); ++j )
				ofs <<  elist[ i ][ j ]<< " ";
			ofs << std::endl;
		}

		ofs << "CELL_TYPES " << elist.size( ) << std::endl;
		for( int i=0; i < elist.size( ); ++i )
		{

			if( elist[ i ].size( ) == 8 )
				ofs << "12\n";
			else if( elist[ i ].size( ) == 4 )
				ofs << "10\n";
			else {
				std::cerr << "Undefined element type!\n";
				std::cerr << "File: " << __FILE__ << std::endl;
				std::cerr << "Line: " << __LINE__ << std::endl;
			}

		}
		ofs.close( );
}


/**
 * @brief This method updates a MeshPointIdToGlobalIdMap hash with the mesh panes and point IDs mapping to the global ID.
 * @param meshPaneId the pane id of the mesh that the local point id belongs to.
 * @param pointId the point id of the point assigned a new global ID.
 * @param globalId the global Id to be assigned to the given point.
 * @param hash pointer to the has data-structure to be updated.
 * @pre Program.hasPane(meshPaneId ) == true.
 * @pre pointId >= 0 && pointId < Program.meshes[ Program.getPaneIndex( meshPaneId ) ].nc.Size( )
 * @pre globalId >= 0.
 * @pre hash != NULL.
 */
void updateHash( const int meshPaneId, const int pointId, const int globalId, MeshPointIdToGlobalIdMap *hash )
{
	#ifdef ASSERT_ON
		assert( Program.hasPane( meshPaneId ) );
		assert( pointId >= 0 && pointId < Program.meshes[ Program.getPaneIndex( meshPaneId ) ].nc.Size( ) );
		assert( globalId >= 0 );
		assert( hash != NULL );
	#endif

	hash ->insert( meshPaneId, pointId, globalId );

	int idx = Program.getPaneIndex( meshPaneId );

	#ifdef ASSERT_ON
		assert( idx >= 0 && idx < Program.numberOfPanes );
	#endif

  std::vector< std::pair< int, int > > connections;
	Program.getPaneConnectivity( idx ) ->getPointConnectionPairs( pointId, connections );

	for( int i=0; i < connections.size( ); ++i )
	{

		int remotePaneId = connections[ i ].first;
		int nodeIndex		 = connections[ i ].second;

		#ifdef ASSERT_ON
			assert( Program.hasPane( remotePaneId ) );
		#endif

		int remotePoint 					= Program.getPaneConnectivity( Program.getPaneIndex( remotePaneId ) )->getSharedPointIdAt( meshPaneId, nodeIndex );
		std::pair< int, int > key = std::make_pair( remotePaneId, remotePoint );


		#ifdef ASSERT_ON
			assert( remotePoint >= 0 && remotePoint < Program.meshes[ Program.getPaneIndex( remotePaneId ) ].nc.Size( ) );
			assert( Program.isNodeShared( Program.getPaneIndex( remotePaneId ), remotePoint ) == true );
		#endif

		if( !hash ->exists( remotePaneId, remotePoint ) )
			hash ->insert( remotePaneId, remotePoint, globalId );

	}

}


/**
 * @brief This method adds the vertex with the given x,y,z coordinates to the global node list and returns the assigned global id (index).
 * @param x	the x--coordinate of the vector being added.
 * @param y the y--coordinate of the vector being added.
 * @param z the z--coordinate of the vector being added.
 * @param nodelist the global node list where the new node will be inserted.
 * @return GlobalID the global id of the vertex added.
 * @post nodelist.size( ) is incremented by 3.
 * @post GlobalID >= 0.
 */
int addToGlobalNodeList( const double x, const double y, const double z, std::vector< double > &nodelist )
{
	nodelist.push_back( x );
	nodelist.push_back( y );
	nodelist.push_back( z );

	return( (nodelist.size( )/3)-1 );
}


/**
 * @brief This method will stitch the partitioned solution for the single grid.
 * @param hash a hash of the local mesh points to the corresponding global Id.
 * @param ehash a hash of the lobal element ids to the corresponding global element Id.
 * @pre hash != NULL && ehash != NULL.
 * @pre hash ->size( ) > 0 && ehash ->size( ) > 0..
 * @pre Program.globalNodeList.size( ) > 0.
 * @pre Program.globalElementList.size( ) > 0.
 * @pre Program.solution.size( ) == Program.numberOfPanes.
 */
void stitchSolutionData( MeshPointIdToGlobalIdMap *hash, MeshPointIdToGlobalIdMap *ehash )
{

	Program.solutionignore.resize( Program.smdv.size( ), false );

	std::cout << "Stitching the solution...";
	std::cout.flush( );

	#ifdef ASSERT_ON
		assert( hash != NULL && ehash != NULL );
		assert( Program.globalNodeList.size( ) > 0 );
		assert( Program.globalElementList.size( ) > 0 );
		assert( hash ->size( ) > 0 && ehash ->size( ) > 0 );
		assert( Program.solution.size( ) == Program.numberOfPanes );
	#endif

	/* Initialize the global solution vector */

	Program.globalSolution.resize( Program.smdv.size( ) );

	for( int i=0; i < Program.globalSolution.size( ); ++i )
	{
		if( Program.getDataItemType( i ) == NODEDATA )
			Program.globalSolution[ i ].resize( Program.globalNodeList.size( )*Program.smdv[ i ].ncomp, 	 0.0 );
		else if( Program.getDataItemType( i ) == CELLDATA )
			Program.globalSolution[ i ].resize( Program.globalElementList.size( )*Program.smdv[ i ].ncomp, 0.0 );
		else {
			std::cerr << "Code should not reach here!\n";
			std::cerr << "File: " << __FILE__ << std::endl;
			std::cerr << "Line: " << __LINE__ << std::endl;
			assert( false );
		}

	}

	for( int paneidx=0; paneidx < Program.numberOfPanes; ++paneidx )
	{
		int paneId = Program.paneIds[ paneidx ];

		#ifdef ASSERT_ON
			assert( Program.solution[ paneidx ].size( ) == Program.smdv.size( ) );
		#endif

		for( int dataidx=0; dataidx < Program.solution[ paneidx].size( ); ++dataidx )
		{

			#ifdef ASSERT_ON
				assert( dataidx >= 0 && dataidx < Program.solution[paneidx].size( ) );
			#endif

			if( Program.getDataItemType( dataidx ) == NODEDATA )
			{

			  if( Program.solution[ paneidx ][ dataidx ].size( ) == 0 )
			  {
			  	Program.solutionignore[ dataidx ] = true;
			  	Program.globalSolution[ dataidx ].resize( 0 );
			  }
			  else
			  {

			  	int N 			 = Program.smdv[ dataidx ].ncomp;
			  	int NumNodes = Program.meshes[ paneidx ].nc.Size( );

			  	#ifdef ASSERT_ON
			  	  assert( Program.meshes[ paneidx ].nc.Size( )*N == Program.solution[ paneidx ][ dataidx ].size( ) );
					#endif

			  	for( int localNodeId=0; localNodeId < NumNodes; ++localNodeId )
			  	{

						#ifdef ASSERT_ON
							assert( hash ->exists( paneId, localNodeId ) );
						#endif

						int globalId = hash ->getGlobalId( paneId, localNodeId );
			  		for( int component=0; component < N; ++component )
			  		{
			  			int gidx = globalId*N+component;     /* The global index */
			  			int lidx = localNodeId*N+component;  /* The local index  */

							#ifdef ASSERT_ON
								assert( gidx >= 0 && gidx < Program.globalSolution[ dataidx ].size( ) );
								assert( lidx >= 0 && lidx < Program.solution[ paneidx ][ dataidx ].size( ) );
							#endif

							Program.globalSolution[ dataidx ][ gidx ] = Program.solution[ paneidx ][ dataidx ][ lidx ];

			  		} // End for all components

			  	} // End for all nodes

			  } // End else

			} // End if NodeData
			else if( Program.getDataItemType( dataidx ) == CELLDATA )
			{
				if( Program.solution[ paneidx ][ dataidx ].size( ) == 0 )
				{
					Program.solutionignore[ dataidx ] = true;
					Program.globalSolution[ dataidx ].resize( 0 );
				}
				else
				{

					int N 				  = Program.smdv[ dataidx ].ncomp;
					int NumElements = Program.meshes[ paneidx ].con.Nelem( );

					#ifdef ASSERT_ON
						assert( Program.meshes[ paneidx ].con.Nelem( )*N == Program.solution[ paneidx ][ dataidx ].size( ) );
					#endif

					for( int localElement=0; localElement < NumElements; ++ localElement )
					{
						#ifdef ASSERT_ON
							assert( ehash ->exists( paneId, localElement ) );
						#endif

						int globalId = ehash ->getGlobalId( paneId, localElement );
						for( int component=0; component < N; ++component )
						{
							int gidx = globalId*N+component;			/* The global index */
							int lidx = localElement*N+component;	/* The local index */

							#ifdef ASSERT_ON
								assert( gidx >= 0 && gidx < Program.globalSolution[ dataidx ].size( ) );
								assert( lidx >= 0 && lidx < Program.solution[ paneidx ][ dataidx ].size( ) );
							#endif

							Program.globalSolution[ dataidx ][ gidx ] = Program.solution[ paneidx ][ dataidx ][ lidx ];

						} // End for all components

					} // End for all elements

				} // End else

			} // End if CellData
			else
			{
				std::cerr << "Code should not reach here!\n";
				std::cerr << "File: " << __FILE__ << std::endl;
				std::cerr << "Line: " << __LINE__ << std::endl;
				assert( false );
			}

		} /* End for all solution data */

	} /* End For all panes */

	std::cout << "[DONE]\n";

}

/**
 * @brief This method will stitch the grids into a single, global grid.
 * @pre Program.meshes.size( ) == Program.numberOfPanes
 * @pre Program.pconns.size( ) == Program.numberOfPanes
 * @post Program.globalMesh is constructed.
 */
void stitchGrids( )
{
	std::cout << "Stitching grids...";
  std::cout.flush( );

	#ifdef ASSERT_ON
		assert( Program.meshes.size( )  == Program.numberOfPanes );
		assert( Program.pconns.size( )  == Program.numberOfPanes );
		assert( Program.paneIds.size( ) == Program.numberOfPanes );
	#endif

	MeshPointIdToGlobalIdMap  hash; /* Node Hash */
	MeshPointIdToGlobalIdMap ehash; /* Element Hash */

	for( int i=0; i < Program.meshes.size( ); ++i )
	{
		int meshPaneId 							 = Program.paneIds[ i ];
		Mesh::UnstructuredMesh *mesh = &( Program.meshes[ i ] );

		#ifdef ASSERT_ON
			assert( mesh != NULL );
		#endif


		int count = 0;
		for( int node=0; node < mesh ->nc.Size( ); ++node )
		{
			if( ! Program.isNodeShared( i, node ) )
			{
				/* insert the node */
				int globalId = addToGlobalNodeList(
													mesh ->nc.x( node+1 ), mesh ->nc.y( node+1 ), mesh ->nc.z( node+1 ),
													Program.globalNodeList );

				#ifdef ASSERT_ON
					assert( ! hash.exists( meshPaneId, node ) );
				#endif

				hash.insert( meshPaneId, node, globalId );

			}
			else if( !hash.exists( meshPaneId, node ) )
			{
				/* insert the node */
				int globalId = addToGlobalNodeList(
													mesh ->nc.x( node+1 ), mesh ->nc.y( node+1 ), mesh ->nc.z( node+1 ),
													Program.globalNodeList );

				/* update the hash */
				updateHash( meshPaneId, node, globalId, &hash );
			}

		} /* End for all nodes */

		for( int element=0; element < mesh ->con.Nelem( ); ++element )
		{
			int size = mesh ->con.Esize( element+1 );

			std::vector< int > elt;
			elt.resize( size );

			for( int i=0; i < size; ++i )
			{
				int localId  = mesh ->con.Node( element+1, i+1 )-1;

				#ifdef ASSERT_ON
				  assert( hash.exists( meshPaneId, localId ) );
				#endif

				int globalId = hash.getGlobalId( meshPaneId, localId );
				elt[ i ] 		= globalId;
			}

			Program.globalElementList.push_back( elt );
			int globalElementId = Program.globalElementList.size( )-1;
			ehash.insert( meshPaneId, element, globalElementId );

		} /* End for all elements */

	} /* End for all meshes */

	std::cout << "[DONE]\n";

	std::cout << "Total number of nodes: " 		<< (Program.globalNodeList.size( )/3) << std::endl;
	std::cout << "Total number of elements: " << Program.globalElementList.size( )  << std::endl;

	#ifdef ASSERT_ON
		printVtk( Program.globalNodeList, Program.globalElementList );
	#endif

	stitchSolutionData( &hash, &ehash );

}

/**
 * @brief Masks the points that are shared based on the pane connectivity.
 * @post Program.mask is created for each mesh.
 * @pre Program.paneIds
 * @pre Program.mask.size( ) == 0.
 * @pre Program.numberOfPanes >= 1.
 * @pre Program.pconnSizes[ i ] >= 0, 0 <= i < Program.numberOfPanes.
 * @pre Program.pconns[ i ] != NULL,  0 <= i < Program.numberOfPanes.
 */
void maskPoints( )
{

	#ifdef ASSERT_ON
		assert( Program.mask.size( ) == 0 );
		assert( Program.numberOfPanes >= 1 );
		assert( Program.pconns.size( ) == Program.numberOfPanes );
		assert( Program.paneIds.size( ) == Program.numberOfPanes );
	#endif

	Program.mask.resize( Program.numberOfPanes );
	for( int pane=0; pane < Program.numberOfPanes; ++pane )
	{
		/* Initially no nodes are shared so the entire mask is set to false */
		Program.mask[ pane ].resize( Program.meshes[ pane ].nc.Size( ),false );

		/* Get pointer to the pane connectivity object corresponding to this pane */
		PaneConnectivity *pconPtr = &( Program.pconns[ pane ] );

		#ifdef ASSERT_ON
			assert( pconPtr != NULL );
		#endif

		/* Get the remote pane ids connecting to this pane */
		std::vector< int > remotePaneIds;
		pconPtr ->getRemotePaneIds( remotePaneIds );

		#ifdef ASSERT_ON
			assert( pconPtr ->getNumberOfConnections( ) == remotePaneIds.size( ) );
		#endif

	  /* For all remote panes, get the local shared nodes that are shared with each remote pane */
		for( int rmtPane=0; rmtPane < remotePaneIds.size( ); ++rmtPane )
		{
//			std::cout << "Pane: [" << Program.paneIds[ pane ] << "] shares ";
//			std::cout << pconPtr ->getNumberOfNodesSharedWithRemotePane( remotePaneIds[ rmtPane ] );
//			std::cout << " Remote pane: [" << remotePaneIds[ rmtPane ] << "]\n";

			int id = remotePaneIds[ rmtPane ];
			std::vector< int > nodeList = pconPtr ->getNodesSharedWithRemotePane( id );

			#ifdef ASSERT_ON
				assert( nodeList.size( ) == pconPtr ->getNumberOfNodesSharedWithRemotePane( id ) );
			#endif

			for( int i=0; i < nodeList.size( ); ++i )
			{
				int nodeId = nodeList[ i ];

				#ifdef ASSERT_ON
					assert( nodeId >= 0 && nodeId < Program.mask[ pane ].size( ) );
				#endif

				Program.mask[ pane ][ nodeId ] = true;

			} /* For all shared nodes in the node list */

		} /* End For all remote panes */

	} /* End For all panes, i.e., zones or sub-domains */

}

/**
 * @brief Computes the pane connectivity using Rocmap
 * @note Based on Rocmap's User/Developer guid.
 * @param wname the window name.
 */
void getPaneConnectivity( const std::string &wName )
{

	Program.pconns.resize( Program.numberOfPanes );
	std::cout << "Getting the pane connectivity...";

	int MAP_compute_pconn = COM_get_function_handle( "MAP.compute_pconn" );
	int mesh_hndl 			  = COM_get_attribute_handle( ( wName + ".mesh" ).c_str( ) );
	int pconn_hndl 				= COM_get_attribute_handle( ( wName + ".pconn" ).c_str( ) );

	COM_call_function( MAP_compute_pconn, &mesh_hndl, &pconn_hndl );

	for( int i=0; i < Program.numberOfPanes; ++i )
	{

		/* Get the pconn array */
		int *pconn = NULL;
		COM_get_array( ( wName + ".pconn" ).c_str( ),	/* Window Name (in) */
										Program.paneIds[ i ],					/* Pane Id (in) */
										&pconn 	);										/* pconn array (in/out) */

		#ifdef ASSERT_ON
			assert( pconn != NULL );
		#endif

		int size = -1;
		int gs   = -1;
		COM_get_size( ( wName + ".pconn" ).c_str( ),	/* Window Name (in) */
									 Program.paneIds[ i ],					/* Pane Id (in) */
									 &size,													/* Size of the pconn array (in/out) */
									 &gs );													/* ghost node size  (in/out) */

		#ifdef ASSERT_ON
			assert( size >= 0 );
			assert( gs == 0 );
		#endif

		Program.pconns[ i ].constructPaneConnectivityFromArray( pconn, size );

		delete [] pconn;
		pconn = NULL;
	}
	std::cout << "[DONE]\n";

	maskPoints( );

}

/**
 * @brief Writes VTK files for the corresponding meshes.
 * @param meshes List of meshes in the current window.
 */
void writeVtkFiles( std::vector< Mesh::UnstructuredMesh > &meshes )
{

	// TODO: Generalize this function for any type of mesh.
	for( int m=0; m < meshes.size( ); ++m )
	{

		std::ostringstream oss; oss.clear( );
		oss << Program.outputfile << "." << Program.paneIds[ m ] << ".vtk";

		std::ofstream ofs;
		ofs.open( oss.str( ).c_str( ) );

		if( !ofs.is_open( ) )
		{
			std::cerr << "Cannot write VTK file: " << oss.str( ) << std::endl;
			std::cerr << "File: " << __FILE__ << std::endl;
			std::cerr << "Line: " << __LINE__ << std::endl;
			assert( false );
		}

		ofs << "# vtk DataFile Version 3.0\n";
		ofs << oss.str( ) << std::endl;
		ofs << "ASCII\n";
		ofs << "DATASET UNSTRUCTURED_GRID\n";
		ofs << "POINTS " << meshes[ m ].nc.Size( ) << " double\n";

		for( int i=1; i <= meshes[ m ].nc.Size( ); ++i )
		{
			ofs << meshes[ m ].nc.x( i ) << " ";
			ofs << meshes[ m ].nc.y( i ) << " ";
			ofs << meshes[ m ].nc.z( i ) << "\n";
		}

		ofs << "CELLS " << meshes[ m ].con.Nelem( ) << " " << meshes[ m ].con.Nelem( )*9 << "\n";
		for( int e=1; e <= meshes[ m ].con.Nelem( ); ++e )
		{
			ofs <<  "8 ";
			for( int j=1; j <= 8; ++j )
				ofs << meshes[ m ].con.Node( e, j )-1<< " ";
			ofs << std::endl;
		}

		ofs << "CELL_TYPES " << meshes[ m ].con.Nelem( ) << std::endl;
		for( int e=1; e <= meshes[ m ].con.Nelem( ); ++ e )
			ofs << "12\n";

		ofs << "POINT_DATA " << Program.mask[ m ].size( ) << std::endl;
		ofs << "SCALARS SharedNodes double\nLOOKUP_TABLE default\n";
		for( int i=0; i < Program.mask[ m ].size( ); ++i )
		{
			if( Program.mask[ m ][ i ] )
				ofs << "1\n";
			else
				ofs << "0\n";
		}
		ofs.close( );

	}

}


/**
 * @brief Utilizes Roctail to convert the panes to an unstructured grid format
 * @param wName the window name.
 * @return S new window name which holds the unstructured mesh data.
 */
std::string convertWindow( const std::string &wName )
{

	std::cout << "\n\n\n";
	std::cout << "Converting window to Unstructured mesh...";
	std::cout.flush( );

	TAIL_Window2UnstructuredMesh( wName, Program.meshes, Program.smdv, Program.solution, 0, true );

	std::cout << "[DONE]\n";

//	for( int i=0; i < Program.solution.size( ); ++i )
//	{
//		std::cout << "I: " << i << std::endl;
//		for( int j=0; j < Program.solution[ i ].size( ); ++j )
//		{
//			std::cout << "\tJ: " << j << " Name: " << Program.smdv[ j ].name << " Location: " << Program.smdv[ j ].loc;
//			std::cout << " Number of components: " << Program.smdv[ j ].ncomp << std::endl;
//			for( int k=0; k < Program.solution[ i ][ j ].size( ); ++k )
//			{
//				std::cout << "\t\t k[" << k << "]:" << Program.solution[ i ][ j ][ k ] << std::endl;
//			}
//		}
//	}
//	assert( false );

	#ifdef ASSERT_ON
		assert( Program.meshes.size( ) == Program.numberOfPanes );
	#endif

  std::string newWindow( "unstructured" );
  COM_new_window( newWindow.c_str( ) );

	for( int i=0; i < Program.meshes.size( ); ++i )
	{
		TAIL_UnstructuredMesh2Pane( newWindow, Program.paneIds[ i ],
			 Program.meshes[ i ], Program.smdv[ i ], Program.solution[ i ], 0 );
	}

  COM_window_init_done( newWindow.c_str( ) );

//	#ifdef ASSERT_ON
//		assert( Program.solution.size( ) == Program.numberOfPanes );
//		for( int i=0; i < Program.solution.size( ); ++i )
//		{
//			assert( Program.solution[ i ].size( ) == Program.smdv.size( ) );
//			for( int j=0; j < Program.solution[ i ].size( ); ++j )
//			{
//				assert( Program.solution[ i ][ j ].size( ) > 0 );
//			}
//		}
//	#endif

	return newWindow;
}

/**
 * @brief This function prints the panes/grids in the window
 * a into a Tecplot file.
 * @param wname The name of the window.
 */
void printWindow( const std::string &wName )
{
	std::string window = wName;

	if( Program.blocks )
	{
		std::cout << "Converting block-structured grids to unstructured...\n";
		std::cout.flush( );

		window = convertWindow( wName );

		std::cout << "[DONE]\n";
		std::cout.flush( );
	}
	else
	{
		//TODO: Load the Unstructured meshes from the window to the data-structures.
		std::cerr << "Unstructured grids are currently not supported!\n";
		std::cerr << "If your data-set consists of block structured grids make sure the -block-structured argument is supplied at the command line.\n";
		std::cerr << "File: " << __FILE__ << std::endl;
		std::cerr << "Line: " << __LINE__ << std::endl;
		std::cerr << "core dumping...\n";
		assert( false );
	}

	/* Get the pane connectivity and mask the shared points. */
	getPaneConnectivity( window );

	#ifdef ASSERT_ON
		if( Program.vtk )
		{
			std::cout << "Writting VTK files...";
			 writeVtkFiles( Program.meshes );
			std::cout << "[DONE]\n";
		}
	#endif

	// TODO: delete the window and panes, the data is now
	// stored in the separate data-structures, hence it is
	// no longer needed.

	stitchGrids( );

	if( Program.vtk )
		writeVtkData( );

	writePlotFile( );

}


/**
 * @brief Returns a window consisting of the HDF input files.
 * @return s the window name.
 * @note The window layout is changed to contiguous and the
 * number of panes and paneIds is obtained.
 */
std::string getWindow( )
{
	std::string win_in;
	if( Program.readControlFile )
	  win_in = Program.cntrlfile;
	else
	  win_in = Program.fileregx;

	std::string::size_type st = win_in.find_last_of('/');
	if (st != std::string::npos)
		win_in.erase(0, st+1);
	st = win_in.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
	if (st != std::string::npos)
		win_in.erase(st);

  // Initialize an empty time string
  int  len = 15;
  char timeStr[16];
  timeStr[0] = '\0';

  if( Program.readControlFile )
  {
		std::cout << "Reading by control file " << Program.cntrlfile << " into window " << win_in << "...";
		int IN_read = COM_get_function_handle( "IN.read_by_control_file" );
		COM_call_function( IN_read,Program.cntrlfile.c_str( ),
												win_in.c_str(),NULL,timeStr,&len	);
		std::cout << "[DONE]\n";

	}
	else
	{
		std::cout << "Reading HDF file(s) " << Program.fileregx << " into window " << win_in << "...";
		int IN_read = COM_get_function_handle( "IN.read_window");
		COM_call_function( IN_read,Program.fileregx.c_str( ),
												win_in.c_str(),NULL,NULL,timeStr,&len	);
		std::cout << "[DONE]\n";
	}

  // Change the memory layout to contiguous
  // Taken from from Roctail::TAIL_HDF2Window
  std::string win_out( "win_out" );
  COM_new_window( win_out.c_str( ) );
	COM_clone_attribute( (win_out+".all").c_str(), (win_in+".all").c_str(), 1 );
	COM_window_init_done( win_out.c_str( ) );

	// Delete the old memory layout.
	COM_delete_attribute(( win_in+".atts").c_str( ) );
	COM_delete_window( win_in.c_str( ) );

	// Get the number of panes and the corresponding paneIds
	int *paneids = NULL;
	COM_get_panes( win_out.c_str(), &(Program.numberOfPanes), &paneids );
	Program.paneIds.resize( Program.numberOfPanes );

	// Compute the paneId2index mapping
	for( int i=0; i < Program.numberOfPanes; ++i )
	{
		Program.paneIds[ i ] = paneids[ i ];

		#ifdef ASSERT_ON
			assert( Program.paneIds2index.find( Program.paneIds[ i ] ) ==
							Program.paneIds2index.end( ) );
		#endif

		Program.paneIds2index[ Program.paneIds[ i ] ] = i;
	}

	delete [] paneids; paneids = NULL;

	#ifdef ASSERT_ON
		assert( Program.paneIds2index.size( ) == Program.numberOfPanes );
	#endif

	return win_out;

}

/**
 * @brief Parses the user-supplied command line arguments.
 * @param argc the argument counter
 * @param argv the argument vector
 */
void parseArguments( int argc, char **argv )
{
	Program.prgmname 				= std::string( argv[ 0 ] );
	Program.withGhost  		  = false;
	Program.readControlFile = false;
	Program.blocks					= false;
	Program.vtk							= false;
	Program.numberOfPanes   = 0;
	Program.cntrlfile 			= "";
	Program.fileregx				= "";

	for( int i=1; i < argc; ++i )
	{

		if( std::strcmp( argv[ i ], "-g" ) == 0 )
		{
			Program.withGhost = true;
		}
		else if( std::strcmp( argv[ i ], "-c" ) == 0 )
		{
			Program.readControlFile = true;
			Program.cntrlfile = std::string( argv[ ++i ] );
		}
		else if( std::strcmp( argv[ i ], "-o" ) == 0 )
		{
			Program.outputfile = std::string( argv[ ++i ] );
		}
		else if( std::strcmp( argv[ i ], "-regex") == 0 )
		{
			Program.fileregx = std::string( argv[ ++i ] );
		}
		else if( std::strcmp( argv[ i ], "-block-structured") == 0  )
		{
			Program.blocks = true;
		}
		else if( std::strcmp( argv[ i ], "-vtk" ) == 0 )
		{
			Program.vtk = true;
		}
		else if( std::strcmp( argv[ i ], "-h") == 0  )
		{
			showUsage( );
			exit( 0 );
		}

	}

	if( Program.cntrlfile == "" && Program.fileregx == "" )
	{
		std::cerr << "Error parsing command line input!\n";
		showUsage( );
		exit( -1 );
	}


}

/**
 * @brief Prints usage and examples to STDOUT.
 */
void showUsage( )
{
	std::cout << "HDF2PLT OPTIONS:\n";
	std::cout << "-regex \"{somestring}*.hdf\" : Specifies a set of hdf files\n";
	std::cout << "-g : Enables ghost node inclusion in the output file\n";
	std::cout << "-c {cntrlfile} : Specifies a control file to use\n";
	std::cout << "-block-structured: Indicates the the input files are block-structured grids which must be converted to unstructured format\n";
	std::cout << "-o {outputfile} : Specifies output file name. Note, do not append a file extension. A file extension will be automatically appended.\n";
	std::cout << "-vtk : Enables printing of VTK files as well.\n";
	std::cout << "-h : Prints this help menu.\n";
	std::cout << "Examples:\n";
	std::cout << "\thdf2plt -g -regex \"fluid*.hdf\" -block-structured -o output\n";
	std::cout.flush( );
}








/**
 *  @brief This utility code reads in the *.top file information and
 *  generates the initial particle solution by finding the injecting
 *  surfaces. The injecting surface are identified by a user-supplied
 *  boundary condition that corresponds to the boundary condition used
 *  to indicate an injecting surface in the *.top file.
 *
 *  @file plagprep.C
 *  @date Nov 12, 2009
 *  @author George Zagaris (gzagaris@illinois.edu)
 */

//============================================================================
//								P R E P R O C E S S O R  D I R E C T I V E S
//============================================================================
#include <iostream>
#include <cstdlib>
#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>
#include <cstring>

//================================================================================
//								G L O B A L  V A R I A B L E S
//================================================================================

/**< @brief The project name, supplied by the user.  */
std::string prjname;

/**< @brief The output file, computed based on prjname. */
std::string outputfile;

/**< @brief The number of blocks, read from the *.top file. */
int numberOfBlocks;

/**
 * @brief The boundary condition corresponding to an injecting surface.
 * Supplied by the user and represents the boundary condition that
 * will be searched in the prjname.top file.
 */
int bndrycondition;


//=================================================================================
//				F U N C T I O N  P R O T O T Y P E  D E C L A R A T I O N
//=================================================================================

/**
 * @brief Displays usage information for this utility.
 */
void showUsage( );

/**
 * @brief Parses the user-supplied parameters for this utility.
 * @param argc the argument counter.
 * @param argv the argument vector.
 * @return status true if the command line parameters are parsed
 * successfully, otherwise false.
 */
bool parseCmdParameters( int argc, char **argv );

/**
 * @brief Searches the prjname.top file for injecting boundary conditions.
 * @post an prjname.plag_sola_0.00000E+00 is written.
 */
void searchTopFile( );

/**
 * @brief Program main.
 * @param argc the argument counter.
 * @param argv the argument vector.
 * @return rc the return code.
 * @note rc != iff an error occured.
 */
int main( int argc, char **argv )
{
	std::cout << __DATE__ << " Running program: " << argv[ 0 ] << std::endl;

	std::cout << "Parsing command line parameters...";
	if( parseCmdParameters( argc, argv ) )
	{
		std::cout << "[DONE]\n";

		std::cout << "Searching " << prjname << ".top for BC=" << bndrycondition << "...";
		 searchTopFile( );
		std::cout << "[DONE]\n";

		std::cout << "Output is saved at " << outputfile << "!\n";

	}
	else
	{
		std::cout << "[DONE]\n";

		std::cerr << "Error parsing command line parameters!\n";
		std::cerr << "File: " << __FILE__ << std::endl;
		std::cerr << "Line: " << __LINE__ << std::endl;

		showUsage( );

		return(-1 );
	}

	return( 0 );

}

//=============================================================================
//				F U N C T I O N  P R O T O T Y P E  I M P L E M E N T A T I O N
//=============================================================================

void searchTopFile( )
{
	std::string dummyline;

	std::ofstream ofs;
	ofs.open( outputfile.c_str( ) );
	if( !ofs.is_open( ) )
	{
		std::cerr << "\n Error: Cannot open output file " << outputfile << std::endl;
		std::cerr << "File: " << __FILE__ << std::endl;
		std::cerr << "Line: " << __LINE__ << std::endl;
	}

	ofs << "0.00E+000\n";

	std::ifstream  ifs;
	ifs.open( ( prjname+std::string(".top") ).c_str( ) );
	if( !ifs.is_open( ) )
	{
		std::cerr << "\n Error: Cannot open input file " << prjname+std::string(".top") << std::endl;
		std::cerr << "File: " << __FILE__ << std::endl;
		std::cerr << "Line: " << __LINE__ << std::endl;
	}

	std::getline( ifs, dummyline );
	std::getline( ifs, dummyline );

	ifs >> numberOfBlocks;
	std::getline( ifs, dummyline );

	for( int i=1; i <= numberOfBlocks; ++i )
		ofs << i << " 0 0\n";

	int blockidx,bc,npatches;
	for( int i=0; i < numberOfBlocks; ++i )
	{
		ifs >> blockidx;
		std::getline( ifs, dummyline );
		ifs >> npatches;
		std::getline( ifs, dummyline );

		for( int j=0; j < npatches; ++j )
		{
			ifs >> bc;
			std::getline( ifs, dummyline );
			if( bc == bndrycondition )
				ofs << blockidx << " 0\n";
		}
	}

	ofs.close( );
	ifs.close( );

}

//=============================================================================

void showUsage( )
{
	std::cout << "plagprep -- generates the initial particle solution dump at t=0\n";
	std::cout << "OPTIONS:\n";
	std::cout << "-bc {bc} sets the boundary condition used to identify injection surfaces. Default is 91.\n";
	std::cout << "-p {projectname} sets the project name, essentially the filename without the suffix.\n";
	std::cout << "-h prints this help menu.\n";
	std::cout << "Usage: ./plagprep -p titan_60s_MP\n";
	std::cout.flush( );
}

//=============================================================================

bool parseCmdParameters( int argc, char **argv )
{
	prjname 						= "";
	outputfile					= "";
	numberOfBlocks			= 0;
	bndrycondition			= 91;

	for( int i=1; i < argc; ++i )
	{
		if( std::strcmp( argv[ i ], "-bc") == 0 )
		{
			bndrycondition = std::atoi( argv[ ++i ] );
		}
		else if( std::strcmp( argv[ i ], "-p") == 0 )
		{
			prjname    = std::string( argv[ ++i ] );
			outputfile = prjname + std::string( ".sola_0.00000E+00" );
		}
		else if( std::strcmp( argv[ i ], "-h") == 0  )
		{
			showUsage( );
			exit( 0 );
		}
	}

	if( prjname == "" )
		return false;

	return true;
}



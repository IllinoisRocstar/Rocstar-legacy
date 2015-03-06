/* *******************************************************************
 * Rocstar Simulation Suite                                          *
 * Copyright@2015, Illinois Rocstar LLC. All rights reserved.        *
 *                                                                   *
 * Illinois Rocstar LLC                                              *
 * Champaign, IL                                                     *
 * www.illinoisrocstar.com                                           *
 * sales@illinoisrocstar.com                                         *
 *                                                                   *
 * License: See LICENSE file in top level of distribution package or *
 * http://opensource.org/licenses/NCSA                               *
 *********************************************************************/
/* *******************************************************************
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   *
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES   *
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND          *
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE CONTRIBUTORS OR           *
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   *
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE    *
 * USE OR OTHER DEALINGS WITH THE SOFTWARE.                          *
 *********************************************************************/
/**
 ** @file HDF4 to Tecplot ASCII format
 ** @author: Johnny C. Norris II
 **
 ** Shamelessly ripped off from printin.C, which was written by
 ** Orion Sky Lawlor.
 **
 ** 07/03/04. X. Jiao. Fixed bugs for structured meshes.
 **/


#include <cstdlib>
#include <cassert>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <cstring>

#include "roccom.h"

#define SwitchOnDataType(dType, funcCall) \
   switch (dType) { \
      case COM_CHAR: \
      case COM_BYTE: \
         { typedef char TT; \
           funcCall; } \
         break; \
      case COM_UNSIGNED_CHAR: \
         { typedef unsigned char TT; \
           funcCall; } \
         break; \
      case COM_SHORT: \
         { typedef short TT; \
           funcCall; } \
         break; \
      case COM_UNSIGNED_SHORT: \
         { typedef unsigned short TT; \
           funcCall; } \
         break; \
      case COM_INT: \
         { typedef int TT; \
           funcCall; } \
         break; \
      case COM_UNSIGNED: \
         { typedef unsigned int TT; \
           funcCall; } \
         break; \
      case COM_LONG: \
         { typedef long TT; \
           funcCall; } \
         break; \
      case COM_UNSIGNED_LONG: \
         { typedef unsigned long TT; \
           funcCall; } \
         break; \
      case COM_FLOAT: \
         { typedef float TT; \
           funcCall; } \
         break; \
      case COM_DOUBLE: \
         { typedef double TT; \
           funcCall; } \
         break; \
      case COM_LONG_DOUBLE: \
         { typedef long double TT; \
           funcCall; } \
         break; \
   }

struct AttrInfo {
  std::string m_name;
  char m_location;
  int m_dataType;
  int m_numComp;
  std::string m_units;
};

struct ConnInfo {
  std::string m_name;
  std::string m_type;
  int m_numElements;
  int m_numGhost;
};

namespace HDF2PLT
{

	struct Patch
	{
			int boundaryCondition;				/**< Patch boundary condition number */
			int localBlockFaceNumer;			/**< The local block face number */
			int L1BEGIN;									/**< The local patch l1 begin coordinate */
			int L1END;										/**< The local patch l1 end coordinate */
			int L2BEGIN;									/**< The local patch l2 begin coordinate */
			int L2END;										/**< The local patch l2 end coordinate */
			int remoteBlockNumber;				/**< The remote block number */
			int remoteBlockFaceNumber;		/**< The remote block face number that is connecting with this block */
			int remoteL1BEGIN;					  /**< The remote block's patch l1 begin coordinate */
			int remoteL1END;							/**< The remote block's patch l1 end coordinate */
			int remoteL2BEGIN;					  /**< The remote block's patch l2 begin coordinate */
			int remoteL2END;					    /**< The remote block's patch l2 end coordinate */
			int coupled;								  /**< A flag to the external solver */
	};

	struct Block
	{
			int blockNumber;
			int gridLevels;
			int Ni;
			int Nj;
			int Nk;
			std::vector< Patch > pathces;
	};

	struct SGStr
	{
		int numBlocks;
		std::map< int, Block > blocks;
	} StructuredGrid;

} // End Namespace HDF2PLT


/**
 * @brief Gets the block number by parsing the leading significant digits.
 * @param zonenumber the zone number.
 * @param NBlocks the total number of blocks.
 * @return  N the number without the trailing zeros.
 */
inline int getBlockNumber( const int zonenumber, const int NBlocks )
{
	std::stringstream iss;
	iss << zonenumber;
	std::string s 	 = iss.str( );
	bool found 			 = false;
	int reduceFactor = 4;
	int B						 = -1;
	while( !found )
	{
		s.resize( reduceFactor );
		B = std::atoi( s.c_str( ) );
		if( B <= NBlocks )
		{
			found = true;
		}
		--reduceFactor;
	}
	return( B );
}

inline void resolveConnectivity( )
{
	// TODO: implement this
}

inline void readBlock( std::ifstream &ifs )
{
	std::string dummyline;
	int N;

	if( !ifs.is_open( ) )
	{
		std::cerr << "Error opening reading file!"<< std::endl;
		std::cerr << "FILE: " << __FILE__ << std::endl;
		std::cerr << "LINE: " << __LINE__ << std::endl;
		exit( -1 );
	}

	HDF2PLT::Block b;
	ifs >> b.blockNumber >> b.gridLevels;
//	std::cout << "# " << b.blockNumber << " ";

	std::getline( ifs, dummyline );

	ifs >> N >> b.Ni >> b.Nj >> b.Nk;
//	std::cout << b.Ni << " " << b.Nj << " " << b.Nk << std::endl;

	std::getline( ifs,dummyline );

	b.pathces.resize( N );
	for( int i=0; i < N; ++i )
	{
		HDF2PLT::Patch p;
		ifs >> p.boundaryCondition;
		ifs >> p.localBlockFaceNumer;
		ifs >> p.L1BEGIN;
		ifs >> p.L1END;
		ifs >> p.L2BEGIN;
		ifs >> p.L2END;
		ifs >> p.remoteBlockNumber;
		ifs >> p.remoteBlockFaceNumber;
		ifs >> p.remoteL1BEGIN;
		ifs >> p.remoteL1END;
		ifs >> p.remoteL2BEGIN;
		ifs >> p.remoteL2END;
		ifs >> p.coupled;
		std::getline( ifs, dummyline );
		b.pathces[ i ] = p;
	}

	assert( HDF2PLT::StructuredGrid.blocks.find( b.blockNumber ) ==
					HDF2PLT::StructuredGrid.blocks.end( ) );
	HDF2PLT::StructuredGrid.blocks[ b.blockNumber ] = b;


}

inline void readTopFile( const std::string file )
{
	std::ifstream ifs;
	std::string dummyline;

	ifs.open( file.c_str( ) );

	if( !ifs.is_open( ) )
	{
		std::cerr << "Error opening file " << file << std::endl;
		std::cerr << "FILE: " << __FILE__ << std::endl;
		std::cerr << "LINE: " << __LINE__ << std::endl;
		exit( -1 );
	}

	std::getline( ifs, dummyline );
	std::getline( ifs, dummyline );

	ifs >> HDF2PLT::StructuredGrid.numBlocks;

	std::getline( ifs, dummyline );

	int blockId;

	for( int i=0; i < HDF2PLT::StructuredGrid.numBlocks; ++i )
		readBlock( ifs );

	ifs.close( );
}

template <typename TT>
void PrintStructured(const TT* pData, const int ndims, const int* dims_nodes,
                     int ghost, char loc, std::ostream& out)
{
  const int dims[3] = { dims_nodes[0] - (loc!='n'),
                        ndims>=2 ? dims_nodes[1] - (loc!='n') : 0,
                        ndims>=3 ? dims_nodes[2] - (loc!='n') : 0};

  const int last[3] = { dims[0] - ghost,
                        ndims>=2 ? dims[1] - ghost : 0,
                        ndims>=3 ? dims[2] - ghost : 0};
  int count = 0;

  if ( ndims==3)
  {
    for (int k=ghost; k<last[2]; ++k)
    {
      for (int j=ghost; j<last[1]; ++j)
      {
        for (int i=ghost; i<last[0]; ++i,++count)
        {
          out << "  " << std::scientific
              << (!pData ? (TT)-987654321
                         : (loc=='p' ? pData[0]
                                     : pData[i+j*dims[0]+k*dims[0]*dims[1]]));
          if (count % 10 == 9) out << std::endl;
        }
      }
    }
  }
  else if (ndims==2)
  {
    for (int j=ghost; j<last[1]; ++j)
    {
      for (int i=ghost; i<last[0]; ++i,++count)
      {
        out << "  " << std::scientific
            << (!pData ? (TT)-987654321
                       : (loc=='p' ? pData[0] : pData[i+j*dims[0]]));
        if (count % 10 == 9) out << std::endl;
      }
    }

  }
  else
  {
    for (int i=ghost; i<last[0]; ++i,++count)
    {
      out << "  " << std::scientific << (!pData ? (TT)-987654321 : (loc=='p' ? pData[0] : pData[i]));
      if (count % 10 == 9)
      	out << std::endl;
    }
  }
  out << std::endl;
}

template <typename TT>
void PrintUnstructured(const TT* pData, int size, bool panel, std::ostream& out)
{
  int i;
  for (i=0; i<size; ++i) {
    out << "  " << std::scientific << (!pData ? (TT)-987654321
                                              : (panel ? pData[0]
                                                       : pData[i]));
    if (i % 10 == 9) out << std::endl;
  }
  out << std::endl;
}

void PrintConn(const int* pConn, const ConnInfo& ci, char type,
               std::ostream& out)
{
  int nn;
  std::istringstream sin(ci.m_type.substr(1));
    sin >> nn;

  int elem;
  for (elem=0; elem<ci.m_numElements-ci.m_numGhost; ++elem) {
    switch (type) {
      case 't': // The overall type is 't', so all the tables must be 't'
        out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
            << "  " << pConn[elem+2*ci.m_numElements] << std::endl;
        break;

      case 'q':
        if (ci.m_type[0] == 't')
          out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
              << "  " << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+2*ci.m_numElements] << std::endl;
        else // must be q4, q8 or q9.
          out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
              << "  " << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << std::endl;
        break;

      case 'T': // The overall type is 'T', so all the tables must be 'T'
        out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
            << "  " << pConn[elem+2*ci.m_numElements] << "  "
            << pConn[elem+3*ci.m_numElements] << std::endl;
        break;

      case 'B':
        if (ci.m_type[0] == 'T')
          out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
              << "  " << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << std::endl;
        else if (ci.m_type == "P5")
          out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
              << "  " << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << "  "
              << pConn[elem+4*ci.m_numElements] << "  "
              << pConn[elem+4*ci.m_numElements] << "  "
              << pConn[elem+4*ci.m_numElements] << "  "
              << pConn[elem+4*ci.m_numElements] << std::endl;
        else if (ci.m_type == "P6" || ci.m_type == "W6")
          out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
              << "  " << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << "  "
              << pConn[elem+4*ci.m_numElements] << "  "
              << pConn[elem+5*ci.m_numElements] << "  "
              << pConn[elem+5*ci.m_numElements] << std::endl;
        else
          out << "  " << pConn[elem] << "  " << pConn[elem+ci.m_numElements]
              << "  " << pConn[elem+2*ci.m_numElements] << "  "
              << pConn[elem+3*ci.m_numElements] << "  "
              << pConn[elem+4*ci.m_numElements] << "  "
              << pConn[elem+5*ci.m_numElements] << "  "
              << pConn[elem+6*ci.m_numElements] << "  "
              << pConn[elem+7*ci.m_numElements] << std::endl;
        break;
    }
  }
}

void COM_print_window(const std::string& wName, const std::string& timeStr,
                      std::ostream& out, bool with_ghost, bool useTopFile )
{

	if( useTopFile )
	{
		std::cout << "Resolving inter-partition connectivity....";
		resolveConnectivity( );
		std::cout << "[DONE]\n";
	}

  // Obtain the list of panes
  int nPanes;
  int* paneIds;
  COM_get_panes(wName.c_str(), &nPanes, &paneIds);

  // Obtain the list of attributes
  int nAttrs;   // Number of attributes
  char* attrStr;  // names of attributes separated by spaces
  COM_get_attributes(wName.c_str(), &nAttrs, &attrStr);

  // First get the nodal coordinate info.
  int i;
  std::string name;
  std::vector<AttrInfo> attrs(nAttrs+1);
  attrs[0].m_name = "nc";
  COM_get_attribute((wName + '.' + attrs[0].m_name).c_str(),
                    &(attrs[0].m_location), &(attrs[0].m_dataType),
                    &(attrs[0].m_numComp), &(attrs[0].m_units));

  // Then get the basic attribute info.
  {
    std::istringstream sin(attrStr);
    for (i=1; i<nAttrs+1; ++i) {
      sin >> attrs[i].m_name;
      COM_get_attribute((wName + '.' + attrs[i].m_name).c_str(),
                        &(attrs[i].m_location), &(attrs[i].m_dataType),
                        &(attrs[i].m_numComp), &(attrs[i].m_units));
    }
  }

  if ( nPanes > 0 )
  {
    out << "TITLE=\"" << wName << ". Time: " << timeStr << ".\"" << std::endl;
    out << "VARIABLES= \"x\", \"y\", \"z\"";
  }

  // Eliminate window and pane attributes, note element attributes.
  // EDIT: pane attributes with one item are used as element data.
  // We might as well get rid of vectors and tensors, too.
  std::vector<int> elemCentered;
  int var = 4;
  std::vector<AttrInfo>::iterator p = attrs.begin();
  ++p;
  while (p != attrs.end()) {
    if ((*p).m_location == 'w') {
      p = attrs.erase(p);
      continue;
    }

    if ((*p).m_location == 'p') {
      bool okay = true;
      int nItems, nGItems;
      for (i=0; i<nPanes && okay; ++i) {
        COM_get_size((wName + '.' + (*p).m_name).c_str(), paneIds[i], &nItems,
                     &nGItems);
        if (nItems - nGItems != 1)
          okay = false;
      }
      if (!okay) {
        p = attrs.erase(p);
        continue;
      }
    }

    if ((*p).m_numComp == 1)
    {
      out << ", \"" << (*p).m_name << '"';
      if ((*p).m_location != 'n')
        elemCentered.push_back(var);
      ++var;
    }
    else {
      int x;
      for (x=1; x<=(*p).m_numComp; ++x) {
        out << ", \"" << x << '-' << (*p).m_name << '"';
        if ((*p).m_location != 'n')
          elemCentered.push_back(var);
        ++var;
      }
    }
    ++p;
  }
  out << std::endl;

  // Loop through the panes to find the meshes
  for ( i=0; i < nPanes; ++i )
  {

    int nElems, ngElems;
    name = wName + ".conn";
    COM_get_size(name.c_str(), paneIds[i], &nElems, &ngElems);
    if ( with_ghost) ngElems = 0;
    if ( nElems-ngElems==0) continue; // Skip empty panes

    out << "ZONE T=\"" << std::setw(5) << std::setfill('0')
        << paneIds[i] << "\", ";

    // Obtain the size of nodes
    int  nNodes;  // total number of nodes
    int  ghost; // Number of ghost nodes
    name = wName + ".nc";
    COM_get_size(name.c_str(), paneIds[i], &nNodes, &ghost);
    if ( with_ghost) ghost=0;

    // Obtain pane connectivity
//    int nPcon; // total number of pane connectivities
//    name = wName +".pconn";
//    COM_get_size( name.c_str( ), paneIds[ i ], &nPcon );
//		// Obtain a reference to the pconn array.
//		const int* pconnArray = NULL;
//		COM_get_array_const( pConnName.c_str( ), paneIds[ i ], &pconnArray );

    // Obtain the connectivity tables
    int nConn;       // Number of connectivity tables
    char* connNames; // Names of connectivity tables separated by space
    COM_get_connectivities(wName.c_str(), paneIds[i], &nConn, &connNames);

    int   ndims;
    const int* dims = NULL;
    char elemType = '\0';
    int eTotal = 0;
    std::vector<ConnInfo> connInfo(nConn);
    if (nConn == 1 && std::strncmp(connNames, ":st", 3) == 0)
    {
    	// Structured mesh

      connInfo.clear();

      name = wName + '.' + connNames;
      COM_get_size(name.c_str(), paneIds[i], &ndims, &ghost);
      if ( with_ghost) ghost=0;

      // Obtain the dimensions (must be a const array) of the pane
      COM_get_array_const(name.c_str(), paneIds[i], &dims);

//      // Obtain the size of the pconn array
//      std::string pConnName = wName + ".pconn";
//      int pconnsize;
//      COM_get_size( pConnName.c_str( ), paneIds[ i ], &pconnsize );
//
//      std::cout << "Here is the pane connectivity size: " << pconnsize << std::endl;
//      // Obtain a reference to the pconn array.
//      const int* pconnArray = NULL;
//      COM_get_array_const( pConnName.c_str( ), paneIds[ i ], &pconnArray );
//
//      for( int panecon=0; panecon <  pconnsize; panecon++ )
//      {
//      	std::cout << pconnArray[ panecon ] << "\n";
//      }

      out << "I=" << dims[0] - 2 * ghost;
      if ( ndims>=2) out << ", J=" << dims[1] - 2 * ghost;
      if ( ndims>=3) out << ", K=" << dims[2] - 2 * ghost;

      out << ", ZONETYPE=ORDERED, ";

      if( useTopFile )
      {
      	std::cout << "Processing paneId: " << paneIds[ i ] << std::endl;
      	int blockNum = getBlockNumber( paneIds[ i ], HDF2PLT::StructuredGrid.numBlocks );
      	std::cout << "Here is the block number: " << blockNum << std::endl;
      	assert( HDF2PLT::StructuredGrid.blocks.find( blockNum ) != HDF2PLT::StructuredGrid.blocks.end( ) );
      	HDF2PLT::Block b = HDF2PLT::StructuredGrid.blocks[ blockNum ];
      	std::cout << dims[ 0 ] << " == " << HDF2PLT::StructuredGrid.blocks[ blockNum ].Ni+7 << std::endl;
      	std::cout << dims[ 1 ] << " == " << HDF2PLT::StructuredGrid.blocks[ blockNum ].Nj+7	<< std::endl;
      	std::cout << dims[ 2 ] << " == " << HDF2PLT::StructuredGrid.blocks[ blockNum ].Nk+7 << std::endl;
      }

    }
    else
    { // Unstructured mesh
      // Obtain the sizes of connectivity tables
      std::istringstream sin(connNames);
      std::vector<ConnInfo>::iterator c;
      for (c=connInfo.begin(); c!=connInfo.end(); ++c)
      {
        sin >> (*c).m_type;
        (*c).m_name = wName + '.' + (*c).m_type;
        (*c).m_type.erase(0, 1);
        std::string::size_type x = (*c).m_type.find(':', 2);
        if (x != std::string::npos)
           (*c).m_type.erase(x);

        COM_get_size((*c).m_name.c_str(), paneIds[i], &((*c).m_numElements),
                     &((*c).m_numGhost));
        if ( with_ghost) (*c).m_numGhost = 0;
        eTotal += (*c).m_numElements - (*c).m_numGhost;

        // Tecplot can't do mixed elements, so we have to use the biggest
        // and fudge the rest.
        if (elemType == '\0' || elemType == 't'
            || ((elemType == 'q' || elemType == 'T')
                && (*c).m_type[0] > 'A' && (*c).m_type[0] < 'Z')
            || (*c).m_type[0] == 'B' || (*c).m_type[0] == 'H')
           elemType = (*c).m_type[0];
      }
      out << "N=" << nNodes - ghost << ", E=" << eTotal << ", ZONETYPE=";
      switch (elemType) {
        case 't':
          out << "FETRIANGLE, ";
          break;

        case 'q':
          out << "FEQUADRILATERAL, ";
          break;

        case 'T':
          out << "FETETRAHEDRON, ";
          break;

        case 'P':
        case 'W':
        case 'H':
          elemType = 'B';
          // Intentional fall-through

        case 'B':
          out << "FEBRICK, ";
          break;
      }

      // free the buffer of connNames
      COM_free_buffer(&connNames);
    }

    out << "DATAPACKING=BLOCK";
    if (!elemCentered.empty()) {
      std::vector<int>::iterator cv = elemCentered.begin();
      out << ", VARLOCATION=([" << *cv;
      ++cv;
      while (cv != elemCentered.end()) {
        out << ',' << *cv;
        ++cv;
      }
      out << "]=CELLCENTERED)";
    }
    out << std::endl;

    for (p=attrs.begin(); p!=attrs.end(); ++p)
    {
      const void* pArray = NULL;
      if ((*p).m_numComp == 1)
      {
        out << "# Begin " << (*p).m_name << std::endl;
        COM_get_array_const((wName + '.' + (*p).m_name).c_str(), paneIds[i],
                            &pArray);
        if (dims != NULL)
        { // Structured
          SwitchOnDataType((*p).m_dataType,
                           PrintStructured((TT*)pArray, ndims, dims, ghost,
                                           (*p).m_location, out));
        }
        else
        {
          SwitchOnDataType((*p).m_dataType,
                           PrintUnstructured((TT*)pArray,
                                      ((*p).m_location != 'n' ? eTotal
                                                              : nNodes - ghost),
                                      (*p).m_location == 'p', out));
        }

      } // End if m_numComp == 1
      else
      {
        int comp;
        for (comp=1; comp<=(*p).m_numComp; ++comp)
        {
          std::ostringstream sout;
          sout << wName << '.' << comp << '-' << (*p).m_name;
          out << "# Begin " << comp << '-' << (*p).m_name << std::endl;
          COM_get_array_const(sout.str().c_str(), paneIds[i], &pArray);
          if (dims != NULL) { // Structured
            SwitchOnDataType((*p).m_dataType,
                             PrintStructured((TT*)pArray, ndims, dims, ghost,
                                             (*p).m_location, out));
          }
          else
          {
            SwitchOnDataType((*p).m_dataType,
                             PrintUnstructured((TT*)pArray,
                                      ((*p).m_location != 'n' ? eTotal
                                                              : nNodes - ghost),
                                      (*p).m_location == 'p', out));
          }

        }// End for comp

      } // End else

    } // End for attrs.begin

    if (!connInfo.empty( ) )
    {
      std::vector<ConnInfo>::iterator c = connInfo.begin();
      while (c != connInfo.end())
      {
        const int* pConn = NULL;
        COM_get_array_const((*c).m_name.c_str(), paneIds[i], &pConn);
        PrintConn(pConn, *c, elemType, out);
        ++c;
      }

    }

  } // for all panes

  // Free buffers for pane ids and attribute names
  COM_free_buffer(&paneIds);
  COM_free_buffer(&attrStr);

}

COM_EXTERN_MODULE( Rocin);

/// Remove an argument from the argument list.
inline static void remove_arg( int *argc, char ***argv, int i) {
  for ( int j=i; j<*argc-1; ++j) (*argv)[j]=(*argv)[j+1];
  --*argc;
}

/**
 * @brief Holds some global program arguments
 */
struct globarg
{
	bool useTopFile;					///< A flag to specify that a top file needs to be read.
	bool withGhost;						///< A flag that specifies that ghost nodes need to be included.
	bool readControlFile;			///< A flag that specifies that a control file needs to be read.
	std::string cntrlfile;		///< Filename of the control file.
	std::string topfile;			///< Filename for the top file.
	std::string fileregx;			///< Regular expression that describes the files that need to be read.
	std::string outputfile; 	///< The filename of the output file.

	inline void printInfo( )
	{
		std::cout << __DATE__ << std::endl;

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

		if( useTopFile )
		{
			std::cout << "Use *.top connectivity file: yes\n";
			std::cout << "Top file: " << topfile << std::endl;
		}
		else
		{
			std::cout << "Use *.top connectivity file: no\n";
		}

		std::cout << "Use ghost nodes: ";
		(withGhost)? std::cout << "yes\n" : std::cout << "no\n";

		if( outputfile == "" )
			std::cout << "Output file: standard out\n";
		else
			std::cout << "Output file: " << outputfile << std::endl;

	}

} Program;

/**
 * @brief Prints usage and examples to STDOUT.
 */
void showUsage( )
{
	std::cout << "HDF2PLT OPTIONS:\n";
	std::cout << "-regex \"{somestring}*.hdf\" : Specifies a set of hdf files\n";
	std::cout << "-top {topfile} : Specifies a top file to use to resolve the connectivity\n";
	std::cout << "-g : Enables ghost node inclusion in the output file\n";
	std::cout << "-c {cntrlfile} : Specifies a control file to use\n";
	std::cout << "-o {outputfile} : Specifies output file. Output is printed to STDOUT by default.\n";
	std::cout << "-h : Prints this help menu.\n";
	std::cout << "Examples:\n";
	std::cout << "\thdf2plt -g -regex \"fluid*.hdf\" -o output.plt\n";
	std::cout << "\thdf2plt -regex \"fluid*.hdf\" -top fluid.top -o output.plt\n";
	std::cout.flush( );
}

/**
 * @brief Parses the command line arguments.
 * @param argc	the argument counter.
 * @param argv the argument vector.
 */
void parseArguments( int argc, char **argv )
{
	Program.useTopFile 			= false;
	Program.withGhost  		  = false;
	Program.readControlFile = false;

	Program.cntrlfile = "";
	Program.topfile 	= "";
	Program.fileregx  = "";

	for( int i=1; i < argc; ++i )
	{
		if( std::strcmp( argv[ i ], "-top" ) == 0 )
		{
			Program.useTopFile = true;
			Program.topfile		 = std::string( argv[ ++i ] );
		}
		else if( std::strcmp( argv[ i ], "-g" ) == 0 )
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
 * @brief Program main
 * @param argc argument counter.
 * @param argv argument vector.
 * @return rc return code.
 * @post if rc >= 0 program terminated normally.
 */
int main(int argc, char* argv[])
{
	/* Parse the command line arguments */
	 parseArguments( argc, argv );

	 Program.printInfo( );

	/* Initialize the COM Environment */
  COM_init(&argc, &argv);

  /* Load Rocin for reading the HDF files */
  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocin, "IN");

  std::cout << "Reading top file: " << Program.topfile << "...";
  if( Program.useTopFile )
  	readTopFile( Program.topfile );
  std::cout << "[DONE]\n";

  /* Obtain function handle to the obtain attribute function */
  int IN_obtain = COM_get_function_handle("IN.obtain_attribute");

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

    int IN_read = COM_get_function_handle("IN.read_by_control_file");
    COM_call_function(IN_read,
                      Program.cntrlfile.c_str(),
                      win_in.c_str(),
                      NULL,
                      timeStr,
                      &len);

    std::cout << "[DONE]\n";

  }
  else
  {
    std::cout << "Reading HDF file(s) " << Program.fileregx << " into window " << win_in << "...";

    int IN_read = COM_get_function_handle("IN.read_window");
    COM_call_function(IN_read,
                      Program.fileregx.c_str(),
                      win_in.c_str(),
                      NULL,
                      NULL,
                      timeStr,
                      &len);

    std::cout << "[DONE]\n";

  }

  std::string win_all(win_in + ".all");
  int IN_all = COM_get_attribute_handle(win_all.c_str());

  std::cout << "Obtaining data attributes for window  " << win_in << "...";
  COM_call_function(IN_obtain, &IN_all, &IN_all);
  std::cout << "[DONE]\n";

  if( Program.outputfile == "" )
  {
    std::cout << "Writing window out to standard output " << "...";
    COM_print_window(win_in, timeStr, std::cout, Program.withGhost, Program.useTopFile );
    std::cout << "[DONE]\n";
  }
  else
  {
    std::cout << "Writing window out to " << Program.outputfile << "...";
    std::ofstream fout( Program.outputfile.c_str( ) );
    COM_print_window(win_in, timeStr, fout, Program.withGhost, Program.useTopFile );
    std::cout << "[DONE]\n";
  }

  // COM_UNLOAD_MODULE_STATIC_DYNAMIC(Rocin);

  COM_finalize();
  return 0;
}



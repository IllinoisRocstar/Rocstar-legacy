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

#include "roccom.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <cstring>

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

  if ( ndims==3) {
    for (int k=ghost; k<last[2]; ++k)
      for (int j=ghost; j<last[1]; ++j)
	for (int i=ghost; i<last[0]; ++i) {
	  out << "  " << std::scientific
              << (!pData ? (TT)-987654321
                         : (loc=='p' ? pData[0]
                                     : pData[i+j*dims[0]+k*dims[0]*dims[1]]));
	  if ((i - ghost) % 10 == 9) out << std::endl;
	}
  }
  else if (ndims==2) {
    for (int j=ghost; j<last[1]; ++j)
      for (int i=ghost; i<last[0]; ++i) {
	out << "  " << std::scientific
            << (!pData ? (TT)-987654321
                       : (loc=='p' ? pData[0] : pData[i+j*dims[0]]));
	if ((i - ghost) % 10 == 9) out << std::endl;
      }

  }
  else {
    for (int i=ghost; i<last[0]; ++i) {
      out << "  " << std::scientific
          << (!pData ? (TT)-987654321 : (loc=='p' ? pData[0] : pData[i]));
	  if ((i - ghost) % 10 == 9) out << std::endl;
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
                      std::ostream& out, bool with_ghost) {
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

  if ( nPanes>0) {
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
    if ((*p).m_location != 'n' ) {
      p = attrs.erase(p);
      continue;
    }

    if ((*p).m_numComp == 1) {
      out << ", \"" << (*p).m_name << '"';
      ++var;
    } else {
      int x;
      for (x=1; x<=(*p).m_numComp; ++x) {
        out << ", \"" << x << '-' << (*p).m_name << '"';
        ++var;
      }
    }
    ++p;
  }
  out << std::endl;

  int zone_count=0;
  // Loop through the panes to find the meshes
  for (i=0; i<nPanes; ++i) {

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

    // Obtain the connectivity tables
    int nConn;       // Number of connectivity tables
    char* connNames; // Names of connectivity tables separated by space
    COM_get_connectivities(wName.c_str(), paneIds[i], &nConn, &connNames);

    int   ndims;
    const int* dims = NULL;
    char elemType = '\0';
    int eTotal = 0;
    std::vector<ConnInfo> connInfo(nConn);
    if (nConn == 1 && strncmp(connNames, ":st", 3) == 0) { // Structured mesh
      connInfo.clear();

      name = wName + '.' + connNames;
      COM_get_size(name.c_str(), paneIds[i], &ndims, &ghost);
      if ( with_ghost) ghost=0;

      // Obtain the dimensions (must be a const array) of the pane
      COM_get_array_const(name.c_str(), paneIds[i], &dims);

      out << "I=" << dims[0] - 2 * ghost;
      if ( ndims>=2) out << ", J=" << dims[1] - 2 * ghost;
      if ( ndims>=3) out << ", K=" << dims[2] - 2 * ghost;

      out << ", ZONETYPE=ORDERED, ";
    } else { // Unstructured mesh
      // Obtain the sizes of connectivity tables
      std::istringstream sin(connNames);
      std::vector<ConnInfo>::iterator c;
      for (c=connInfo.begin(); c!=connInfo.end(); ++c) {
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

    for (p=attrs.begin(); p!=attrs.end(); ++p) {
      const void* pArray = NULL;
      if ((*p).m_numComp == 1) {
        out << "# Begin " << (*p).m_name << std::endl;
        COM_get_array_const((wName + '.' + (*p).m_name).c_str(), paneIds[i],
                            &pArray);
        if (dims != NULL) { // Structured
          SwitchOnDataType((*p).m_dataType,
                           PrintStructured((TT*)pArray, ndims, dims, ghost,
                                           (*p).m_location, out));
        } else {
          SwitchOnDataType((*p).m_dataType,
                           PrintUnstructured((TT*)pArray,
                                      ((*p).m_location != 'n' ? eTotal
                                                              : nNodes - ghost),
                                      (*p).m_location == 'p', out));
        }
      } else {
        int comp;
        for (comp=1; comp<=(*p).m_numComp; ++comp) {
          std::ostringstream sout;
          sout << wName << '.' << comp << '-' << (*p).m_name;
          out << "# Begin " << comp << '-' << (*p).m_name << std::endl;
          COM_get_array_const(sout.str().c_str(), paneIds[i], &pArray);
          if (dims != NULL) { // Structured
            SwitchOnDataType((*p).m_dataType,
                             PrintStructured((TT*)pArray, ndims, dims, ghost,
                                             (*p).m_location, out));
          } else {
            SwitchOnDataType((*p).m_dataType,
                             PrintUnstructured((TT*)pArray,
                                      ((*p).m_location != 'n' ? eTotal
                                                              : nNodes - ghost),
                                      (*p).m_location == 'p', out));
          }
        }
      }
    }

    if (!connInfo.empty()) {
      std::vector<ConnInfo>::iterator c = connInfo.begin();
      while (c != connInfo.end()) {
        const int* pConn = NULL;
        COM_get_array_const((*c).m_name.c_str(), paneIds[i], &pConn);
        PrintConn(pConn, *c, elemType, out);
        ++c;
      }
    }

    if ( COM_get_attribute_handle( (wName+".ridges").c_str()) >0) {
      ++zone_count;

      int n; COM_get_size( (wName+".ridges").c_str(), paneIds[i], &n);

      if ( n>0) {
	out << "ZONE T=\"fea" << std::setw(5) << std::setfill('0')
	    << paneIds[i] << "\", N=" << nNodes - ghost
	    << ", E=" << n << ", ZONETYPE=FELINESEG, DATAPACKING=BLOCK\n";
	out << "VARSHARELIST=([1-" << var << "]=" << zone_count << ")" << std::endl;

	int *pArray;
	COM_get_array( (wName+".ridges").c_str(), paneIds[i], &pArray);

	for (int i=0; i<n; ++i) {
	  out << "  " << pArray[i] << "  " << pArray[i+n] << std::endl;
	}

	++zone_count;
      }
    }
  }

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

int main(int argc, char* argv[])
{
  COM_init(&argc, &argv);

  bool with_ghost = (argc>1 && std::strcmp(argv[1],"-g")==0);
  if ( with_ghost) remove_arg( &argc, &argv, 1);

  bool read_control = (argc>1 && std::strcmp(argv[1],"-c")==0);
  if ( read_control) remove_arg( &argc, &argv, 1);

  if (argc < 2 || argc > 3 ) {
    std::cerr << "Usage: hdf2plt [-g] <hdf input files> [<output file>]" << std::endl;
    std::cerr << "   or: hdf2plt [-g] -c <Rocin control file> [<output file>]" << std::endl;
    std::cerr << "\nOptions:\n\t -g: Includes ghost nodes and elements in output\n"
	      << "\nExamples:\n"
	      << "\thdf2plt -g \"surfmesh*.hdf\" output.plt\n"
	      << "\thdf2plt -g -c surfmesh.txt output.plt"  << std::endl;
    return -1;
  }

  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocin, "IN");

  int IN_obtain = COM_get_function_handle("IN.obtain_attribute");

  std::string file_in(argv[1]);
  std::string win_in = file_in;

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

  if (read_control) {
    std::cerr << "Reading by control file " << file_in
	      << " into window " << win_in << std::endl;

    int IN_read = COM_get_function_handle("IN.read_by_control_file");
    COM_call_function(IN_read, file_in.c_str(), win_in.c_str(), NULL,
		      timeStr, &len);
  } else {
    std::cerr << "Reading HDF file(s) " << file_in
	      << " into window " << win_in << std::endl;

    int IN_read = COM_get_function_handle("IN.read_window");
    COM_call_function(IN_read, file_in.c_str(), win_in.c_str(), NULL, NULL,
		      timeStr, &len);
  }

  std::string win_all(win_in + ".all");
  int IN_all = COM_get_attribute_handle(win_all.c_str());

  std::cerr << "Obtaining data attributes for window  " << win_in << std::endl;
  COM_call_function(IN_obtain, &IN_all, &IN_all);

  if (argc == 2) {
    std::cerr << "Writing window out to standard output " << std::endl;
    COM_print_window(win_in, timeStr, std::cout, with_ghost);
  } else {
    std::cerr << "Writing window out to " << argv[2] << std::endl;
    std::ofstream fout(argv[2]);
    COM_print_window(win_in, timeStr, fout, with_ghost);
  }

  // COM_UNLOAD_MODULE_STATIC_DYNAMIC(Rocin);

  COM_finalize();
  return 0;
}



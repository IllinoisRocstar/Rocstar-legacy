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
 ** @file CGNS to SMF ASCII format
 ** @author: Eric Shaffer
 **/

#include "Rocin.h"
#include "roccom.h"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

// Linear cells
#define VTK_EMPTY_CELL     0
#define VTK_VERTEX         1
#define VTK_POLY_VERTEX    2
#define VTK_LINE           3
#define VTK_POLY_LINE      4
#define VTK_TRIANGLE       5
#define VTK_TRIANGLE_STRIP 6
#define VTK_POLYGON        7
#define VTK_PIXEL          8
#define VTK_QUAD           9
#define VTK_TETRA         10
#define VTK_VOXEL         11
#define VTK_HEXAHEDRON    12
#define VTK_WEDGE         13
#define VTK_PYRAMID       14
#define VTK_PENTAGONAL_PRISM 15
#define VTK_HEXAGONAL_PRISM  16

// Quadratic, isoparametric cells
#define VTK_QUADRATIC_EDGE       21
#define VTK_QUADRATIC_TRIANGLE   22
#define VTK_QUADRATIC_QUAD       23
#define VTK_QUADRATIC_TETRA      24
#define VTK_QUADRATIC_HEXAHEDRON 25
#define VTK_QUADRATIC_WEDGE      26
#define VTK_QUADRATIC_PYRAMID    27

// Special class of cells formed by convex group of points
#define VTK_CONVEX_POINT_SET 41

// Higher order cells in parametric form
#define VTK_PARAMETRIC_CURVE        51
#define VTK_PARAMETRIC_SURFACE      52
#define VTK_PARAMETRIC_TRI_SURFACE  53
#define VTK_PARAMETRIC_QUAD_SURFACE 54
#define VTK_PARAMETRIC_TETRA_REGION 55
#define VTK_PARAMETRIC_HEX_REGION   56


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

using namespace std;

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

 bool rocElement2SMF(string rtype, string & etype)
   {
     if (rtype == "t3")
       etype = "f";
     else if (rtype == "t6")
       etype = "f6";
     else if (rtype == "q4")
       etype = "q";
     else if (rtype == "q8")
       etype = "f";
     else if (rtype == "T4")
       etype = "x";
     else if (rtype == "T10")
       etype = "x10";
     else if (rtype == "H8")
       etype = "H8";
     else if (rtype == "B20")
       etype = "B20";
     else if (rtype == "P5")
       etype = "P5";
     else if (rtype == "W6")
       etype = "W6";
     else if (rtype == "P6")
       etype = "P6";
     else
       {
	 etype = "UNKNOWN";
	 return false;
       }
     return true;
   }

template <typename TT>
void PrintStructured(const TT** pData, int nComp, int ndims,
                     const int* dims_nodes, int ghost, char loc,
                     std::ostream& out)
{
  const int dims[3] = { dims_nodes[0] - (loc == 'e'),
                        ndims >= 2 ? dims_nodes[1] - (loc == 'e') : 0,
                        ndims >= 3 ? dims_nodes[2] - (loc == 'e') : 0 };
 
  const int last[3] = { dims[0] - ghost,
                        ndims >= 2 ? dims[1] - ghost : 0,
                        ndims >= 3 ? dims[2] - ghost : 0 };
 
  int c, i, j, k;
  switch (ndims) {
    case 1:
      for (i=ghost; i<last[0]; ++i) {
        for (c=0; c<nComp; ++c)
          out << (pData[c] ? pData[c][i] : (TT)-987654321) << ' ';
        out << std::endl;
      }
      break;

    case 2:
      for (j=ghost; j<last[1]; ++j)
        for (i=ghost; i<last[0]; ++i) {
          for (c=0; c<nComp; ++c)
            out << (pData[c] ? pData[c][i+j*dims[0]] : (TT)-987654321) << ' ';
          out << std::endl;
        }
      break;

    case 3:
      for (k=ghost; k<last[2]; ++k)
        for (j=ghost; j<last[1]; ++j)
          for (i=ghost; i<last[0]; ++i) {
            for (c=0; c<nComp; ++c)
              out << (pData[c] ? pData[c][i+j*dims[0]+k*dims[0]*dims[1]]
                               : (TT)-987654321) << ' ';
            out << std::endl;
          }
      break;
  }
}

template <typename TT>
void PrintUnstructured(const TT** pData, int nComp, int size, std::ostream& out)
{
  int c, i;
  for (i=0; i<size; ++i) { 
    out << "v ";
    for (c=0; c<nComp; ++c)
      out << (pData[c] ? pData[c][i] : (TT)-987654321) << ' ';
    out << std::endl;
  }
}

void PrintConn(const int* pConn, const ConnInfo& ci, std::ostream& out)
{
  std::string etype;
  int nn;
  if (ci.m_type == "q9")
    {
      nn = 8;
    }
  else 
    {
      std::istringstream sin(ci.m_type.substr(1));
      sin >> nn;
    }
  rocElement2SMF(ci.m_type,etype);

  int ne = ci.m_numElements - ci.m_numGhost;

  int elem, i;
  for (elem=0; elem<ne; ++elem) {
    out << etype;
    for (i=0; i<nn; ++i)
      out << ' ' << (pConn[elem+i*ci.m_numElements]);
      //out << ' ' << (pConn[elem+i*ci.m_numElements] - 1);
    out << std::endl;
  }
}

void COM_print_window(const std::string& wName, const std::string& timeStr,
                      const std::string& file_in, bool mesh_only,  std::ofstream & out)
{
  static std::map<int, std::string> DataTypeAsString;
  static std::map<std::string, int> COM2VTK;
  if (DataTypeAsString.empty()) {
    DataTypeAsString[COM_CHAR] = "char";
    DataTypeAsString[COM_BYTE] = "char";
    DataTypeAsString[COM_UNSIGNED_CHAR] = "unsigned_char";
    DataTypeAsString[COM_SHORT] = "short";
    DataTypeAsString[COM_UNSIGNED_SHORT] = "unsigned_short";
    DataTypeAsString[COM_INT] = "int";
    DataTypeAsString[COM_UNSIGNED] = "unsigned_int";
    DataTypeAsString[COM_LONG] = "long";
    DataTypeAsString[COM_UNSIGNED_LONG] = "unsigned_long";
    DataTypeAsString[COM_FLOAT] = "float";
    DataTypeAsString[COM_DOUBLE] = "double";
    DataTypeAsString[COM_LONG_DOUBLE] = "double";  // Not really, but...
  }

  if (COM2VTK.empty()) {
    COM2VTK["t3"] = VTK_TRIANGLE;
    COM2VTK["t6"] = VTK_QUADRATIC_TRIANGLE;
    COM2VTK["q4"] = VTK_QUAD;
    COM2VTK["q8"] = VTK_QUADRATIC_QUAD;
    COM2VTK["q9"] = VTK_QUADRATIC_QUAD;
    COM2VTK["T4"] = VTK_TETRA;
    COM2VTK["T10"] = VTK_QUADRATIC_TETRA;
    COM2VTK["B8"] = VTK_HEXAHEDRON;
    COM2VTK["H8"] = VTK_HEXAHEDRON;
    COM2VTK["B20"] = VTK_QUADRATIC_HEXAHEDRON;
    COM2VTK["P5"] = VTK_PYRAMID;
    COM2VTK["W6"] = VTK_WEDGE;
    COM2VTK["P6"] = VTK_WEDGE;
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

  // Eliminate window and pane attributes.  Count node and element attributes.
  std::set<char> locset;
  std::vector<AttrInfo>::iterator p = attrs.begin();
  ++p;
  while (p != attrs.end()) {
    char loc = (*p).m_location;
    if (loc == 'w' || loc == 'p') {
      p = attrs.erase(p);
      continue;
    }
    locset.insert(loc);
    ++p;
  }

 
  // Loop through the panes to find the meshes
  for (i=0; i<nPanes; ++i) {
   
    

    out << "# SMF Version 1.1" << std::endl;
    // out << "Material=" << wName << ", Block=" << i << ", Time: " << timeStr
    //  << "." << std::endl;
    out << "#ASCII" << std::endl;

    // Obtain the size of nodes
    int  nNodes;  // total number of nodes
    int  ghost; // Number of ghost nodes
    name = wName + ".nc";
    COM_get_size(name.c_str(), paneIds[i], &nNodes, &ghost);
    nNodes -= ghost;
      
    // Obtain the connectivity tables
    int nConn;       // Number of connectivity tables
    char* connNames; // Names of connectivity tables separated by space
    COM_get_connectivities(wName.c_str(), paneIds[i], &nConn, &connNames);

    const int* dims = NULL;
    int ndims, eTotal = 0, eSize = 0;
    std::vector<ConnInfo> connInfo(nConn);
    std::vector<std::pair<int, int> > cellTypes;
    if (nConn == 1 && strncmp(connNames, ":st", 3) == 0) { // Structured mesh
      name = wName + '.' + connNames;
      COM_get_size(name.c_str(), paneIds[i], &ndims, &ghost);

      // Obtain the dimensions (must be a const array) of the pane
      COM_get_array_const(name.c_str(), paneIds[i], &dims);

      out << "DATASET STRUCTURED_GRID" << std::endl;
      out << "DIMENSIONS " << dims[0] - 2 * ghost << " " << dims[1] - 2 * ghost
          << " " << dims[2] - 2 * ghost << std::endl;

      eTotal = (dims[0] - 2 * ghost > 1 ?  dims[0] - 2 * ghost - 1 : 1)
               * (dims[1] - 2 * ghost > 1 ?  dims[1] - 2 * ghost - 1 : 1)
               * (dims[2] - 2 * ghost > 1 ?  dims[2] - 2 * ghost - 1 : 1);
    } else { // Unstructured mesh
      out << "#DATASET UNSTRUCTURED_GRID" << std::endl;
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
        eTotal += (*c).m_numElements - (*c).m_numGhost;

        int nn;
        if ((*c).m_type == "q9")
          nn = 8;
        else {
          std::istringstream sin((*c).m_type.substr(1));
          sin >> nn;
        }
        eSize += ((*c).m_numElements - (*c).m_numGhost) * (nn + 1);

        cellTypes.push_back(std::pair<int, int>(COM2VTK[(*c).m_type],
                                         (*c).m_numElements - (*c).m_numGhost));
      }
    }

    out << "#POINTS " << nNodes << ' ' << DataTypeAsString[attrs[0].m_dataType]
        << std::endl;
    const void* pArray[9] = { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
                              NULL };
    p = attrs.begin();
    int comp;
    for (comp=1; comp<=(*p).m_numComp; ++comp) {
      std::ostringstream sout;
      sout << wName << '.' << comp << '-' << (*p).m_name;
      COM_get_array_const(sout.str().c_str(), paneIds[i], &(pArray[comp-1]));
    }

    if (dims != NULL) { // Structured
      SwitchOnDataType((*p).m_dataType,
                       PrintStructured((const TT**)pArray, (*p).m_numComp,
                                       ndims, dims, ghost, (*p).m_location,
                                       out));
    } else {
      SwitchOnDataType((*p).m_dataType,
                       PrintUnstructured((const TT**)pArray, (*p).m_numComp,
                                         nNodes, out));
    }

    if (nConn != 1 || strncmp(connNames, ":st", 3) != 0) { // Unstructured mesh
      out << "#CELLS " << eTotal << ' ' << eSize << std::endl;

      std::vector<ConnInfo>::iterator c;
      for (c=connInfo.begin(); c!=connInfo.end(); ++c) {
        const int* pConn = NULL;
        COM_get_array_const((*c).m_name.c_str(), paneIds[i], &pConn);
        PrintConn(pConn, *c, out);
      }

      out << "#CELL_TYPES " << eTotal << std::endl;

      // int i;
//       std::vector<std::pair<int, int> >::iterator t = cellTypes.begin();
//       while (t != cellTypes.end()) {
//         for (i=0; i<(*t).second; ++i)
//           out << (*t).first << std::endl;
//         ++t;
//       }
    }

    // free the buffer of connNames
    COM_free_buffer(&connNames);

    if (!mesh_only && locset.count('n') > 0) {
      out << "#POINT_DATA " << nNodes << std::endl;
      for (p=attrs.begin(); p!=attrs.end(); ++p) {
        if ((*p).m_location != 'n')
          continue;

        if ((*p).m_numComp == 1) {
          COM_get_array_const((wName + '.' + (*p).m_name).c_str(), paneIds[i],
                              &(pArray[0]));
          out << "#SCALARS ";
        } else {
          int comp;
          for (comp=1; comp<=(*p).m_numComp; ++comp) {
            std::ostringstream sout;
            sout << wName << '.' << comp << '-' << (*p).m_name;
            COM_get_array_const(sout.str().c_str(), paneIds[i],
                                &(pArray[comp-1]));
          }
          if ((*p).m_numComp == 3)
            out << "#VECTORS ";
          else
            out << "#TENSORS ";
        }

        out << (*p).m_name << ' ' << DataTypeAsString[(*p).m_dataType]
            << std::endl;

        if ((*p).m_numComp == 1)
          out << "#LOOKUP_TABLE default" << std::endl;

        if (dims != NULL) { // Structured
          SwitchOnDataType((*p).m_dataType,
                           PrintStructured((const TT**)pArray, (*p).m_numComp,
                                           ndims, dims, ghost, (*p).m_location,
                                           out));
        } else {
          SwitchOnDataType((*p).m_dataType,
                           PrintUnstructured((const TT**)pArray, (*p).m_numComp,
                                             nNodes, out));
        }
      }
    }

    if (!mesh_only && locset.count('e') > 0) {
      out << std::endl << "#CELL_DATA " << eTotal << std::endl;
      for (p=attrs.begin(); p!=attrs.end(); ++p) {
        if ((*p).m_location != 'e')
          continue;

        if ((*p).m_numComp == 1) {
          COM_get_array_const((wName + '.' + (*p).m_name).c_str(), paneIds[i],
                              &(pArray[0]));
          out << "#SCALARS ";
        } else {
          int comp;
          for (comp=1; comp<=(*p).m_numComp; ++comp) {
            std::ostringstream sout;
            sout << wName << '.' << comp << '-' << (*p).m_name;
            COM_get_array_const(sout.str().c_str(), paneIds[i],
                                &(pArray[comp-1]));
          }
          if ((*p).m_numComp == 3)
            out << "#VECTORS ";
          else
            out << "TENSORS ";
        }

        out << (*p).m_name << ' ' << DataTypeAsString[(*p).m_dataType]
            << std::endl;

        if ((*p).m_numComp == 1)
          out << "LOOKUP_TABLE default" << std::endl;

        if (dims != NULL) { // Structured
          SwitchOnDataType((*p).m_dataType,
                           PrintStructured((const TT**)pArray, (*p).m_numComp,
                                           ndims, dims, ghost, (*p).m_location,
                                           out));
        } else {
          SwitchOnDataType((*p).m_dataType,
                           PrintUnstructured((const TT**)pArray, (*p).m_numComp,
                                             eTotal, out));
        }
      }
    }
  }

  // Free buffers for pane ids and attribute names
  COM_free_buffer(&paneIds);
  COM_free_buffer(&attrStr);
}

COM_EXTERN_MODULE( Rocin);

int main(int argc, char* argv[])
{
  COM_init(&argc, &argv);
  bool fail = false;
  bool mesh_only = false;
  //bool read_control = false;
  //int i;
  if (std::strcmp(argv[1], "-meshonly") == 0)
    {
      mesh_only = true;
      if (argc != 4)
	fail = true;
    }
  else 
    {
      if (argc != 3)
	fail = true;
    }
  
  if (fail)
    {
      std::cerr << "Usage: cgns2smf [-meshonly] <cgns file> <smf file>" << std::endl;
      return 1;
    }

  ofstream outfile(argv[argc-1], ios::out);
  COM_LOAD_MODULE_STATIC_DYNAMIC(Rocin, "IN");
  
  int IN_obtain = COM_get_function_handle("IN.obtain_attribute");

  COM_set_verbose(0);
  COM_set_profiling(0);

  std::string file_in(argv[argc-2]);
  std::string win_in(file_in);
  std::string::size_type st = win_in.find_last_of('/');
  if (st != std::string::npos)
    win_in.erase(0, st+1);
  st = win_in.find_first_not_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
  if (st != std::string::npos)
    win_in.erase(st);

  int  len = 15;
  char timeStr[16] = "";

  
  std::cout << "Reading CGNS file(s) " << file_in
	    << " into window " << win_in << std::endl;
  
  int IN_read = COM_get_function_handle("IN.read_window");
  COM_call_function(IN_read, file_in.c_str(), win_in.c_str(), NULL, NULL,
		    timeStr, &len);
  
  cerr << "Finished reading CGNS file into window\n";
  int IN_all = COM_get_attribute_handle((win_in + ".all").c_str());
  COM_call_function(IN_obtain, &IN_all, &IN_all);

  std::cout << "Printing surface mesh to..." << argv[argc-1] << std::endl;
  COM_print_window(win_in, timeStr, file_in, mesh_only,outfile);
  std::cout << "Done" << std::endl;

#ifdef DUMMY_MPI
  COM_UNLOAD_MODULE_STATIC_DYNAMIC(Rocin,"IN");
#endif // DUMMY_MPI

  COM_finalize();
  return 0;
}







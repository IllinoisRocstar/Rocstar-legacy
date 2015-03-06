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
// $Id: rocin_block.h,v 1.7 2008/12/06 08:43:19 mtcampbe Exp $

#ifndef _ROCIN_BLOCK_H_
#define _ROCIN_BLOCK_H_

#include <map>

/**
 ** Struct containing necessary information about an attribute in a window.
 **/
struct VarInfo_HDF4 {
   /// Constructor for quick initialization.
  VarInfo_HDF4(const std::string &name, char pos, COM_Type dType, 
	       const std::string& units, int nc, int32 i, int nitems, 
	       int ng, bool is_null)
  : m_name(name), m_position(pos), m_dataType(dType), m_units(units), 
    m_indices(nc, i), m_nitems( nitems), m_ng(ng), m_is_null(nc,is_null)
  {}

  std::string m_name;            ///< Name of variable
  char m_position;              ///< Location, 'w', 'p', 'n', or 'e'.
  COM_Type m_dataType;          ///< Roccom datatype.
  std::string m_units;          ///< Units of measurement.
  std::vector<int32> m_indices; ///< HDF4 dataset indices for each component.
  int m_nitems;                 ///< Total number of items
  int m_ng;                     ///< Number of ghost items
  std::vector<bool>  m_is_null; ///< Whether or not a component in NULL.
};

/**
 ** Struct containing necessary information on a mesh of a pane.
 **/
struct GridInfo_HDF4 {
   /// Constructor for quick initialization.  Use for structured grids.
   inline GridInfo_HDF4(int32* size, int ng)
   : m_name(":st0:"),
     m_numElements(size[2] - 1
                   * std::max(int32(1), (size[1] - 1))
                   * std::max(int32(1), (size[0] - 1))),
     m_numGhostElements(ng), m_index(FAIL)
   { m_name[3] += (size[1] > 1 ? (size[0] > 1 ? 3 : 2) : 1);
     m_size[0] = size[0]; m_size[1] = size[1]; m_size[2] = size[2]; }

   /// Constructor for quick initialization.  Use for unstructured grids.
   inline GridInfo_HDF4(std::string name, int ne, int ng, int32 i)
   : m_name(name), m_numElements(ne), m_numGhostElements(ng), m_index(i)
   { m_size[0] = 0; m_size[1] = 0; m_size[2] = 0; }

   int32 m_size[3];        ///< Grid dimensions (structured only).
   std::string m_name;     ///< Grid name ":st?:*", ":t3:*", ":B8:*", etc.
   int m_numElements;      ///< Number of elements in a mesh.
   int m_numGhostElements; ///< Number of ghost elements in a mesh.
   int32 m_index;          ///< HDF4 dataset index of conn table (unstr only).
};

/**
 ** Struct containing necessary information for a pane.
 **/
struct Block_HDF4 {
   /// Constructor for fast initialization.
   inline Block_HDF4(const std::string& file, const std::string& geomFile,
                     int32 indices[], int paneId, const std::string& time,
		     const std::string& units, int numNodes, int ghostNodes)
   : m_file(file), m_geomFile(geomFile), m_paneId(paneId), time_level(time),
     m_units(units), m_numNodes(numNodes), m_numGhostNodes(ghostNodes)
   { m_indices[0] = indices[0]; m_indices[1] = indices[1];
     m_indices[2] = indices[2]; }

   std::string m_file;     ///< Data file.
   std::string m_geomFile; ///< External geometry file (may be empty).
   int32 m_indices[3];     ///< HDF4 dataset indices of the nodal coordinates.
   int m_paneId;           ///< The pane id.
   std::string time_level; ///< The dataset's time stamp.
   std::string m_units;    ///< The mesh's units of measurement.
   int m_numNodes;         ///< Number of nodes in the mesh.
   int m_numGhostNodes;    ///< Number of ghost nodes in a mesh.
   std::vector<GridInfo_HDF4> m_gridInfo; ///< Dimensions or conn table(s)
   std::vector<VarInfo_HDF4>  m_variables; ///< Info on each variable.
};
typedef std::multimap<std::string, Block_HDF4*> BlockMM_HDF4;

#ifdef USE_CGNS

/**
 ** Struct containing necessary information about an attribute in a window.
 **/
struct VarInfo_CGNS {
   /// Constructor for quick initialization.
  VarInfo_CGNS(const std::string &name, char pos, COM_Type dType, 
	       const std::string& units, int nc, int i, int nitems, 
	       int ng, bool is_null)
  : m_name(name), m_position(pos), m_dataType(dType), m_units(units), 
    m_indices(nc, i), m_nitems(nitems), m_ng(ng), m_is_null(nc,is_null)
  {}

  std::string m_name;          ///< Name of variable
  char m_position;             ///< Location, 'w', 'p', 'n', or 'e'.
  COM_Type m_dataType;         ///< Roccom datatype.
  std::string m_units;         ///< Units of measurement.
  std::vector<int> m_indices;  ///< CGNS array indices for each component.
  int m_nitems;                ///< Total number of items
  int m_ng;                    ///< Number of ghost items
  std::vector<bool> m_is_null; ///< Whether or not a component in NULL.
};

/**
 ** Struct containing necessary information on a mesh of a pane.
 **/
struct GridInfo_CGNS {
   /// Constructor for quick initialization.  Use for structured grids.
   inline GridInfo_CGNS(int* size, int cellDim, int ng)
   : m_name(":st0:"), m_numElements(size[cellDim]),
     m_numGhostElements(ng)
   { m_name[3] += cellDim; m_size[0] = size[0]; m_size[1] = m_size[2] = 1;
     if (cellDim > 1) { m_size[1] = size[1]; m_numElements *= size[cellDim+1];
       if (cellDim > 2) { m_size[2] = size[2]; m_numElements *= size[5]; }
   } }

   /// Constructor for quick initialization.  Use for unstructured grids.
   inline GridInfo_CGNS(std::string name, int ne, int ng)
   : m_name(name), m_numElements(ne), m_numGhostElements(ng)
   { m_size[0] = 0; m_size[1] = 0; m_size[2] = 0; }

   int m_size[3];          ///< Grid dimensions (structured only).
   std::string m_name;     ///< Grid name ":st?:*", ":t3:*", ":B8:*", etc.
   int m_numElements;      ///< Number of elements in a mesh.
   int m_numGhostElements; ///< Number of ghost elements in a mesh.
   // We don't really need to save the Element index, since we use all of
   // them in the Zone.
};

/**
 ** Struct containing necessary information for a pane.
 **/
struct Block_CGNS {
   /// Constructor for fast initialization.
   inline Block_CGNS(const std::string& file, int B, int Z, int G, int paneId,
                     const std::string& time, const std::string& units,
                     int numNodes, int ghostNodes)
   : m_file(file), m_B(B), m_Z(Z), m_G(G), m_W(0), m_P(0), m_C(0), m_E(0),
     m_N(0), m_paneId(paneId), time_level(time), m_units(units),
     m_numNodes(numNodes), m_numGhostNodes(ghostNodes)
   {}

   std::string m_file;     ///< Data file.
   int m_B;                ///< Base index.
   int m_Z;                ///< Zone index.
   int m_G;                ///< GridCoordinates index.
   int m_W;                ///< IntegralData index for window attrs.
   int m_P;                ///< IntegralData index for pane attrs.
   int m_C;                ///< IntegralData index for conn attrs.
   int m_E;                ///< FlowSolution index for element attrs.
   int m_N;                ///< FlowSolution index for node attrs.
   int m_paneId;           ///< The pane id.
   std::string time_level; ///< The dataset's time stamp.
   std::string m_units;    ///< The mesh's units of measurement.
   int m_numNodes;         ///< Number of nodes in the mesh.
   int m_numGhostNodes;    ///< Number of ghost nodes in a mesh.
   std::vector<GridInfo_CGNS> m_gridInfo; ///< Dimensions or connectivity table.
   std::vector<VarInfo_CGNS>  m_variables; ///< Info on each variable.
};
typedef std::multimap<std::string, Block_CGNS*> BlockMM_CGNS;

#endif // USE_CGNS

#endif







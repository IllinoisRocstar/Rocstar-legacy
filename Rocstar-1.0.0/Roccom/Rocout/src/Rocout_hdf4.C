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
/** \file Rocout_hdf4.C
 *  Implementation of Rocout HDF4 routines.
 */
/*  Author John Norris
 *  Initial date:   May 17, 2004
 */

#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>
#include <cstring>
#include <algorithm>
#include <strings.h>

#include "Rocout.h"
#include "HDF4.h"

#ifndef DOXYGEN_SHOULD_SKIP_THIS
USE_COM_NAME_SPACE
#endif

// #define DEBUG_DUMP_PREFIX "/turing/home/jnorris/rocout/"
// #define DEBUG_MSG(x) std::cout << "ROCOUT DEBUG: " << __LINE__ << ": " << x << std::endl
#define DEBUG_MSG(x)


#ifdef DEBUG_DUMP_PREFIX
static std::string s_material;
static std::string s_timeLevel;
static std::ofstream* s_fout = NULL;
#endif // DEBUG_DUMP_PREFIX

static void io_pane(const char* fname, const COM::Pane* pane,
                    const COM::Attribute* attr, const char* material,
                    const char* timelevel, const char* mfile,
                    const std::string& errorhandle, const int mode);

static void io_pane_header(const char* fname, const COM::Pane* pane,
                           const char* blockname, const char* material,
                           const char* timelevel, const char* mfile,
                           const std::string& errorhandle, const int mode);

static void io_pane_coordinates(const char* fname, const COM::Pane* pane,
                                const char* timelevel, const char* coordsys,
                                const char* unit,
                                const std::string& errorhandle, const int mode);

static void io_pane_connectivity(const char* fname, const COM::Pane* pane,
                                 const char* timelevel, const char* coordsys,
                                 const std::string& errorhandle,
                                 const int mode);

static void io_pane_attribute(const char* fname, const COM::Pane* pane,
                              const COM::Attribute* attr, const char* timelevel,
                              const char* coordsys,
                              const std::string& errorhandle, const int mode);

static void io_hdf_data(const char* fname, const char* label, const char* units,
                        const char* format, const char* coordsys, int rank,
                        int shape[], int ng1, int ng2, int dim,
                        const COM_Type type, const void* p, int stride,
                        const std::string& errorhandle, const int mode,
                        const void* minv, const void* maxv);

static void min_element(const void* begin, const int rank, const int shape[],
                        const int ng1, const int ng2, const COM_Type type,
                        void* f);

static void max_element(const void* begin, const int rank, const int shape[],
                        const int ng1, const int ng2, const COM_Type type,
                        void* f);

static void squared_sum(const void* a, const int length, const int stride,
                        const COM_Type type, void* ssum);

static void min_sqrt_element(const void* begin, const int rank,
                             const int shape[], const int ng1, const int ng2,
                             const COM_Type type, void* v);

static void max_sqrt_element(const void* begin, const int rank,
                             const int shape[], const int ng1, const int ng2,
                             const COM_Type type, void* v);

/*
static void hdf_error_message(const char* s, int i,
                              const std::string& errorhandle);
 */

static int write_data(const char* fname, const char* label, const char* units,
                      const char* format, const char* coordsys, const int _rank,
                      const int _shape[], const int ng1, const int ng2,
                      const COM_Type type, const void* p, const void* minv,
                      const void* maxv, const std::string& errorhandle,
                      const int mode = 1);

static int comtype2hdftype(COM_Type i);

inline void append_str(std::vector<char>& vec, const char* str)
{
  vec.insert(vec.end(), str, str + std::strlen(str));
}

void write_attr_HDF4(const std::string& fname, const std::string& mfile,
                     const COM::Attribute* attr, const char* material,
                     const char* timelevel, int pane_id,
                     const std::string& errorhandle, int mode)
{
  const Window* w = attr->window();
  COM_assertion(w != NULL);
  const Pane& pn = w->pane(pane_id);
  io_pane(fname.c_str(), &pn, attr, material, timelevel,
          !mfile.empty() ? mfile.c_str() : NULL, errorhandle, mode);
}

static void io_pane(const char* fname, const COM::Pane* pane,
                    const COM::Attribute* attr, const char* material,
                    const char* timelevel, const char* mfile,
                    const std::string& errorhandle, const int mode)
{
  char buf[20];
  std::sprintf(buf, "%04d", pane->id());
  std::string blockname = buf;

  std::string coordsys(material);
  coordsys.append("|");
  coordsys.append(blockname);

  bool with_mesh = mfile == NULL || std::strlen(mfile) == 0 || 
    attr->id() == COM::COM_MESH || attr->id() == COM::COM_PMESH || 
    attr->id() == COM::COM_ALL;

#ifdef DEBUG_DUMP_PREFIX
  s_material = material;
  {
    double f;
    std::istringstream in(timelevel);
    in >> f;
    std::ostringstream out;
    out << f;
    s_timeLevel = out.str();
  }
#endif // DEBUG_DUMP_PREFIX

  // Write the pane header only when writing the mesh, or coordinates, 
  // or if mfile is present but not the same with fname.
  if ( with_mesh || attr->id() == COM::COM_NC || 
       mfile && std::strcmp( fname, mfile) )
    io_pane_header(fname, pane, blockname.c_str(), material, 
		   timelevel, mfile, errorhandle, mode);

  if (with_mesh) {
    COM_assertion_msg( attr->id() != COM_NC && attr->id() != COM_CONN &&
		       attr->id() != COM_PCONN, 
		       "Must not write mesh along with nc, conn or pconn");

    // Write out coordinates
    io_pane_coordinates( fname, pane, timelevel, coordsys.c_str(), 
			 pane->attribute(COM::COM_NC)->unit().c_str(),
                         errorhandle, mode);

    // Write out connectivity
    io_pane_connectivity( fname, pane, timelevel, coordsys.c_str(),
                          errorhandle, mode);

    // Write out ridges
    io_pane_attribute( fname, pane, pane->attribute(COM::COM_RIDGES), 
		       timelevel, NULL, errorhandle, mode);
    if (attr->id() == COM::COM_MESH) return;

    // Write out pane connectivity
    io_pane_attribute( fname, pane, pane->attribute(COM::COM_PCONN), 
		       timelevel, NULL, errorhandle, mode);
    if (attr->id() == COM::COM_PMESH) return;
  }

  if ( attr->id() == COM::COM_CONN) {
    // Write out connectivity
    io_pane_connectivity( fname, pane, timelevel, coordsys.c_str(),
                          errorhandle, mode);
    if (attr->id() == COM::COM_CONN || attr->id() == COM::COM_MESH) return;
  }
  else if ( attr->id() == COM::COM_ALL || attr->id() == COM::COM_ATTS) {
    std::vector<const Attribute*> attrs;
    pane->attributes(attrs);
    std::vector<const Attribute*>::const_iterator it;
    for (it=attrs.begin(); it!=attrs.end(); ++it) {
      io_pane_attribute(fname, pane, *it, timelevel, NULL, errorhandle, mode);
    }
  } else {
    // Call io_pane_attribute on the attribute in the given pane.
    io_pane_attribute( fname, pane, pane->attribute(attr->id()), 
		       timelevel, NULL, errorhandle, mode);
  }
}

static void io_pane_header(const char* fname, const COM::Pane* pane,
                           const char* blockname, const char* material,
                           const char* timelevel, const char* mfile,
                           const std::string& errorhandle, const int mode)
{
  // Mesh description array
  int mesh_type;
  if (pane->is_structured())
    mesh_type = 2;   // 2 for structured mesh
  else if (pane->dimension() == 2)
    mesh_type = 5;   // 5 for surface meshes
  else
    mesh_type = 3;   // 3 for unstructured volume mesh

  // The header now supports the new format.
  std::ostringstream sout;
  if (mesh_type == 2) {
    sout << mesh_type << '|';
    COM_assertion(pane->size_of_nodes());
    sout << pane->size_of_ghost_layers();
  }
  else {
    std::vector<const Connectivity*> elems;
    pane->connectivities(elems);
    sout << mesh_type << '|';
    if (pane->size_of_nodes() == 0) // Add one dummy node
      sout << 1;
    else
      sout << pane->size_of_ghost_nodes();

    // Loop through the number of connectivity tables
    std::vector<const Connectivity*>::const_iterator it;
    for (it=elems.begin(); it!=elems.end(); ++it) {
      // Write out connectivity for the mesh
      if ((*it)->size_of_items() == 0)
        sout << ',' << 1; // Set the number of ghost (dummy) elements to 1
      else
        sout << ',' << (*it)->size_of_ghost_elements();
    }
  }

  if (mfile && std::strlen(mfile) != 0 && std::strcmp(fname, mfile) != 0)
    sout << '|' << mfile;
  std::string s = sout.str();

  // Write out the head
  int shape[2];
  shape[0] = s.size() + 1;

  write_data(fname, blockname, timelevel, "block header", material,
             1, shape, 0, 0, COM_CHAR, s.c_str(), NULL, NULL, errorhandle,
             mode);
}

static void io_pane_coordinates(const char* fname, const COM::Pane* pane,
                                const char* timelevel, const char* coordsys,
                                const char* unit,
                                const std::string& errorhandle, const int mode)
{
#ifdef DEBUG_DUMP_PREFIX
  s_fout = new std::ofstream((DEBUG_DUMP_PREFIX + s_material + ".nc_" + s_timeLevel + ".hdf").c_str(), std::ios_base::app);
#endif // DEBUG_DUMP_PREFIX
  int ncomp=pane->attribute(COM::COM_NC)->size_of_components();
  for ( int i=COM::COM_NC1; i<COM::COM_NC1+ncomp; ++i) {
    io_pane_attribute(fname, pane, pane->attribute(i),
		      timelevel, coordsys, errorhandle, mode);
  }
#ifdef DEBUG_DUMP_PREFIX
  delete s_fout;
  s_fout = NULL;
#endif // DEBUG_DUMP_PREFIX
}

static void io_pane_connectivity(const char* fname, const COM::Pane* pane,
                                 const char* timelevel, const char* coordsys,
                                 const std::string& errorhandle, const int mode)
{
  if (!pane->is_unstructured())
    return;
  // Only unstructured mesh has connectivity tables.

  std::vector<const Connectivity*> elems;
  pane->connectivities(elems);
  std::vector<int> conn;
  int shape[2];

  std::vector<const Connectivity*>::const_iterator it;
  for (it=elems.begin(); it!=elems.end(); ++it) {
    // Write out connectivity for the mesh
    // Defined as const to avoid error when calling pointer on immutable array

    const int* e = (*it)->pointer();
    shape[0] = (*it)->size_of_nodes_pe();
    shape[1] = (*it)->size_of_items();

    conn.resize(shape[0] * shape[1]);
    const int length = (*it)->capacity();
    bool is_staggered = ((*it)->stride() == 1);
    int maxv, minv;

    // Encode size info in the long name
    std::ostringstream sout;
    sout << (*it)->name() << "|p";
    if (shape[1])
      // Followed by number of ghosts
      sout << ',' << (*it)->size_of_ghost_items();
    else 
      sout << "00";           // Followed by 0
    sout << " AT TIME " << timelevel;
    std::string label = sout.str();

    if (shape[1]) { 
      // Permute the connectivity array
      for (int i=0; i<shape[1]; ++i){
	for (int j=0; j<shape[0]; ++j) {
	  conn[j*shape[1]+i] = e[is_staggered?j*length+i:i*shape[0]+j];
        }
      }
      minv = 1;
      maxv = pane->size_of_nodes();
    }
    else { // Create a dummy element
      shape[1] = 1;
      conn.clear();
      conn.resize(shape[0], 0);
      maxv = minv = 0;
    }

    // Initialize attribute name
    std::string str = (*it)->name();

    // Perform IO
    io_hdf_data(fname, label.c_str(), "", str.c_str(), coordsys, 2, shape, 
                0, 0, 1, COM_INT, &conn[0], 1, errorhandle, mode, &minv, &maxv);
  }
}

static void io_pane_attribute(const char* fname, const COM::Pane* pane,
                              const COM::Attribute* attr, const char* timelevel,
                              const char* coordsys,
                              const std::string& errorhandle, const int mode)
{
  COM_assertion(attr);
#ifdef DEBUG_DUMP_PREFIX
  bool alreadyOpen = (s_fout != NULL);
  if (!alreadyOpen) {
    s_fout = new std::ofstream((DEBUG_DUMP_PREFIX + s_material + '.' + attr->name() + '_' + s_timeLevel + ".hdf").c_str(), std::ios_base::app);
  }
#endif // DEBUG_DUMP_PREFIX
  int ncomp = attr->size_of_components();

  int rank, shape[3], num_items, ng1 = 0, ng2 = 0;
  if (attr->is_nodal()) {
    if (pane->is_unstructured()) {
      rank = 1;
      shape[0] = num_items = pane->size_of_nodes();
      ng2 = pane->size_of_ghost_nodes();
      shape[1] = shape[2] = 1;
    } else {
      rank = 3;
      ng1 = ng2 = pane->size_of_ghost_layers();
      shape[0] = pane->size_k(); // We need to reverse the order
      shape[1] = pane->size_j();
      shape[2] = pane->size_i();
      num_items = shape[0] * shape[1] * shape[2];
    }
  }
  else if (attr->is_elemental()) {
    if (pane->is_unstructured()) {
      rank = 1;
      shape[0] = num_items = pane->size_of_elements();
      ng2 = pane->size_of_ghost_elements();
      shape[1] = shape[2] = 1;
    } else {
      rank = 3;
      ng1 = ng2 = pane->size_of_ghost_layers();
      shape[0] = pane->size_k() - 1;
      shape[1] = pane->size_j() - 1;
      shape[2] = pane->size_i() - 1;
      if (shape[0] == 0) shape[0] = 1;
      num_items = shape[0] * shape[1] * shape[2];
    }
  } else if (attr->data_type() != COM_VOID
             && attr->data_type() != COM_F90POINTER) {
    rank = 1;
    shape[0] = num_items = attr->size_of_items();
    ng2 = attr->size_of_ghost_items();
    shape[1] = shape[2] = 1;
  } else { // skip windowed attributes that are pointers
    return; 
  }

  // Create a buffer for storing sqrt of values and 
  // for placeholder for empty attributes.
  int sizeof_type = COM::Attribute::get_sizeof(attr->data_type(), 1);
  std::vector<char> buf(std::max(num_items, 1) * sizeof_type);
  std::fill(buf.begin(), buf.end(), 0);

  double t1, t2; // Buffer for storing the min and max
  void* minv = NULL;
  void* maxv = NULL;

  bool is_vector3 = ncomp==3 && (attr->is_nodal() || attr->is_elemental());
  bool is_tensor9 = ncomp==9 && (attr->is_nodal() || attr->is_elemental());
  
  // Compute the range for vector and tensers
  if (mode >= 0 && (is_vector3 || is_tensor9)) {
    void* begin = &buf[0];
    for (int i=0; i<ncomp; ++i) {
      const Attribute* pa = pane->attribute(attr->id() + i + 1);
      if (pa->pointer()) 
        squared_sum(pa->pointer(), num_items, pa->stride(), 
                    attr->data_type(), &buf[0]);
      else {
        begin = NULL;
        break;
      }
    }

    minv = &t1;
    min_sqrt_element(begin, rank, shape, ng1, ng2, attr->data_type(), &t1);
    maxv = &t2;
    max_sqrt_element(begin, rank, shape, ng1, ng2, attr->data_type(), &t2);
  }

  // Initialize unit and attribute name
  std::string unit = attr->unit();

  // Initialize label
  std::ostringstream sout;
  if (is_vector3)
    sout << "x-";
  else if (is_tensor9)
    sout << "xx-";

  // Append the name and location of the attribute
  std::string a_name = attr->name();
  sout << a_name << '|' << attr->location();
  int li0 = sout.str().size();
  sout << ',' << ng2 << " AT TIME " << timelevel;

  DEBUG_MSG("Preparing to write attribute '" << attr->name() << "' (ncomp == "
            << ncomp << ')');
  // Perform IO the data
  for (int i=0; i<ncomp; ++i) {
    int label_index = li0;
    const Attribute* pa = pane->attribute(attr->id() + i + (ncomp>1));

    DEBUG_MSG("Preparing to write attribute '" << pa->name() << '\'');
    // Set label
    std::string label = sout.str();
    if ( is_vector3 || is_tensor9) {
      if ( is_vector3)
	label[0] = 'x'+i;
      else {
	label[0] = 'x'+ i / 3;
	label[1] = 'x' + i % 3;
      }
    }
    else if ( ncomp>1) { // Use <d>-attr as the attribute name.
      a_name = pa->name();
      COM_assertion( a_name.find('-')!=a_name.npos);
      label_index += a_name.find('-') + 1;
      label = a_name.substr(0,a_name.find('-')+1)+sout.str();
    }

    // Special handling for coordinates.	 
    if (attr->id() >= COM_NC1 && attr->id() <= COM_NC3) {	 
      label[0] = label[0] + ('x' - '1');
      a_name = "";	 
    }

    if (num_items == 0) {
      shape[0] = shape[rank-1] = 1;
      label[label_index] = '0';
#ifdef DEBUG_DUMP_PREFIX
      delete s_fout;
      s_fout = NULL;
#endif // DEBUG_DUMP_PREFIX
    } else if (pa->pointer() == NULL) {
      label[label_index] = '@'; 
#ifdef DEBUG_DUMP_PREFIX
      delete s_fout;
      s_fout = NULL;
#endif // DEBUG_DUMP_PREFIX
    } else {
      label[label_index] = ','; 
    }

    // Set addr and strd
    const void* addr = pa->pointer();
    int strd = pa->stride();
    if (addr == NULL) {
      addr = &buf[0];
      strd = 1;
    }

    DEBUG_MSG("fname == " << fname << ", label == '" << label << "', unit == '" << unit << "', ng1 == " << ng1 << ", ng2 == " << ng2);
    io_hdf_data(fname, label.c_str(), unit.c_str(), a_name.c_str(), 
		coordsys, rank, shape, ng1, ng2, 1, attr->data_type(), 
		addr, strd, errorhandle, mode, minv, maxv);
  }
#ifdef DEBUG_DUMP_PREFIX
  if (!alreadyOpen) {
    delete s_fout;
    s_fout = NULL;
  }
#endif // DEBUG_DUMP_PREFIX
}


static void io_hdf_data(const char* fname, const char* label, const char* units,
                        const char* format, const char* coordsys, int rank,
                        int shape[], int ng1, int ng2, int dim,
                        const COM_Type type, const void* p, int stride,
                        const std::string& errorhandle, const int mode,
                        const void* minv, const void* maxv)
{
  int length = shape[0];
  for (int i=1; i<rank; ++i)
    length *= shape[i];
  COM_assertion(length && p);

  if (dim == 1) {
    char crd[3];

    if (coordsys == NULL) {
      const char* cend = std::strchr(label, '-');

      if (cend && label[0] >= 'x' && label[0] <='z') {
        int n = cend-label;
        COM_assertion(n < 3);
        for (int i=0; i<n; ++i)
          crd[i] = label[i] + ('1' - 'x');
        crd[n] = 0;
      } else
        strcpy(crd, "0");
      coordsys = crd;
    }

    if (stride > 1) {
      int s = COM::Attribute::get_sizeof(type, 1);
      std::vector<char> w(s * length);
      for (int i=0; i<length; ++i) 
        std::memcpy(&w[i*s], &((const char*)p)[i*stride*s], s);

      write_data(fname, label, units, format, coordsys, 
                 rank, shape, ng1, ng2, type, &w[0], minv, maxv, errorhandle);
    } else {
      write_data(fname, label, units, format, coordsys, 
                 rank, shape, ng1, ng2, type, p, minv, maxv, errorhandle);
    }
  } else {
    COM_assertion(stride == 1);
    int s = COM::Attribute::get_sizeof(type, 1);

    std::vector<char> coors(coordsys ? std::strlen(coordsys) + 3 : 3);
    std::vector<char> l;
    std::vector<char> fmt;
    std::vector<char> w(s * length);
    l.reserve(20);
    fmt.reserve(std::strlen(format) + 1);

    for (int k=0; k<dim; ++k) {
      l.clear();
      if (coordsys)
        std::strcpy(&coors[0], coordsys);
      fmt.clear();
      append_str(fmt, format);

      if (dim == 3) {
        l.push_back('x' + k);
        if (label != NULL) {
          l.push_back('-');
          append_str(l, label);
        }

        if (coordsys == NULL) {
          coors[0] = '1' + k;
          coors[1] = 0;
        }
      } else if (dim == 9) {
        l.resize(2);
        l[0] = 'x' + k / 3;
        l[1] = 'x' + k % 3;

        if (label != NULL) {
          l.push_back('-');
          append_str(l, label);
        }

        if (coordsys == NULL) {
          coors[0] = '1' + k / 3;
          coors[1] = '1' + k % 3;
          coors[2]=0;
        }
      } else if (label != NULL) { 
        std::vector<char> buf(std::strlen(label) + 5);
        std::sprintf(&buf[0], "%d-%s", k + 1, label);
        l = buf;

        const char* cend = std::strchr(&buf[0], ' ');
        if (cend) {
          fmt.clear();
          fmt.insert(fmt.end(), (const char*)&buf[0], cend);
        } else
          fmt = buf;

        std::strcpy(&coors[0], "0");
      } else {
        l = fmt;
        COM_assertion(coordsys);
      }

      l.push_back(0);
      fmt.push_back(0);

      for (int i=0; i<length; ++i) 
        std::memcpy(&w[i*s], &((const char*)p)[(i*dim+k)*s], s);

      write_data(fname, &l[0], units, &fmt[0], &coors[0], 
                 rank, shape, ng1, ng2, type, &w[0], minv, maxv, errorhandle);
    }
  }
}

/// Template implementation for determining the minimum entry in an array.
/// The array can contain ghost layers and
/// the entries in the ghost layers are omitted.
template <typename T>
const T* min_element__(const T* begin, const int rank, const int shape[],
                       const int ng1, const int ng2)
{
  if (begin == NULL)
    return NULL;

  if (ng1 == 0 && ng2 == 0) {
    int size = shape[0];
    for (int i=1; i<rank; ++i)
      size *= shape[i];
    return std::min_element(begin, begin + size);
  }

  const T* t = begin;
  const T* result = NULL;
  for (int i=0; i<shape[0]-ng2; ++i) {
    for (int j=0; j<(rank>1?shape[1]:1); ++j) {
      for (int k=0; k<(rank>2?shape[2]:1); ++k, ++t) {
        if (i >= std::min(ng1, shape[0] - 1) && j >= std::min(ng1, shape[1] - 1)
            && j < std::max(1, shape[1] - ng2)
            && k >= std::min(ng1, shape[2] - 1)
            && k < std::max(1, shape[2] - ng2)) {
          if (result == NULL || *result > *t)
            result = t;
        }
      }
    }
  }
  return result;
}

/// Template implementation for determining the maximum entry in an array.
/// The array can contain ghost layers and
/// the entries in the ghost layers are omitted.
template <typename T>
const T* max_element__(const T* begin, const int rank, const int shape[],
                       const int ng1, const int ng2)
{
  if (begin == NULL)
    return NULL;

  if (ng1 == 0 && ng2 == 0) {
    int size = shape[0];
    for (int i=1; i<rank; ++i)
      size *= shape[i];
    return std::max_element(begin, begin + size);
  }
  const T* t = begin;
  const T* result = NULL;
  for (int i=0; i<shape[0]-ng2; ++i) {
    for (int j=0; j<(rank>1?shape[1]:1); ++j) {
      for (int k=0; k<(rank>2?shape[2]:1); ++k, ++t) {
        if (i >= std::min(ng1, shape[0] - 1) && j >= std::min(ng1, shape[1] - 1)
            && j < std::max(1, shape[1] - ng2)
            &&  k >= std::min(ng1, shape[2] - 1)
            && k < std::max(1, shape[2] - ng2)) {
          if (result == NULL || *result < *t)
            result = t;
        }
      }
    }
  }
  return result;
}

#ifndef HUGE_VALF
#define HUGE_VALF 1e+36F
#endif

static void min_element(const void* begin, const int rank, const int shape[],
                        const int ng1, const int ng2, const COM_Type type,
                        void* f)
{
  switch (type) {
    case COM_CHAR:
    case COM_CHARACTER: {
      const void* p = min_element__((const char*)begin, rank, shape, ng1, ng2);
      *(char*)f = p ? *(const char*)p : '\0';
      return;
    }

    case COM_INT:
    case COM_INTEGER: {
      const void* p = min_element__((const int*)begin, rank, shape, ng1, ng2);
      *(int*)f = p ? *(const int*)p : 0xEFFFFFFF;
      return;
    }

    case COM_FLOAT: 
    case COM_REAL: {
      const void* p = min_element__((const float*)begin, rank, shape, ng1, ng2);
      *(float*)f = p ? *(const float*)p : HUGE_VALF;
      return;
    }

    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:  {
      const void* p = min_element__((const double*)begin, rank, shape, ng1,
                                    ng2);
      *(double*)f = p ? *(const double*)p : HUGE_VAL;
      return;
    }

    default:
      COM_assertion(false);
      return;
  }
}

static void max_element(const void* begin, const int rank, const int shape[],
                        const int ng1, const int ng2, const COM_Type type,
                        void* f)
{
  switch (type) {
    case COM_CHAR:
    case COM_CHARACTER: {
      const void* p = max_element__((const char*)begin, rank, shape, ng1, ng2);
      *(char*)f = p ? *(const char*)p : '\0';
      return;
    }

    case COM_INT:
    case COM_INTEGER: {
      const void* p = max_element__((const int*)begin, rank, shape, ng1, ng2);
      *(int*)f = p ? *(const int*)p : -0xEFFFFFFF;
      return;
    }

    case COM_FLOAT: 
    case COM_REAL: {
      const void* p = max_element__((const float*)begin, rank, shape, ng1, ng2);
      *(float*)f = p ? *(const float*)p : -HUGE_VALF;
      return;
    }

    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION:  {
      const void* p = max_element__((const double*)begin, rank, shape, ng1,
                                    ng2);
      *(double*)f = p ? *(const double*)p : -HUGE_VAL;
      return;
    }

    default:
      COM_assertion(false);
      return;
  }
}

static void squared_sum(const void* a, const int length, const int stride,
                        const COM_Type type, void* ssum)
{
  switch (type) {
    case COM_FLOAT:
    case COM_REAL:
      for (int i=0; i<length; ++i) {
        float t = ((float*)a)[i*stride];
        ((float*)ssum)[i] += t * t;
      }
      break;

    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION: 
      for (int i=0; i<length; ++i) {
        double t = ((double*)a)[i*stride];
        ((double*)ssum)[i] += t * t;
      }
      break;

    case COM_INT:
    case COM_INTEGER: {
      for (int i=0; i<length; ++i) {
        int t = ((int*)a)[i*stride];
        ((int*)ssum)[i] += t * t;
      }
      break;
    }

    default:
      COM_assertion(false);
      abort();
  }
}

static void min_sqrt_element(const void* begin, const int rank,
                             const int shape[], const int ng1, const int ng2,
                             const COM_Type type, void* v)
{
  switch (type) {
    case COM_FLOAT:
    case COM_REAL: {
      const void* t = min_element__((const float*)begin, rank, shape, ng1, ng2);
      if (t)
        *(float*)v = std::sqrt(*(const float*)t); 
      else
        *(float*)v = HUGE_VALF;
      break;
    }

    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION: {
      const void* t = min_element__((const double*)begin, rank, shape, ng1,
                                    ng2);
      if (t)
        *(double*)v = std::sqrt(*(const double*)t); 
      else
        *(double*)v = HUGE_VAL;
      break;
    }

    case COM_INT:
    case COM_INTEGER: {
      const void* t = min_element__((const int*)begin, rank, shape, ng1,
                                    ng2);
      if (t)
        *(int*)v = (int)std::sqrt(double(*(const int*)t)); 
      else
        *(int*)v = 0xEFFFFFFF;
      break;
    }

    default:
      COM_assertion( false); abort();
  }
}

static void max_sqrt_element(const void* begin, const int rank,
                             const int shape[], const int ng1, const int ng2,
                             const COM_Type type, void* v)
{
  switch (type) {
    case COM_FLOAT:
    case COM_REAL: {
      const void* t = max_element__((const float*)begin, rank, shape, ng1, ng2);
      if (t)
        *(float*)v = std::sqrt(*(const float*)t); 
      else
        *(float*)v = 0.;
      break;
    }

    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION: {
      const void* t = max_element__((const double*)begin, rank, shape, ng1,
                                    ng2);
      if (t)
        *(double*)v = std::sqrt(*(const double*)t); 
      else
        *(double*)v = 0.;
      break;
    }

    case COM_INT:
    case COM_INTEGER: {
      const void* t = max_element__((const int*)begin, rank, shape, ng1,
                                    ng2);
      if (t)
        *(int*)v = (int)std::sqrt(double(*(const int*)t)); 
      else
        *(int*)v = 0;
      break;
    }

    default:
      COM_assertion(false);
      abort();
  }
}

#include <hdf.h>

/* Old error reporting routine
static void hdf_error_message(const char* s, int i,
                              const std::string& errorhandle)
{
  if (errorhandle != "ignore") {
    std::cerr << "Rocout Error: HDF routine " << s << " returned value " << i
              << ": " << std::endl;
    if (errorhandle == "abort") {
      if (COMMPI_Initialized())
        MPI_Abort(MPI_COMM_WORLD, 0);
      else
        abort();
    }
  }
}
 */

/** New informative error-checking macro to replace hdf_error_message().
 */
#define HDF_CHECK(routine, args) \
{ \
  intn status = HDF4::routine args; \
  if (status == FAIL && errorhandle != "ignore") { \
    std::cerr << "Rocout::write_attribute: " #routine " (line " \
              << __LINE__ << " in " << __FILE__ << ") failed: " \
              << HDF4::error_msg() << '\n' \
              << "in write_data( fname == '" << fname << "', label == '" \
              << label << "', units == '" << units << "', ... , rank == " \
              << rank << ", shape[] == { " << shape[0]; \
    { int x; for (x=1; x<rank; ++x) std::cerr << ", " << shape[x]; } \
    std::cerr << " }, ... )" << std::endl; \
    if (errorhandle == "abort") { \
      if (COMMPI_Initialized()) \
        MPI_Abort(MPI_COMM_WORLD, 0); \
      else \
        abort(); \
    } \
  } \
}

static int write_data(const char* fname, const char* label, const char* units,
                      const char* format, const char* coordsys, const int _rank,
                      const int _shape[], const int ng1, const int ng2,
                      const COM_Type type, const void* p, const void* minv,
                      const void* maxv, const std::string& errorhandle,
                      const int mode)
{
  int32 rank = _rank;
  int32 shape[] = { _shape[0], _shape[1], _shape[2] };
#ifdef DEBUG_DUMP_PREFIX
  int32 numItems = 1;
#endif // DEBUG_DUMP_PREFIX

  if (_shape[0] == 0 || _shape[rank-1] == 0)
    return false;
  for (int i=0; i<rank; ++i) {
#ifdef DEBUG_DUMP_PREFIX
    numItems *= shape[i];
#endif // DEBUG_DUMP_PREFIX
    if (shape[i] < 1) {
      std::cerr << "Rocout Error: Dimension (starting with 1) " << i + 1
                << " of data " << format << " is " << shape[i]
                << ".\n A positive number is expected. Aborting..." 
                << std::endl;
      abort();
    }
  }

#ifdef DEBUG_DUMP_PREFIX
  {
    if (s_fout) {
      int i;
      switch (comtype2hdftype(type)) {
        case DFNT_CHAR8:
          for (i=0; i<numItems; ++i)
            *s_fout << i << " : " << (int)((const char*)p)[i] << '\n';
          break;
        case DFNT_INT32:
          for (i=0; i<numItems; ++i)
            *s_fout << i << " : " << ((const int*)p)[i] << '\n';
          break;
        case DFNT_FLOAT32:
          for (i=0; i<numItems; ++i)
            *s_fout << i << " : " << ((const float*)p)[i] << '\n';
          break;
        case DFNT_FLOAT64:
          for (i=0; i<numItems; ++i) 
            *s_fout << i << " : " << ((const double*)p)[i] << '\n';
          break;
        default:
          *s_fout << "Datatype is " << comtype2hdftype(type) << '\n';
          break;
      }
      *s_fout << "###########################################\n";
    }
  }
#endif // DEBUG_DUMP_PREFIX

  HDF_CHECK(DFSDsetdims, (rank, shape)); 
  HDF_CHECK(DFSDsetNT, (comtype2hdftype(type)));

  HDF_CHECK(DFSDsetdatastrs, (label, units, format, coordsys)); 

  double t1, t2;
  if (minv == NULL) { // Compute the max and min
    minv = &t1;
    min_element(p, _rank, _shape, ng1, ng2, type, &t1);
    maxv = &t2;
    max_element(p, _rank, _shape, ng1, ng2, type, &t2);
  }

  HDF_CHECK(DFSDsetrange, (const_cast<void*>(maxv), const_cast<void*>(minv)));

  if (mode > 0) { // append
    HDF_CHECK(DFSDadddata, (fname, rank, shape, const_cast<void*>(p)));
  } else {
    HDF_CHECK(DFSDputdata, (fname, rank, shape, const_cast<void*>(p)));
  }

  return true;
};

static int comtype2hdftype(COM_Type i)
{
  switch (i) {
    case COM_CHAR:
    case COM_CHARACTER: 
      return DFNT_CHAR8;

    case COM_INT:
    case COM_INTEGER:   
      return (sizeof(int) == 4) ? DFNT_INT32 : DFNT_INT64;

    case COM_FLOAT: 
    case COM_REAL:
      return (sizeof(float) == 4) ? DFNT_FLOAT32 : DFNT_FLOAT64;

    case COM_DOUBLE:
    case COM_DOUBLE_PRECISION: 
      if (sizeof(double) == 4) 
        return DFNT_FLOAT32;
      else if ( sizeof( double) == 8) 
        return DFNT_FLOAT64;
      else
        return DFNT_FLOAT128;

    default:
      COM_assertion(false);
      return 0;
  }
}







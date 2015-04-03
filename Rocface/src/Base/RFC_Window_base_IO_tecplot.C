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
// $Id: RFC_Window_base_IO_tecplot.C,v 1.4 2008/12/06 08:43:28 mtcampbe Exp $

#include "RFC_Window_base.h"
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <cassert>

RFC_BEGIN_NAME_SPACE

// The main interface for output a pane in Tecplot format.
void RFC_Pane_base::
write_tec_ascii( std::ostream &os, const COM::Attribute *attr) const {
  if ( _base->size_of_nodes()==0) return;
  
  set_ascii_mode(os);
  if ( _base->is_structured()) 
    write_tec_ij( os, attr); // Structured mesh
  else { // Unstructured mesh
    std::vector< const COM::Connectivity*>  elems;
    _base->elements( elems);
    
    if ( elems.size() == 1 && elems[0]->size_of_edges_pe() == 3)
      // Triangular finite element mesh
      write_tec_tri( os, *elems[0], attr);
    else
      // Quadrilateral/mixed finite element mesh
      write_tec_mixed( os, elems, attr);
  }
}

static void write_attr( std::ostream &os, const COM::Attribute *attr, int i) {
  const int dim = attr->size_of_components();
  switch ( attr->data_type()) {
  case COM_DOUBLE:
  case COM_DOUBLE_PRECISION: {
    const double *values = reinterpret_cast<const double*>(attr->pointer());
    for ( int j=0; j<dim; ++j) os << ' ' << values[i*dim+j];
    break;
  }
  case COM_FLOAT:
  case COM_REAL: {
    const float *values = reinterpret_cast<const float*>(attr->pointer());
    for ( int j=0; j<dim; ++j) os << ' ' << values[i*dim+j];
    break;
  }
  case COM_INT:
  case COM_INTEGER: {
    const int *values = reinterpret_cast<const int*>(attr->pointer());
    for ( int j=0; j<dim; ++j) os << ' ' << values[i*dim+j];
    break;
  }
  default:
    assert(false); abort();
  }
}

// Write out an IJ-ordered data in POINT format of Tecplot.
void RFC_Pane_base::
write_tec_ij( std::ostream &os, const COM::Attribute *attr) const {  
  RFC_assertion( _base != NULL);

  // Write out the head
  if ( attr)
    os << "TITLE = \"" << attr->name() << "\" \n";
  else
    os << "TITLE = \"Structured mesh\" \n";
  os << "VARIABLES = \"X\", \"Y\", \"Z\"";
  if ( attr) {
    const int MAXDIM=4, dim=attr->size_of_components();
    const char vname[MAXDIM+1]="UVWT";
    RFC_assertion( dim<=MAXDIM);
    for ( int i=0; i<dim; ++i)
      os << ", \"" << vname[i] << "\"";
  }
  os << "\nZONE I=" << _base->size_i()
     << ", J=" << _base->size_j() << ", F=POINT\n";

  // Write out the coordinates
  const Real *p = _base->coordinates();
  for ( int i=0, n=_base->size_of_nodes(); i<n; ++i) {
    os << p[3*i] << ' ' << p[3*i+1] << ' ' << p[3*i+2];
    if ( attr) write_attr( os, attr, i);
    os << std::endl; 
  }
  os << std::endl;
}

// Write out a triangular finite element mesh in FEPOINT format of Tecplot
void RFC_Pane_base::
write_tec_tri( std::ostream &os, const COM::Connectivity &ec,
	       const COM::Attribute *attr) const {  
  RFC_assertion( _base != NULL);

  if ( attr)
    os << "TITLE = \"" << attr->name() << "\" \n";
  else
    os << "TITLE = \"Triangular finite element mesh\" \n";
  os << "VARIABLES = \"X\", \"Y\", \"Z\"";
  if ( attr) {
    const int MAXDIM=4, dim=attr->size_of_components();
    const char vname[MAXDIM+1]="UVWT";
    RFC_assertion( dim<=MAXDIM);
    for ( int i=0; i<dim; ++i)
      os << ", \"" << vname[i] << "\"";
  }
  os << "\nZONE N=" << _base->size_of_nodes() << ", E="
     << _base->size_of_elements() << ", F=FEPOINT, ET=TRIANGLE\n";

  // Write out the coordinates
  const Real *p = _base->coordinates();
  for ( int i=0, n=_base->size_of_nodes(); i<n; ++i) {
    os << p[3*i] << ' ' << p[3*i+1] << ' ' << p[3*i+2];
    if ( attr) write_attr( os, attr, i);
    os << std::endl; 
  }
  os << std::endl;

  // Write out the elements
  const int *e = ec.pointer();
  int nn = ec.size_of_nodes_pe();
  for ( unsigned int i=0, n=_base->size_of_elements(); i<n; ++i)
  { os << e[nn*i] << ' ' << e[nn*i+1] << ' ' << e[nn*i+2] << std::endl; }
}

// Write out a triangular finite element mesh in FEPOINT format of Tecplot
void RFC_Pane_base::
write_tec_mixed( std::ostream &os, 
		 const std::vector<const COM::Connectivity*> &elems,
		 const COM::Attribute *attr) const {
  RFC_assertion( _base != NULL);

  if ( attr)
    os << "TITLE = \"" << attr->name() << "\" \n";
  else
    os << "TITLE = \"Triangular finite element mesh\" \n";
  os << "VARIABLES = \"X\", \"Y\", \"Z\"";
  if ( attr) {
    const int MAXDIM=4, dim=attr->size_of_components();
    const char vname[MAXDIM+1]="UVWT";
    RFC_assertion( dim<=MAXDIM);
    for ( int i=0; i<dim; ++i)
      os << ", \"" << vname[i] << "\"";
  }
  os << "\nZONE N=" << _base->size_of_nodes() << ", E="
     << _base->size_of_elements() << ", F=FEPOINT, ET=QUADRILATERAL\n";

  // Write out the coordinates
  const Real *p = _base->coordinates();
  for ( int i=0, n=_base->size_of_nodes(); i<n; ++i) {
    os << p[3*i] << ' ' << p[3*i+1] << ' ' << p[3*i+2];
    if ( attr) write_attr( os, attr, i);
    os << std::endl;
  }
  os << std::endl;

  // Write out the elements
  for ( std::vector<const COM::Connectivity*>::const_iterator
	  it = elems.begin(); it != elems.end(); ++it) {
    const int *e = (*it)->pointer();
    int nn = (*it)->size_of_nodes_pe();
    switch ( (*it)->size_of_edges_pe()) {
    case 3:
      for ( int i=0,n=(*it)->size_of_elements(); i<n; ++i)
      { os << e[nn*i] << ' ' << e[nn*i+1] << ' ' 
	   << e[nn*i+2] << ' ' << e[nn*i+2] << std::endl; }
      break;
    case 4: 
      for ( int i=0, n=(*it)->size_of_elements(); i<n; ++i)
      { os << e[nn*i] << ' ' << e[nn*i+1] << ' ' 
	   << e[nn*i+2] << ' ' << e[nn*i+3] << std::endl; }
      break;
    default:
      RFC_assertion( false);
    }
  }
}


// Write out the subdivision in FEPOINT format of Tecplot
void RFC_Pane_base::
write_tec_sub( std::ostream &os) const {
  RFC_assertion( _base != NULL);
  if ( size_of_subnodes() == 0) return;

  os << "TITLE = \"Triangular subdivision mesh\" \n";
  os << "VARIABLES = \"X\", \"Y\", \"Z\"";
  os << "\nZONE N=" << size_of_subnodes() << ", E="
     << size_of_subfaces() << ", F=FEPOINT, ET=TRIANGLE\n";

  // Write out the coordinates, and the node ids start from 1.
  for ( int i=1, size=size_of_subnodes(); i<=size; ++i) {
    Point_3 p = get_point_of_subnode( i);

    os << p.x() << ' ' << p.y() << ' ' << p.z() << std::endl;
  }
  os << std::endl;

  // Write out the elements. We directly access the connectivity table
  // and their indices start from 0. 
  for ( int i=0, size=size_of_subfaces(); i<size; ++i) {
    os << _subfaces[i][0] << ' ' << _subfaces[i][1] 
       << ' ' << _subfaces[i][2] << std::endl;
  }
}

/*!
  \param fname Name of the output file.
  \param a Attribute to be writtin out. Default is mesh only.
*/
void 
RFC_Window_base::write_tec_sub( const char *prefix) const {
  std::ofstream os( (std::string( prefix) + ".plt").c_str()); RFC_assertion(os);
  os.precision(9);
  os.setf( std::ios::scientific, std::ios::floatfield);
  for (Pane_set::const_iterator 
	 it=_pane_set.begin(), iend=_pane_set.end(); it != iend; ++it) { 
    RFC_Pane_base &pane = *it->second;
    if ( pane.is_master()) {
      pane.write_tec_sub( os);
    }
  }
}

/*!
  \param fname Name of the output file.
  \param a Attribute to be writtin out. Default is mesh only.
*/
void 
RFC_Window_base::write_tec_ascii( const char *prefix) const {
  std::ofstream os( (std::string( prefix) + ".plt").c_str()); RFC_assertion(os);
  os.precision(9);
  os.setf( std::ios::scientific, std::ios::floatfield);
  for (Pane_set::const_iterator 
	 it=_pane_set.begin(), iend=_pane_set.end(); it != iend; ++it) { 
    RFC_Pane_base &pane = *it->second;
    if ( pane.is_master()) {
      pane.write_tec_ascii( os, 0);
    }
  }
}

RFC_END_NAME_SPACE







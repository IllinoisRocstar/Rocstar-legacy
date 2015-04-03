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
// $Id: compute_element_areas.C,v 1.6 2008/12/06 08:43:23 mtcampbe Exp $

/** \file compute_element_areas.C
 *  This file contains implementation for computing the area of each face of 
 *  the surface mesh of a given window. 
 */
/*  Author: Phillip Alexander
 *  Dates:  Dec. 13, 2002
 */

#include "Rocsurf.h"
#include "roccom_devel.h"
#include "Element_accessors.h"
#include "Generic_element_2.h"
#include <vector>

SURF_BEGIN_NAMESPACE

void Rocsurf::compute_element_areas( COM::Attribute *element_areas, 
				     const COM::Attribute *pnts) {
  COM_assertion_msg( element_areas && element_areas->is_elemental(),
		     "Argument must be elemental attribute");
  COM_assertion_msg( element_areas->size_of_components()== 1 &&
		     COM_compatible_types(element_areas->data_type(), COM_DOUBLE),
		     "Argument must be double-precision scalars"); 

  std::vector< COM:: Pane*> panes;
  element_areas->window() -> panes( panes); 
  Element_node_vectors_k_const<Point_3<Real> > ps;
  Vector_2<Real> nc(0,0);

  std::vector< COM::Pane*>::const_iterator it = panes.begin();
  
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    COM::Pane &pane = **it; 

    const COM::Attribute *nc_pane = (pnts==NULL)?pane.attribute( COM::COM_NC):
      pane.attribute( pnts->id());
    COM::Attribute *a_pane = pane.attribute( element_areas->id());
    COM_assertion( pane.size_of_elements()==0 ||
		   nc_pane->stride()==3 && a_pane->stride()==1);

    const Point_3<Real> *pnts = (const Point_3<Real>*)(nc_pane->pointer()); 
    Real *ptr = (Real *)(a_pane->pointer());

    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next(),++ptr) {
      Generic_element_2 e(ene.size_of_edges(), ene.size_of_nodes());
      ps.set( pnts, ene, 1);
      int size = e.get_num_gp();
      Real this_area = 0;

      for (int k = 0; k<size; k++){
	Real weight = e.get_gp_weight( k);
	e.get_gp_nat_coor(k, nc);
	Real jacobi_det = e.Jacobian_det(ps,nc);
	this_area += weight * jacobi_det;
      }
      *ptr = this_area;
    } 
  }
}

// Integrate a function given by an elemental attribute over surface
void Rocsurf::integrate( const COM::Attribute *x, double *z) {

  COM_assertion_msg( x && x->is_elemental(),
		     "Argument must be elemental attribute");
  COM_assertion_msg( COM_compatible_types(x->data_type(), COM_DOUBLE),
		     "Argument must be double precision");

  const int ncomp = x->size_of_components();
  // Initialize output to 0.
  for ( int kk=0; kk<ncomp; ++kk) z[kk]=0.;

  std::vector< const COM:: Pane*> panes;
  x->window()-> panes( panes); 
  Element_node_vectors_k_const<Point_3<Real> > ps;
  Vector_2<Real> nc(0,0);

  std::vector< const COM::Pane*>::const_iterator it = panes.begin();
  
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    // Assuming contiguous layout
    const COM::Attribute *nc_pane = pane.attribute( COM::COM_NC);
    const COM::Attribute *x_pane = pane.attribute( x->id());
    COM_assertion( pane.size_of_elements()==0 ||
		   nc_pane->stride()==3 && x_pane->stride()==ncomp);

    const Point_3<Real> *pnts = (const Point_3<Real>*)(nc_pane->pointer()); 
    const Real *xptr = (Real *)(x_pane->pointer());
    
    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next(), xptr+=ncomp) {
      Generic_element_2 e(ene.size_of_edges(), ene.size_of_nodes());
      ps.set( pnts, ene, 1);
      int size = e.get_num_gp();

      for (int k = 0; k<size; k++){
	Real weight = e.get_gp_weight( k);
	e.get_gp_nat_coor(k, nc);
	Real jacobi_det = e.Jacobian_det(ps,nc);

	for ( int kk=0; kk<ncomp; ++kk) 
	  z[kk] += weight * jacobi_det * xptr[kk];
      }
    } 
  }

  if ( COMMPI_Initialized()) {
    // Perform reduction on all processros within the communicator.
    std::vector<double> lval(ncomp); 
    std::copy( z, z+ncomp, lval.begin());
    MPI_Allreduce( &lval[0], z, ncomp, MPI_DOUBLE, MPI_SUM, 
		   x->window()->get_communicator());
  }
}

SURF_END_NAMESPACE







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
// $Id: compute_element_normals.C,v 1.4 2008/12/06 08:43:23 mtcampbe Exp $

#include "Rocsurf.h"
#include "roccom_devel.h"
#include "Generic_element_2.h"
#include <vector>

SURF_BEGIN_NAMESPACE

void Rocsurf::compute_element_normals( COM::Attribute *elem_nrmls,
				       const int *to_normalize, 
				       const COM::Attribute *pnts) {

  assert( elem_nrmls != NULL && elem_nrmls ->size_of_components() == 3); 

  std::vector< COM:: Pane*> panes;
  elem_nrmls->window() -> panes( panes); 
  Element_node_vectors_k_const<Point_3<Real> > ps;

  Vector_2<Real> nc(0.5,0.5);
  Vector_3<Real> J[2];

  std::vector< COM::Pane*>::const_iterator it = panes.begin();
  
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 

    const COM::Attribute *nc_pane = (pnts==NULL)?pane.attribute( COM::COM_NC):
      pane.attribute( pnts->id());
    COM_assertion( pane.size_of_elements()==0 || nc_pane->stride()==3);
    
    const Point_3<Real> *pnts = (const Point_3<Real>*)(nc_pane->pointer());
    Vector_3<Real> *ptr = (Vector_3<Real> *)
      (pane.attribute( elem_nrmls->id())->pointer());
    
    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1); 

    for ( int j=pane.size_of_elements(); j>0; --j, ene.next(),++ptr) {
      Generic_element_2 e(ene.size_of_edges(), ene.size_of_nodes());
      ps.set( pnts, ene, 1);
	
      e.Jacobian( ps, nc, J);
      *ptr = Vector_3<Real>::cross_product( J[0], J[1]);
      if ( to_normalize==NULL || *to_normalize) 
	ptr->normalize();
      else if ( e.size_of_edges()==3) // If triangle, reduce by half.
	(*ptr) *= 0.5;
    }
  }
}

SURF_END_NAMESPACE







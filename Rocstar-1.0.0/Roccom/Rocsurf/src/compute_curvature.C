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
// $Id: compute_curvature.C,v 1.6 2008/12/06 08:43:23 mtcampbe Exp $

#include "Manifold_2.h"
#include "Generic_element_2.h"
#include "../Rocblas/include/Rocblas.h"

SURF_BEGIN_NAMESPACE

static inline double sign(double x) {
  return (x>=0) ? 1 : -1;
}

// Helper function for compute_nodal_mcn.
// Compute weights of LB-operator about the first vertex of a given
// quadrilateral. Also returns the area of the element.
static double
compute_lbop_weights( Point_3<Real> *ps, Real *ws) {
  ws[0] = ws[1] = ws[2] = 0.;

  Generic_element_2 e(4);
  int size = e.get_num_gp();

  double area = 0;
  for (int i=0; i<size; i++){
    // Compute tangents at quadrature points.
    Vector_2<Real> nc;
    e.get_gp_nat_coor(i, nc);

    const Real &xi=nc[0], &eta=nc[1];

    const Vector_3<Real> u = (1-eta)*(ps[1]-ps[0])+eta*(ps[2]-ps[3]);
    const Vector_3<Real> v = (1-xi)* (ps[3]-ps[0])+xi* (ps[2]-ps[1]);

    // Obtain the weights and local area of quadrature point
    const Real weight = e.get_gp_weight( i);

    // Evaluate the weights for Laplace-Beltrami operator
    const Real jacobi_det = u.cross_product(u,v).norm();
    const Real uv = u*v;

    const Real su = weight * ((1-eta)*uv-(1-xi)*(u*u))/jacobi_det;
    const Real sv = weight * ((1-xi)*uv-(1-eta)*(v*v))/jacobi_det;

    ws[0] += (1-eta)*sv -xi*su;
    ws[1] += xi*su + eta*sv;
    ws[2] += (1-xi)*su - eta*sv;;
    area  += weight * jacobi_det;
  }

  return area;
}

// Compute mean-curvature normals and their Laplace-Beltrami operator.
// Algorithm based on Y. Zhang, C. Bajaj, and G. Xu, "Surface Smoothing
// and quality improvement of quadrilateral/hexahedral meshes with
// geometric flow", International Meshing Roundtable, 2005.
void Window_manifold_2::
compute_mcn( COM::Attribute *mcn_in, COM::Attribute *lbmcn_in) {

  COM_assertion( mcn_in != NULL && mcn_in ->size_of_components() == 3 && 
		 mcn_in->is_nodal());
  COM_assertion( lbmcn_in != NULL && lbmcn_in->size_of_components() == 3 && 
		 lbmcn_in->is_nodal());

  COM::Attribute *mcn, *lbmcn;
  // Inherit mcn and lbmcn onto buffer window
  if ( mcn_in->window() != _buf_window)
    mcn = _buf_window->inherit( mcn_in, "mcn__MCNTEMP", 
				false, true, NULL, 0);
  else 
    mcn = mcn_in;

  if ( lbmcn_in->window() != _buf_window)
    lbmcn = _buf_window->inherit( lbmcn_in, "lbmcn__MCNTEMP", 
				  false, true, NULL, 0);
  else
    lbmcn = lbmcn_in;

  // Allocate buffer spaces for weights and areas
  COM::Attribute *areas=NULL, *weights=NULL;
  areas = _buf_window->new_attribute( "areas__MCNTEMP", 'n',
				      COM_DOUBLE, 1, "");
  _buf_window->resize_array( areas, NULL);

  weights = _buf_window->new_attribute( "weights__MCNTEMP", 'e',
					COM_DOUBLE, 12, "");
  _buf_window->resize_array( weights, NULL);
  _buf_window->init_done( false);

  // Initialize attributes to 0s
  double zero = 0.;
  Rocblas::copy_scalar( &zero, mcn);
  Rocblas::copy_scalar( &zero, areas);
  Rocblas::copy_scalar( &zero, weights);

  std::vector< COM::Pane*> panes;
  mcn->window()->panes( panes); 
  Point_3<Real> ps[4];

  std::vector< COM::Pane*>::const_iterator it = panes.begin();

  // First, compute the mean-curvature normal, and save the weights
  // for Laplace-Beltrami operator.
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it;

    const Point_3<Real> *pnts =
      reinterpret_cast<const Point_3<Real>*>(pane.coordinates()); 
    Vector_3<Real> *mcn_ptr = (Vector_3<Real> *)
      (pane.attribute( mcn->id())->pointer());
    Vector_3<Real> *ws_ptr = (Vector_3<Real> *)
      (pane.attribute( weights->id())->pointer());
    Real *area_ptr = (Real *)(pane.attribute( areas->id())->pointer());

    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1);

    for ( int j=0, jsize=pane.size_of_elements(); j<jsize; ++j, ene.next()) {
      Point_3<Real> cnt(0,0,0);
      int ne=ene.size_of_edges();
      for ( int kk=0; kk<ne; ++kk) 
	(Vector_3<Real>&)cnt += (const Vector_3<Real>&)pnts[ene[kk]-1];
      (Vector_3<Real>&)cnt /= ne;

      // Loop through each vertex 
      for ( int k=0; k<ne; ++k) {
	// Assign points for the quadrilateral.
	ps[0] = pnts[ene[k]-1];
	ps[1] = ps[0]+0.5*(pnts[ene[(k+1)%ne]-1]-ps[0]);
	ps[2] = cnt;
	ps[3] = ps[0]+0.5*(pnts[ene[(k+ne-1)%ne]-1]-ps[0]);

	// Compute weights of LB-operator for the first vertex and
	// increment the area.
	Vector_3<Real> &ws = ws_ptr[4*j+k];
	area_ptr[ ene[k]-1] += compute_lbop_weights( ps, &ws[0]);
	
	Vector_3<Real> dA = (ws[0]*(ps[1]-ps[0])+ws[1]*(ps[2]-ps[0])+
			     ws[2]*(ps[3]-ps[0]));

	mcn_ptr[ ene[k]-1] += dA;
      }
    }
  }

  // Reduce on the area and mcn and divide mcn by areas.
  reduce_on_shared_nodes( areas, MPI_SUM);
  reduce_on_shared_nodes( mcn,  MPI_SUM);
  Rocblas::div( mcn, areas, mcn);

  // Second, compute the Laplace-Beltrami of the mean-curvature.
  Rocblas::copy_scalar( &zero, lbmcn);

  it = panes.begin();
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it) {
    const COM::Pane &pane = **it; 

    const Vector_3<Real> *mcn_ptr = (const Vector_3<Real> *)
      (pane.attribute( mcn->id())->pointer());
    const Vector_3<Real> *ws_ptr = (const Vector_3<Real> *)
      (pane.attribute( weights->id())->pointer());

    Vector_3<Real> *lbmcn_ptr = (Vector_3<Real> *)
      (pane.attribute( lbmcn->id())->pointer());

    // Loop through elements of the pane
    Element_node_enumerator ene( &pane, 1); 
    double fs[4];

    for ( int j=0, jsize=pane.size_of_elements(); j<jsize; ++j, ene.next()) {
      int ne=ene.size_of_edges();

      // Loop through each vertex 
      for ( int k=0; k<ne; ++k) {

	// Compute the magnitude and vector of current vertex
	int ii0=ene[k]-1;
	fs[0] = mcn_ptr[ii0].norm();
	Vector_3<Real> vec=(mcn_ptr[ii0]);  if (fs[0]>0) vec/=fs[0];
	
	// Compute length of other vertices mcn corresponding to the 
	// direction of mcn at the current vertex
	for ( int kk=1; kk<ne; ++kk) {
	  int ii=ene[(k+kk)%ne]-1;

	  fs[kk] = sign(mcn_ptr[ii]*vec)*mcn_ptr[ii].norm();
	}

	const Vector_3<Real> &ws = ws_ptr[4*j+k];
	lbmcn_ptr[ ene[k]-1] += 4.*(ws[0]*(fs[1]-fs[0])+ws[1]*(fs[2]-fs[0])+
				    ws[2]*(fs[3]-fs[0]))*vec;
      }
    }
  }

  // Reduce on lbmcn and divide it by areas.
  reduce_on_shared_nodes( lbmcn,  MPI_SUM);
  Rocblas::div( lbmcn, areas, lbmcn);

  // Delete buffer space in reverse order
  _buf_window->delete_attribute( weights->name());
  _buf_window->delete_attribute( areas->name());
  if ( lbmcn_in->window() != _buf_window)
    _buf_window->delete_attribute( lbmcn->name());
  if ( mcn_in->window() != _buf_window)
    _buf_window->delete_attribute( mcn->name());

  _buf_window->init_done(false);
}

SURF_END_NAMESPACE







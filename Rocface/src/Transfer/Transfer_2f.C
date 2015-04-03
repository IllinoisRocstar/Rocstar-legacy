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
// $Id: Transfer_2f.C,v 1.9 2008/12/06 08:43:29 mtcampbe Exp $

//===================================================================
// This file contains the implementation for transfering data from nodes
// to faces and from faces to faces.
//
//  Author: Xiangmin Jiao
//  Revision: June 15, 2001
//===================================================================

#include "Transfer_2f.h"
#include <limits>
#define QUIET_NAN   std::numeric_limits<Real>::quiet_NaN()

RFC_BEGIN_NAME_SPACE

// This integrator can integrate over subset of an element.
// Note that the integral is added to the current values of v and area.
template < class _Data>
void
Transfer_base::integrate_subface( const RFC_Pane_transfer *p_src,
				  RFC_Pane_transfer *p_trg,
				  const _Data &data_s,
				  ENE &ene_src,
				  ENE &ene_trg,
				  int sfid_src,
				  int sfid_trg,
				  const Real alpha, 
				  Facial_data &tDF,
				  Facial_data &tBF,
				  int doa) 
{
  // Initialize natural cooredinates of the subnodes of the subface in its
  // parent source and target faces. Compute ncs_s only if alpha!=1. 
  Point_3 ps_s[Generic_element::MAX_SIZE], ps_t[Generic_element::MAX_SIZE];
  Point_2 ncs_s[Generic_element::MAX_SIZE], ncs_t[Generic_element::MAX_SIZE];

  Generic_element e_s(ene_src.pane()?ene_src.size_of_edges():3, 
		      ene_src.pane()?ene_src.size_of_nodes():3);

  // Initialize physical and natural coordinates for source element
  if ( is_nodal(data_s) || alpha!=1) {
    for ( int i=0; i<3; ++i)
      p_src->get_nat_coor_in_element( sfid_src, i, ncs_s[i]);

    if ( alpha!=1.) {
      Nodal_coor_const nc;
      Element_coor_const  pnts_s( nc, p_src->coordinates(), ene_src);
  
      for ( int i=0; i<3; ++i) e_s.interpolate( pnts_s, ncs_s[i], &ps_s[i]);
    }
  }
  else {
    std::fill_n( &ncs_s[0][0], 6, QUIET_NAN);
    std::fill_n( &ps_s[0][0], Generic_element::MAX_SIZE*3, QUIET_NAN);
  }

  {  // Initialize physical and natural coordinates for target element
    for ( int i=0; i<3; ++i)
      p_trg->get_nat_coor_in_element( sfid_trg, i, ncs_t[i]);

    Nodal_coor_const nc;
    Generic_element e_t( ene_trg.size_of_edges(), ene_trg.size_of_nodes());
    Element_coor_const  pnts_t( nc, p_trg->coordinates(), ene_trg);

    for ( int i=0; i<3; ++i) e_t.interpolate( pnts_t, ncs_t[i], &ps_t[i]);
  }

  // Loop throught the quadrature points of the subfacet.
  Vector_n vt(data_s.dimension(), 0); 
  Array_n  t( vt.begin(), vt.end()), v=tDF.get_value( p_trg, ene_trg.id());
  Real     &area = tBF.get_value( p_trg, ene_trg.id())[0];

  Generic_element sub_e( 3);
  Point_2 sub_nc, nc_s; 
  
  if ( is_nodal( data_s)) {
    if ( doa == 0) doa = 2; // Set the default degree of accuracy
  }
  else 
    doa=1;

  for ( int i=0, n = sub_e.get_num_gp(doa); i<n; ++i) {
    sub_e.get_gp_nat_coor( i, sub_nc, doa);

    if ( is_nodal( data_s)) 
      sub_e.interpolate( ncs_s, sub_nc, &nc_s);

    interpolate( e_s, data_s, nc_s, t);

    Real a=sub_e.get_gp_weight(i, doa);
    a *= sub_e.Jacobian_det( ps_s, ps_t, alpha, sub_nc);

    t *= a;
    v += t;
    area += a;
  }
}

template < class _SDF>
void 
Transfer_base::transfer_2f( const _SDF &sDF, 
			    Facial_data &tDF,
			    const Real alpha,
			    int doa, 
			    bool verbose) 
{
  double t0=0.;
  if ( verbose) {
    trg.barrier(); t0=get_wtime();
  }

  src.replicate_data( sDF, alpha!=1);
  // First, create buffer space for the target window and initialize
  //        the entries of the target mesh to zero.
  trg.init_facial_buffers( tDF, 1);
  Facial_data tBF ( trg.facial_buffer( 0));

  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *trg_data = (*pit)->pointer( tDF.id());
    Real *trg_buf  = (*pit)->pointer( tBF.id());
    
    std::fill( trg_data, trg_data+(*pit)->size_of_faces()*tDF.dimension(), 0);
    std::fill( trg_buf, trg_buf+(*pit)->size_of_faces(), 0);
  }

  // Second, compute the integral over the target meshes by looping through
  //         the subfaces of the target window
  ENE   ene_src, ene_trg; 
  const RFC_Pane_transfer *p_src = NULL;
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    // Loop through the subfaces of the target window
    for ( int i=1, size=(*pit)->size_of_subfaces(); i<=size; ++i) {
      (*pit)->get_host_element_of_subface( i, ene_trg);
      if ( !(*pit)->need_recv( ene_trg.id())) continue;

      const Face_ID &fid = (*pit)->get_subface_counterpart(i);
      if ( !p_src || p_src->id()!=fid.pane_id) 
	p_src = get_src_pane( fid.pane_id);
      if ( alpha!=1 || is_nodal( sDF.tag()) )
	p_src->get_host_element_of_subface( fid.face_id, ene_src);

      if ( is_nodal( sDF.tag()))
	integrate_subface( p_src, *pit, 
			   make_field(sDF, p_src, ene_src),
			   ene_src, ene_trg, fid.face_id, i, alpha, tDF, tBF, doa);
      else {
	int id = p_src->get_parent_face( fid.face_id);
	integrate_subface( p_src, *pit,
			   make_field(sDF, p_src, id),
			   ene_src, ene_trg, fid.face_id, i, alpha, tDF, tBF, doa);
      }
    }
  }

  // Loop through the panes of the target mesh
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *trg_data = (*pit)->pointer( tDF.id());
    Real *trg_buf  = (*pit)->pointer( tBF.id());

    for ( int i=1, size=(*pit)->size_of_faces(); i<=size; ++i) {
      if ( !(*pit)->need_recv(i)) continue;
      tDF.get_value( trg_data, i) /= tBF.get_value( trg_buf, i)[0];
    }
  }

  // Clean up the transfer buffers
  trg.delete_facial_buffers();
  src.clear_replicated_data();

  if ( verbose) {
    trg.barrier();
    if ( trg.is_root()) {
      std::cout << "RFACE: Transfer to faces done in " 
		<< get_wtime()-t0 << " seconds." << std::endl;
    }
  }
}

void Transfer_n2f::
transfer( const Nodal_data_const &sv, Facial_data &tf, 
	  const Real alpha, int doa, bool verb) {
  Base::transfer_2f( sv, tf, alpha, doa, verb);
}

void Transfer_f2f::
transfer( const Facial_data_const &sf, Facial_data &tf, 
	  const Real alpha, int doa, bool verb) {
  Base::transfer_2f( sf, tf, alpha, doa, verb);
}


RFC_END_NAME_SPACE







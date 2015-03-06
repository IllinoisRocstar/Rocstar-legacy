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
// $Id: Transfer_2n.C,v 1.10 2008/12/06 08:43:29 mtcampbe Exp $

//===================================================================
// This file contains the implementation for transfering data from nodes
// to nodes and from faces to nodes.
//
//===================================================================

#include "Transfer_2n.h"

RFC_BEGIN_NAME_SPACE

// This function obtains the initial guess for the unknowns 
//   by transfering them from the source mesh.
template <class _SDF>
void Transfer_base::
interpolate_fe( const _SDF &sDF, Nodal_data &tDF, bool verbose) 
{
  double t0=0.;
  if ( verbose) {
    trg.barrier(); t0=get_wtime();
  }

  std::vector<bool> flags;
  // Loop through all the panes of target window
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    RFC_Pane_transfer *p_trg = *pit;
    Real *p = p_trg->pointer( tDF.id());
    std::fill( p, p+p_trg->size_of_nodes()*tDF.dimension(), Real(0));
    flags.clear(); flags.resize(p_trg->size_of_nodes()+1, false);

    // Loop through the faces to mark the ones that expect values.
    ENE  ene( p_trg->base(),  1);
    for ( int k=1, size=p_trg->size_of_faces(); k<=size; ++k, ene.next()) {
      if ( !p_trg->need_recv(k)) continue;

      for ( int i=0, n=ene.size_of_nodes(); i<n; ++i) flags[ene[i]] = true;
    }

    ENE ene_src; 
    Point_2  nc; 
    const RFC_Pane_transfer *p_src = NULL;
    int count=0, nnodes=p_trg->size_of_nodes()-p_trg->size_of_isolated_nodes();

    // Loop through the subnodes of the target window
    for ( int i=1, size=p_trg->size_of_subnodes(); i<=size; ++i) {

      int svid_trg = i;
      if ( p_trg->parent_type_of_subnode( svid_trg) != PARENT_VERTEX) 
	{ if ( count >= nnodes) break; else continue; }
      else ++count;

      int pvid_trg = p_trg->get_parent_node( svid_trg);
      if ( !flags[pvid_trg]) continue;
      
      const Node_ID  &SVID_src = p_trg->get_subnode_counterpart( svid_trg);
      if ( !p_src || p_src->id()!=SVID_src.pane_id) 
	p_src = get_src_pane( SVID_src.pane_id);
      
      int svid_src = SVID_src.node_id;
      p_src->get_host_element_of_subnode(svid_src, ene_src, nc);

      Array_n  v = tDF.get_value( p, pvid_trg);

      if ( is_nodal(sDF.tag())) {
	Generic_element  e( ene_src.size_of_edges(), ene_src.size_of_nodes());
	interpolate( e, make_field( sDF, p_src, ene_src), nc, v);
      }
      else {
	interpolate( Generic_element(3), 
		     make_field( sDF, p_src, ene_src), nc, v);
      }
    }

    // If linear, we are done with this pane
    if ( !p_trg->is_quadratic()) continue;

    // Otherwise, we must interpolate values to other nodes. 
    // We now loop through the faces of the pane.
    ene = ENE( p_trg->base(),  1);
    for ( int k=1, size=p_trg->size_of_faces(); k<=size; ++k, ene.next()) {
      if ( !p_trg->need_recv(k)) continue; // Skip the face if not receiving 
      Element_var  f( tDF, p_trg->pointer( tDF.id()), ene);
      
      switch ( ene.size_of_nodes()) {
      case 6:
	f[3] = 0.5*(f[0]+f[1]);
	f[4] = 0.5*(f[1]+f[2]);
	f[5] = 0.5*(f[2]+f[0]); 
	break;
      case 9:
	f[8] = 0.25*(f[0]+f[1]+f[2]+f[3]); // Then continue as 8-nodes
      case 8:
	f[4] = 0.5*(f[0]+f[1]);
	f[5] = 0.5*(f[1]+f[2]);
	f[6] = 0.5*(f[2]+f[3]); 
	f[7] = 0.5*(f[3]+f[0]); 
	break;
      }
    }
  }

  trg.reduce_maxabs_to_all( tDF);

  if ( verbose) {
    // Output timing information
    trg.barrier();
    if ( trg.is_root())
      std:: cout << "RFACE: Interpolation done in " 
		 << get_wtime()-t0 << " seconds." << std::endl;
  }
}

// Computes the element-wise load vector and mass matrix.
template < class _Data, class _Points, class _Loads, class Tag>
void Transfer_base::
element_load_vector( const _Data &data_s,
		     const Generic_element &e_s,
		     const Generic_element &e_t,
		     const _Points &pnts_s,
		     const _Points &pnts_t,
		     const Point_2 *ncs_s,
		     const Point_2 *ncs_t,
		     const Real     alpha,
		     const int sne,
		     const Tag &tag,
		     _Loads loads,
		     _Loads diag,
		     Real  *emm_out,
		     int    doa,
		     bool   lump)
{
  const unsigned int n = e_t.size_of_nodes();

  Generic_element sub_e( sne);
  Vector_n vt(data_s.dimension(), 0); Array_n v( vt.begin(), vt.end());

  // Interpolate the coordinates.
  Point_3 ps_s[Generic_element::MAX_SIZE];
  Point_3 ps_t[Generic_element::MAX_SIZE];
  for ( int i=0; i<sne; ++i) e_t.interpolate( pnts_t, ncs_t[i], &ps_t[i]);
  if ( alpha!=1.)
    for ( int i=0; i<sne; ++i) e_s.interpolate( pnts_s, ncs_s[i], &ps_s[i]);

  // Set the default degree of accuracy for the quadrature rules.
  if ( is_nodal( data_s)) 
  { if ( doa == 0) doa = std::max(e_t.order(), e_s.order())==1? 2 : 4; }
  else doa=1;

  // Create a local buffer for element-mass-matrix. Note that only
  // lower triangle matrix is computed, as emm is symmetric.
  Real emm[Generic_element::MAX_SIZE*Generic_element::MAX_SIZE];
  bool compute_mass = diag.dimension()>0;
  if ( compute_mass) std::fill(emm, emm+n*n, 0.);

#ifdef SOBOLEV
  // Evaluate the gradient as well for Sobolev minimization
  std::vector<Real> esf(n*n,0.);
  std::vector<Vector_n> l_t(n,Vector_n(data_s.dimension(),0));
#endif

  Point_2 sub_nc, nc_s, nc_t;
  Real N[Generic_element::MAX_SIZE];
  // Loop through the quadrature points of the subelement
  for ( int i=0, ni=sub_e.get_num_gp(doa); i < ni; ++i) {
    sub_e.get_gp_nat_coor( i, sub_nc, doa);
    sub_e.interpolate( ncs_s, sub_nc, &nc_s);

    // Interploate to the quadrature point
    interpolate( e_s, data_s, nc_s, v);

    // Compute area of subelement in target element
    Real a_t=sub_e.get_gp_weight(i, doa), a_s=a_t;
    a_t *= sub_e.Jacobian_det( ps_t, sub_nc);

    // Compute area of subelement in source element. Use target area if alpha=1
    if ( alpha!=1.) a_s *= sub_e.Jacobian_det( ps_s, sub_nc);
    else a_s = a_t;

    v *= a_s;
    sub_e.interpolate( ncs_t, sub_nc, &nc_t);
    e_t.shape_func( nc_t, N);

#if SOBOLEV
    Vector_3 grads_t[Generic_element::MAX_SIZE];
    e_t.gradients( pnts_t, nc_t, grads_t);

    std::vector<Vector_3> load_prime( data_s.dimension(), NULL_VECTOR);

    Vector_3 grads_s[Generic_element::MAX_SIZE];
    if ( alpha!=1)
      e_s.gradients( pnts_s, nc_s, grads_s);
    else
      std::copy(grads_t,grads_t+n, grads_s);
    compute_load_prime( e_s, data_s, grads_s, &load_prime[0], tag);
#endif

    for ( unsigned int j=0; j<n; ++j) {
      loads[j] += N[j] * v;
      if ( compute_mass) {
	for ( unsigned int k=0; k<=j; ++k)
	  emm[j*n+k] += N[j]*N[k]*a_t;
      }
#if SOBOLEV
      for ( unsigned int k=0; k<data_s.dimension(); ++k)
	l_t[j][k] += grads_t[j]*load_prime[k]*area;

      if ( !lump && compute_mass) {
	for ( unsigned int k=0; k<=j; ++k) { 
	  // Weigh esf by the stretch ratio.
	  esf[j*n+k] += grads_t[j]*grads_t[k]*area;
	}
      }
#endif
    }
  }

#if SOBOLEV
  Real mu=-1;
  if ( mu<0 && !lump && compute_mass) {
    Real t1=0, t2=0;
    // Evaluate mu
    for ( unsigned int j=0; j<n; ++j) {
      for ( unsigned int k=0; k<j; ++k) {
	t1 += emm[j*n+k]*esf[j*n+k]; t2 += esf[j*n+k]*esf[j*n+k];
      }
    }
    mu = -t1 / t2;
  }

  if ( mu > 0) {
    // Add the stiffness to the mass matrix.
    for ( unsigned int j=0; j<n; ++j) {
      loads[j] += mu * l_t[j];

      if ( emm_out) {
	for ( unsigned int k=0; k<=j; ++k) { 
	  emm[j*n+k] += mu*esf[j*n+k];
	}
      }
    }
  }
#endif

  // Add emm onto emm_out and/or diag. Note that emm only saves lower triangle
  if ( compute_mass) {
    for ( unsigned int j=0; j<n; ++j) {
      // Update the diagonal 
      if ( lump) {
	// Lump the mass matrix	
	for ( unsigned int k=0; k<n; ++k) 
	  diag[j][0] += emm[std::max(j,k)*n+std::min(j,k)]; 
      }
      else // Otherwise, add only the diagonal entries
	diag[j][0] += emm[j*n+j];
    }
    
    // Update the element mass matrix. Adding lower triangle to the 
    // diagonal and upper triangle as well.
    if ( emm_out) {
      for ( unsigned int j=0; j<n; ++j) 
	for ( unsigned int k=0; k<n; ++k)
	  emm_out[j*n+k] += emm[std::max(j,k)*n+std::min(j,k)];
    }
  }
}

template < class _SDF>
void Transfer_base::
compute_load_vector_wra( const RFC_Pane_transfer *p_src,
			 RFC_Pane_transfer *p_trg,
			 const _SDF &sDF,
			 ENE  &ene_src,
			 ENE  &ene_trg,
			 int   sfid_src,
			 int   sfid_trg,
			 const Real alpha,
			 Nodal_data &rhs,
			 Nodal_data &diag,
			 int   doa,
			 bool  lump) 
{
  // Construct generic elements in parent source and target elements
  Generic_element e_src( ene_src.size_of_edges(), ene_src.size_of_nodes());
  Generic_element e_trg( ene_trg.size_of_edges(), ene_trg.size_of_nodes());

  // Obtain the local coordinates in source and target elements
  Point_2 ncs_src[3], ncs_trg[3];
  for ( int i=0; i<3; ++i) {
    p_src->get_nat_coor_in_element( sfid_src, i, ncs_src[i]);
    p_trg->get_nat_coor_in_element( sfid_trg, i, ncs_trg[i]);
  }

  // Construct enumerators for nodal vertices in source and target elements
  Nodal_coor_const nc;
  Element_coor_const pnts_s( nc, p_src->coordinates(), ene_src);
  Element_coor_const pnts_t( nc, p_trg->coordinates(), ene_trg);

  // Initialize pointer to element mass matrix. If a lumped mass matrix
  // is desired, then use a local buffer to store the element matrix.
  Real *pemm=NULL;

  bool needs_diag = diag.dimension()>0;
  // Compute the element mass matrix only needs_diag is true.
  if ( needs_diag)
    pemm = lump ? (Real*)NULL : p_trg->get_emm( ene_trg.id());
  
  // Invoke the implementation to compute load vector and mass matrix.
  element_load_vector( make_field( sDF, p_src, ene_src), 
		       e_src, e_trg, pnts_s, pnts_t, ncs_src, ncs_trg, 
		       alpha, 3, sDF.tag(),
		       make_field( rhs, p_trg, ene_trg), 
		       make_field( diag, p_trg, ene_trg),
		       pemm, doa, lump);
}

template < class _SDF>
void Transfer_base::
init_load_vector( const _SDF &sDF, 
		  const Real alpha, 
		  Nodal_data &rhs, 
		  Nodal_data &diag,
		  int doa, 
		  bool lump) 
{
  bool needs_diag = diag.dimension()>0;

  // First, initialize the entries of the target mesh to zero.
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *p = (*pit)->pointer( rhs.id());
    std::fill( p, p+(*pit)->size_of_nodes()*rhs.dimension(), Real(0));

    if ( needs_diag) {
      p = (*pit)->pointer( diag.id());
      std::fill( p, p+(*pit)->size_of_nodes()*diag.dimension(), Real(0));
    }
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
      p_src->get_host_element_of_subface( fid.face_id, ene_src);

      compute_load_vector_wra( p_src, *pit, sDF, ene_src, ene_trg, 
			       fid.face_id, i, alpha, rhs, diag, doa, lump);
    }
  }

  trg.reduce_to_all( rhs, MPI_SUM);
  if ( needs_diag) trg.reduce_to_all( diag, MPI_SUM);
}
  
template < class _SDF>
void Transfer_base::
transfer_2n( const _SDF &sDF, Nodal_data &tDF, const Real alpha,
	     Real *tol, int *iter, int doa, bool verbose) 
{
  double t0(0);

  if ( verbose) {
    trg.barrier(); t0 = get_wtime();
  }

  // Allocate buffers
  trg.init_nodal_buffers( tDF, (*iter>0)?7:3, (*iter>0));
  Nodal_data b( trg.nodal_buffer(0));
  Nodal_data z( trg.nodal_buffer(1));
  Nodal_data diag( trg.nodal_buffer(2));

  bool needs_source_coor = alpha!=1.;
  // Replicate the data of the source mesh (including coordinates if alpha!=1)
  src.replicate_data( sDF, needs_source_coor);

  bool lump = *iter<=0; // whether to lump mass matrix

  // Initialize the load vector and the diagonal vector. 
  init_load_vector( sDF, alpha, b, diag, doa, lump);

  // Obtaining an initial guess by interpolation.
  if ( !lump) interpolate_fe( sDF, tDF, false);

  // Clear up replicated data after obtaining the load vector and interpolation
  src.clear_replicated_data();

  if ( *iter>0) {
    Nodal_data p( trg.nodal_buffer(3));
    Nodal_data q( trg.nodal_buffer(4));
    Nodal_data r( trg.nodal_buffer(5));
    Nodal_data s( trg.nodal_buffer(6));

    int ierr=pcg( tDF, b, p, q, r, s, z, diag, tol, iter);
    
    if (ierr) {
      std::cerr << "***RFACE: WARNING: PCG did not converge after " 
		<< *iter << " iterations and relative error is "
		<< *tol << std::endl;
    }
  }
  else {
    precondition_Jacobi( b, diag, tDF);
  }
  
  trg.reduce_maxabs_to_all( tDF);

  // Delete buffer spaces
  trg.delete_nodal_buffers();

  if ( verbose) {
    trg.barrier();
    if ( trg.is_root()) {
      std:: cout << "RFACE: Transfer to nodes done in " 
		 << get_wtime()-t0 << " seconds";
      if ( *iter > 0)
	std:: cout << " with relative error " 
		   << *tol << " after " << *iter << " iterators"
		   << std::endl;
      else
	std:: cout << "." << std::endl;
    }
  }
}

void Transfer_n2n::
transfer( const Nodal_data_const &sv, Nodal_data &tv, const Real alpha, 
	  Real *tol, int *iter, int doa, bool verbose) 
{
  Base::transfer_2n( sv, tv, alpha, tol, iter, doa, verbose);
}

void Transfer_f2n::
transfer( const Facial_data_const &sf, Nodal_data &tv, const Real alpha, 
	  Real *tol, int *iter, int doa, bool verbose) 
{
  Base::transfer_2n( sf, tv, alpha, tol, iter, doa, verbose);
}

void Interpolator::
transfer( const Nodal_data_const &sv, Nodal_data &tv, bool verbose)  
{
  double t0=0.;
  if ( verbose) {
    trg.barrier(); t0=get_wtime();
  }

  src.replicate_data( sv, false); // replicate data but not the coordinates
  Base::interpolate_fe( sv, tv, verbose);
  src.clear_replicated_data();

  if ( verbose) {
    // Output timing information
    trg.barrier();
    if ( trg.is_root())
      std:: cout << "RFACE: Interpolation done in " 
		 << get_wtime()-t0 << " seconds." << std::endl;
  }
}

// This function computes the load vector.
template <class _SDF>
void Transfer_base::
loadtransfer( const _SDF &sDF, Nodal_data &tDF, const Real alpha, 
	      const int order, bool verbose) 
{
  double t0=0.;
  if ( verbose) {
    trg.barrier(); t0=get_wtime();
  }

  // Replicate the data of the source mesh (including coordinates if alpha!=1)
  bool needs_source_coor = alpha!=1.;
  src.replicate_data( sDF, needs_source_coor);

  Nodal_data dummy;
  init_load_vector( sDF, alpha, tDF, dummy, order, false);
  src.clear_replicated_data();

  if ( verbose) {
    // Output timing information
    trg.barrier();
    if ( trg.is_root())
      std:: cout << "RFACE: Load transfer done in " 
		 << get_wtime()-t0 << " seconds." << std::endl;
  }
}

void Transfer_n2n::
comp_loads( const Nodal_data_const &sv, Nodal_data &tv,
	    const Real alpha, const int order, bool verbose)
{
  Base::loadtransfer( sv, tv, alpha, order, verbose);
}

void Transfer_f2n::
comp_loads( const Facial_data_const &sv, Nodal_data &tv,
	    const Real alpha, const int order, bool verbose)
{
  Base::loadtransfer( sv, tv, alpha, order, verbose);
}

RFC_END_NAME_SPACE







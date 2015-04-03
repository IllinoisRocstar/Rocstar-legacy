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
// $Id: Transfer_base.C,v 1.23 2008/12/06 08:43:29 mtcampbe Exp $

//=====================================================================
// This file contains the implementation of Transfer_base for 
//      overlay-based data transfer algorithms.
// 
// Author: Xiangmin Jiao
// Revision: June 16, 2001
//=====================================================================

#include "Transfer_base.h"
#include <iostream>

RFC_BEGIN_NAME_SPACE

// This function solves the linear system A*x=b.
int 
Transfer_base::pcg( Nodal_data &x, Nodal_data &b, Nodal_data &p, 
		    Nodal_data &q, Nodal_data &r, Nodal_data &s, 
		    Nodal_data &z, Nodal_data &di, Real *tol, int *iter ) {
  Real  resid, alpha, beta, rho, rho_1=0, sigma=0;

  Real normb = norm2(b);
  Real tol_sq = *tol * *tol;
  
  // r = b - A*x
  multiply_mass_mat_and_x(x, r);
  saxpy( Real(1), b, Real(-1), r);
  
  if (normb < 1.e-15) normb = Real(1);
  
  if ( (resid = norm2(r) / normb) <= tol_sq) {
    *tol = sqrt(resid);
    *iter = 0;
    return 0;
  }
  
  for (int i = 1; i <= *iter; i++) {
    precondition_Jacobi( r, di, z);
    multiply_mass_mat_and_x(z, s);
    
    // rho = dot(r, z); sigma = dot(z, s);
    Real gsums[2];

    dot2(r, z, z, s, Array_n(gsums,2));
    rho = gsums[0];

    if (i==1) {
      copy_vec( z, p);
      copy_vec( s, q);
      sigma = gsums[1];
    }
    else {
      beta = rho / rho_1;
      // p = z + beta * p;
      saxpy( Real(1), z, beta, p);
      
      // q = s + beta * q; i.e., q = A*p;
      saxpy( Real(1), s, beta, q);
      sigma = gsums[1] - beta * beta * sigma;
    }
    alpha = rho / sigma;
    
    // x += alpha * p;
    saxpy( alpha, p, Real(1), x);
    // r -= alpha * q;
    saxpy( -alpha, q, Real(1), r);
    
    if ( (resid = norm2(r) / normb) <= tol_sq) {
      *tol = sqrt(resid);
      *iter = i;
      return 0;     
    }
    
    rho_1 = rho;
  }
  
  *tol = sqrt(resid);
  return 1;
}

void
Transfer_base::precondition_Jacobi( const Nodal_data_const &rhs, 
				    const Nodal_data_const &diag,
				    Nodal_data &x) {
  copy_vec( rhs, x); // Copy value from rhs to x

  // Divide x by diag.
  for (Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *p = (*pit)->pointer( x.id());
    const Real *q = (*pit)->pointer( diag.id());

    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      x.get_value( p, i) /= diag.get_value( q, i)[0];
  }
}

// This function evaluates a matrix-vector multiplication.
void 
Transfer_base::multiply_mass_mat_and_x( const Nodal_data_const &x, 
					Nodal_data &y) {
  // Loop through the elements of the target window to integrate
  //   \int_e N_iN_j de.
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    RFC_Pane_transfer *p_trg = *pit;
    Real *py = p_trg->pointer(y.id());
    const Real *px = p_trg->pointer(x.id());
    
    // Initialize y by setting all its entries to 0.
    Real *p = p_trg->pointer( y.id());
    std::fill( p, p+(*pit)->size_of_nodes()*y.dimension(), Real(0));

    // Loop through the faces of each pane.
    ENE ene( p_trg->base(),  1);
    for ( int k=1, size=p_trg->size_of_faces(); k<=size; ++k, ene.next()) {
      if ( !p_trg->need_recv(k)) continue;
      Real              *emm = p_trg->get_emm( k);
      Element_var        fy( y, py, ene);
      Element_var_const  fx( x, px, ene);

      // Add the submatrix onto the global matrix.
      for ( int i=0, n=ene.size_of_nodes(); i<n; ++i) {
	Array_n t = fy[i];
	for ( int j=0; j<n; ++j, ++emm) t += (*emm) * fx[j];
      }
    }
  }

  trg.reduce_to_all( y, MPI_SUM);
}

Real
Transfer_base::norm2( const Nodal_data_const &x) const {
  Real nrm(0);
  
  for ( Pane_iterator_const pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    const Real *p = (*pit)->pointer( x.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      if ( (*pit)->is_primary_node( i))
	nrm += square( x.get_value( p, i));
  }
  trg.allreduce( &nrm, MPI_SUM);
  return nrm;
}
  
Real
Transfer_base::dot( const Nodal_data_const &x, 
		    const Nodal_data_const &y) const {
  Real prod(0);
  
  for ( Pane_iterator_const pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    const Real *px = (*pit)->pointer( x.id());
    const Real *py = (*pit)->pointer( y.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      if ( (*pit)->is_primary_node( i))
	prod += x.get_value( px, i) * y.get_value( py, i);
  }
  
  trg.allreduce( &prod, MPI_SUM);
  return prod;
}

// Comput the products x1*y1 and x2*y2 and assign to prod[0] and prod[1].
void
Transfer_base::dot2( const Nodal_data_const &x1, 
		     const Nodal_data_const &y1,
		     const Nodal_data_const &x2, 
		     const Nodal_data_const &y2,
		     Array_n prods) const {
  prods[0] = prods[1] = 0;
  for ( Pane_iterator_const pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    const Real *px1 = (*pit)->pointer( x1.id());
    const Real *py1 = (*pit)->pointer( y1.id());
    const Real *px2 = (*pit)->pointer( x2.id());
    const Real *py2 = (*pit)->pointer( y2.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      if ( (*pit)->is_primary_node( i)) {
	prods[0] += x1.get_value( px1, i) * y1.get_value( py1, i);
	prods[1] += x2.get_value( px2, i) * y2.get_value( py2, i);
      }
  }
  
  trg.allreduce( prods, MPI_SUM);
}

void
Transfer_base::saxpy( const Real &a, const Nodal_data_const &x, 
		      const Real &b, Nodal_data &y) {
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *px = (*pit)->pointer( x.id());
    Real *py = (*pit)->pointer( y.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      (y.get_value( py, i)*=b) += a*x.get_value( px, i);
  }
}

// This function computes x = a*x
void
Transfer_base::scale( const Real &a, Nodal_data &x) {
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *px = (*pit)->pointer( x.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      x.get_value( px, i) *= a;
  }
}

// This function computes x = 1 / x
void
Transfer_base::invert( Nodal_data &x) {
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *px = (*pit)->pointer( x.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      x.get_value( px, i).invert();
  }
}

// This function computes y = a*x + b*y
void
Transfer_base::copy_vec(const Nodal_data_const &x, Nodal_data &y) {
  for ( Pane_iterator pit=trg_ps.begin(); pit!=trg_ps.end(); ++pit) {
    Real *px = (*pit)->pointer( x.id());
    Real *py = (*pit)->pointer( y.id());

    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i)
      y.set_value( py, i, x.get_value( px, i));
  }
}

//============== The following functions for transfering to faces
void 
Transfer_base::minmax( const RFC_Window_transfer &win,
		       const Nodal_data_const &sDF, 
		       Array_n &min_v, 
		       Array_n &max_v) {

  min_v = Vector_n( sDF.dimension(), HUGE_VAL);
  max_v = Vector_n( sDF.dimension(), -HUGE_VAL);
  
  std::vector<const RFC_Pane_transfer*> ps;
  win.panes( ps);
  
  for (std::vector<const RFC_Pane_transfer*>::iterator 
	 pit=ps.begin(); pit!=ps.end(); ++pit) {
    const Real *p = (*pit)->pointer( sDF.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_nodes(); i<=size; ++i) {
      Array_n_const t = sDF.get_value( p, i);
      for ( unsigned int k=0; k<t.dimension(); ++k) {
	if ( !isfinite(t[k])) {
	  std::cerr << "ERROR: ****Invalid number " 
		    << t[k] << " in " << win.name() << "[" 
		    << i-1 << "][" << k << "] (C Convention). Aborting..." << std::endl;
	  RFC_assertion( isfinite(t[k])); MPI_Abort( MPI_COMM_WORLD, -1);
	}
      }
      min_v = min( min_v, t);
      max_v = max( max_v, t);
    }
  }

  // Perform global reduction
  win.allreduce( min_v, MPI_MIN);
  win.allreduce( max_v, MPI_MAX);
}

void 
Transfer_base::minmax( const RFC_Window_transfer &win,
		       const Facial_data_const &sDF, 
		       Array_n &min_v, 
		       Array_n &max_v) {
  min_v = Vector_n( sDF.dimension(), HUGE_VAL);
  max_v = Vector_n( sDF.dimension(), -HUGE_VAL);
  
  std::vector<const RFC_Pane_transfer*> ps;
  win.panes( ps);
  
  for (std::vector<const RFC_Pane_transfer*>::iterator 
	 pit=ps.begin(); pit!=ps.end(); ++pit) {
    const Real *p = (*pit)->pointer( sDF.id());
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_faces(); i<=size; ++i) {
      Array_n_const t = sDF.get_value( p, i);
      for ( unsigned int k=0; k<t.dimension(); ++k) {
	if ( !isfinite(t[k])) {
	  std::cerr << "ERROR: ****Invalid number " 
		    << t[k] << " in " << win.name() << "[" 
		    << i-1 << "][" << k << "] (C Convention). Aborting..." << std::endl;
	  RFC_assertion( isfinite(t[k])); MPI_Abort( MPI_COMM_WORLD, -1);
	}
      }
      min_v = min( min_v, t);
      max_v = max( max_v, t);
    }
  }

  // Perform global reduction
  win.allreduce( min_v, MPI_MIN);
  win.allreduce( max_v, MPI_MAX);
}

// Integrate the values over the surface.
void 
Transfer_base::integrate( const RFC_Window_transfer &win,
			  const Facial_data_const &sDF, 
			  Array_n &integral,
			  const int doa) {
  integral = Vector_n( sDF.dimension(), 0);

  std::vector<const RFC_Pane_transfer*> ps;
  win.panes( ps);
  
  Vector_n vt(sDF.dimension(), 0); Array_n t( vt.begin(), vt.end());

  Point_2 nc; 
  for (std::vector<const RFC_Pane_transfer*>::iterator 
	 pit=ps.begin(); pit!=ps.end(); ++pit) {
    ENE ene( (*pit)->base(), 1);
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_faces(); i<=size; ++i, ene.next()) {
      Generic_element e( ene.size_of_edges(), ene.size_of_nodes());

      Nodal_coor_const coors;
      Element_coor_const  pnts( coors, (*pit)->coordinates(), ene);

      for ( int i=0, n = e.get_num_gp(doa); i<n; ++i) {
	e.get_gp_nat_coor( i, nc, doa);
	
	t = make_field( sDF, *pit, ene);
	Real a=e.get_gp_weight(i, doa) * e.Jacobian_det( pnts, nc);
	t *= a;
	integral += t;
      }
    }
  }

  // Perform global reduction
  win.allreduce( integral, MPI_SUM);
}

// Integrate the values over the surface.
void 
Transfer_base::integrate( const RFC_Window_transfer &win,
			  const Nodal_data_const &sDF, 
			  Array_n &integral,
			  const int doa) {
  integral = Vector_n( sDF.dimension(), 0);

  std::vector<const RFC_Pane_transfer*> ps;
  win.panes( ps);
  
  Vector_n vt(sDF.dimension(), 0); Array_n t( vt.begin(), vt.end());

  Point_2 nc; 
  for (std::vector<const RFC_Pane_transfer*>::iterator 
	 pit=ps.begin(); pit!=ps.end(); ++pit) {
    ENE ene( (*pit)->base(), 1);
    // Loop through the nodes of each pane.
    for ( int i=1, size=(*pit)->size_of_faces(); i<=size; ++i, ene.next()) {
      Generic_element e( ene.size_of_edges(), ene.size_of_nodes());

      Nodal_coor_const coors;
      Element_coor_const  pnts( coors, (*pit)->coordinates(), ene);

      for ( int i=0, n = e.get_num_gp(doa); i<n; ++i) {
	e.get_gp_nat_coor( i, nc, doa);
	
	interpolate( e, make_field( sDF, *pit, ene), nc, t);
	Real a=e.get_gp_weight(i, doa) * e.Jacobian_det( pnts, nc);
	t *= a;
	integral += t;
      }
    }
  }

  // Perform global reduction
  win.allreduce( integral, MPI_SUM);
}


RFC_END_NAME_SPACE







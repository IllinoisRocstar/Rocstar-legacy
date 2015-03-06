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
// $Id: Transfer_base.h,v 1.29 2008/12/06 08:43:27 mtcampbe Exp $

//=====================================================================
// This file contains the prototypes of Transfer_base for 
//      overlay-based data transfer algorithms.
// 
// Author: Xiangmin Jiao
// Revision: June 16, 2001
//=====================================================================

#ifndef __TRANSFER_BASE_H_
#define __TRANSFER_BASE_H_

#include "rfc_basic.h"
#include "Vector_n.h"
#include "RFC_Window_transfer.h"
#include "Timing.h"
#include <cmath>

RFC_BEGIN_NAME_SPACE

// The base implementation for all the transfer algorithms.
class Transfer_base {
public:
  typedef Element_node_enumerator                 ENE;

  typedef Field< Nodal_data, ENE>                 Element_var;
  typedef Field< const Nodal_data_const, ENE>     Element_var_const;
  typedef Field< const Nodal_coor_const, ENE>     Element_coor_const;

  typedef Transfer_base                                   Self;
  typedef std::vector<RFC_Pane_transfer*>::iterator       Pane_iterator;
  typedef std::vector<RFC_Pane_transfer*>::const_iterator Pane_iterator_const;
  typedef std::vector<const RFC_Pane_transfer*>
  ::const_iterator                                        Pane_const_iterator;

  Transfer_base( RFC_Window_transfer *s, RFC_Window_transfer *t)
    : src( *s), trg( *t), sc( s->color()), _src_pane(NULL), _trg_pane(NULL)
  { src.panes( src_ps); trg.panes( trg_ps); }

public:
  /** template function for transfering from nodes/faces to faces.
   *  \param sDF   Souce data
   *  \param tDF   Target data
   *  \param alpha Parameter to control interpolation of 
   *               coordinates between the input meshes
   *  \param doa   Degree of accuracy for quadrature rule to be used
   *  \param verb  Verbose level
   */
  template < class _SDF>
  void transfer_2f( const _SDF &sDF, Facial_data &tDF, const Real alpha,
		    int doa, bool verb);

  /** template function for transfering from nodes/faces to nodes.
   *  \param sDF   Souce data
   *  \param tDF   Target data
   *  \param alpha Parameter to control interpolation of 
   *               coordinates between the input meshes
   *  \param tol   Tolerance of iterative solver
   *  \param iter  Number of iterations of iterative solver.
   *  \param doa   Degree of accuracy for quadrature rule to be used
   *  \param verb  Verbose level
   */
  template < class _SDF>
  void transfer_2n( const _SDF &sDF, Nodal_data &tDF, const Real alpha,
		    Real *tol, int *iter, int doa, bool verb);
  
  /** Perform finite-element interpolation (non-conservative), assuming  
   *  source data has been replicated.
   *  \param sDF   Souce data
   *  \param tDF   Target data
   *  \param verb  Verbose level
   */
  template <class _SDF>
  void interpolate_fe( const _SDF &sDF, Nodal_data &tDF, bool verb);

  /** Computes the load vector. Arguments similar to transfer_2f. 
   *  \see transfer_2f, transfer_2n
   */
  template <class _SDF>
  void loadtransfer( const _SDF &vS, Nodal_data &vT, const Real alpha,
		     const int order, bool verb);

protected:
  // Integrating over a sub-face whose parent element in the source
  // window is the face incident on s.
  template < class _SDF>
  void integrate_subface( const RFC_Pane_transfer *p_src,
			  RFC_Pane_transfer *p_trg,
			  const _SDF &sDF,
			  ENE &ene_src,
			  ENE &ene_trg,
			  int sfid_src,
			  int sfid_trg,
			  const Real alpha,
			  Facial_data &tDF,
			  Facial_data &tBF,
			  int doa);

  // The following are helpers for transfer_to_nodes, where 
  // the geometry to be used is (1-alpha)*Source+alpha*Target. 

  /** Initialize load vector and the diagonal of the mass matrix.
   *  \param vS    Souce data
   *  \param alpha Parameter to control interpolation of 
   *               coordinates between input meshes
   *  \param ld    Load vector
   *  \param diag  Diagonal of corresponding mass matrix.
   *  \param doa   Degree of accuracy for quadrature rule to be used
   *  \param lump  Indicates whether to compute lump mass matrix for
   *               faster estimation of solution.
   */
  template < class _SDF>
  void init_load_vector( const _SDF &vS, const Real alpha,
			 Nodal_data &ld, Nodal_data &diag, 
			 int doa, bool lump);

  // This is a matrix-free solver that solves the equation M*x=ld,
  // where M is the mass matrix computed on the fly.
  int  pcg( Nodal_data &x, Nodal_data &b, Nodal_data &p, 
	    Nodal_data &q, Nodal_data &r, Nodal_data &s, 
	    Nodal_data &z, Nodal_data &di, Real *tol, int *max_iter);

  /// Diagonal (Jacobi) preconditioner
  /// \param rhs is the right-hand side of the system
  /// \param diag is the diagonal of the mass matrix.
  /// \param x is the solution vector
  void precondition_Jacobi( const Nodal_data_const &rhs,
			    const Nodal_data_const &diag,
			    Nodal_data &x);

  //============== Helper routines for cg==================
  // Multiply the mass matrix M with a vector X, and get y.
  void multiply_mass_mat_and_x( const Nodal_data_const &x, Nodal_data &y);
  
  Real square( const Array_n_const &x) const { return x*x; }

  Real norm2( const Nodal_data_const &x) const;
  Real dot( const Nodal_data_const &x, const Nodal_data_const &y) const;
  void dot2( const Nodal_data_const &x1, const Nodal_data_const &y1,
	     const Nodal_data_const &x2, const Nodal_data_const &y2,
	     Array_n prod) const;

  void scale( const Real &a, Nodal_data &x);
  void invert( Nodal_data &x);

  // This function computes y = a*x + b*y
  void copy_vec(const Nodal_data_const &x, Nodal_data &y);

  void saxpy( const Real &a, const Nodal_data_const &x, 
	      const Real &b, Nodal_data &y);

  /// Computes the element-wise load vector, and also computes mass matrix if 
  /// diag has a nonnegative ID. The mass matrix will be lumped if lump is true
  template < class _SDF>
  void 
  compute_load_vector_wra( const RFC_Pane_transfer *p_src, //< Source pane
			   RFC_Pane_transfer *p_dst, //< target pane
			   const _SDF &sDF, //< source data
			   ENE  &ene_src,   //< node enumerator of source element
			   ENE  &ene_trg,   //< node enumerator of target element
			   int   sfid_src,  //< parent face ID in source pane 
			   int   sfid_trg,  //< parent face ID in target pane 
			   const Real alpha, //< coordinate interpolation parameter
			   Nodal_data &rhs,  //< load vector
			   Nodal_data &diag, //< Diagonal of mass matrix.
			   int   doa,        //< Degree of accuracy of quadrature
			   bool  lump);      //< whether to lump mass matrix.
  
  /// Computes the element-wise load vector, and also computes mass matrix 
  /// if emm is not NULL.
  /// Note that loads and mass matrix are added to the arrays.
  template < class _Data, class _Points, class _Loads, class Tag>
  void 
  element_load_vector( const _Data &data_s,        //< Data field
		       const Generic_element &e_s, //< Master source element
		       const Generic_element &e_t, //< Master target element
		       const _Points &pnts_s,      //< Points of source element
		       const _Points &pnts_t,      //< Points of target element
		       const Point_2 *ncs_s,       //< Local coordinates of src
		       const Point_2 *ncs_t,       //< Local coordinates or trg
		       const Real     alpha,       //< coordinate interp para
		       const int sne,              //< #edges of sub-element
		       const Tag &tag,             //< Tag_facial/Tag_nodal
		       _Loads loads,               //< Load vector
		       _Loads diag,                //< Diagonal of mass matrix
		       Real  *emm,                 //< Element-wise mass matrix
		       int    doa,                 //< Degree of quadrature
		       bool   lump);               //< Whether to lump mass mat

  
  template < class _Data>
  void compute_load_prime( const Generic_element &e_s,
			   const _Data &data_s, const Vector_3 *grads_s,
			   Vector_3 *load_prime, Tag_nodal ) {
    Vector_3 NULL_VECTOR(0,0,0);
    for ( unsigned int k=0; k<data_s.dimension(); ++k) {
      load_prime[k] = NULL_VECTOR;
      for ( unsigned int j=0; j<e_s.size_of_nodes(); ++j) {
	load_prime[k] += data_s[j][k] * grads_s[j];
      }
    }
  }

  template < class _Data>
  void compute_load_prime( const Generic_element &e_s,
			   const _Data &data_s, const Vector_3 *grads_s,
			   Vector_3 *load_prime, Tag_facial ) {
    Vector_3 NULL_VECTOR(0,0,0);
    for ( unsigned int k=0; k<data_s.dimension(); ++k) {
      load_prime[k] = NULL_VECTOR;
    }
  }

public:
  // Useful utilities
  void minmax( const RFC_Window_transfer &win,
	       const Facial_data_const &sDF, 
	       Array_n &min_v, 
	       Array_n &max_v);
  
  void minmax( const RFC_Window_transfer &win,
	       const Nodal_data_const &sDF, 
	       Array_n &min_v, 
	       Array_n &max_v);

  void integrate( const RFC_Window_transfer &win,
		  const Nodal_data_const &sDF, 
		  Array_n &intergral,
		  const int doa);

  void integrate( const RFC_Window_transfer &win,
		  const Facial_data_const &sDF, 
		  Array_n &intergral,
		  const int doa);

#ifndef isfinite /* this is a macro under Intel CC 8.0 */
  bool isfinite( Real x) { return x>-HUGE_VAL && x<HUGE_VAL; }
#endif
private:
  // Interpolation
  template < class _Value>
  void 
  interpolate( const Generic_element &e, 
	       const Element_var_const values,
	       const Generic_element::Nat_coor &nc,
	       _Value &v)
  { e.interpolate( values, nc, &v); }
  
  template < class _Value>
  void
  interpolate( const Generic_element &e, 
	       const Array_n_const value,
	       const Generic_element::Nat_coor &nc,
	       _Value &v)
  { v = value; }

protected:
  // Data members
  RFC_Window_transfer          &src;
  RFC_Window_transfer          &trg;
  int                          sc;

private:
  // Caches for the pane
  const RFC_Pane_transfer  *_src_pane;
  RFC_Pane_transfer        *_trg_pane;

  std::vector<const RFC_Pane_transfer*>   src_ps;
  std::vector<RFC_Pane_transfer*>         trg_ps;


  const RFC_Pane_transfer *get_src_pane( int i) {
    if ( !_src_pane || _src_pane->id()) 
      _src_pane = &src.pane(i);
    return _src_pane;
  }
  RFC_Pane_transfer  *get_trg_pane( int i) {
    if ( !_trg_pane || _trg_pane->id()) 
      _trg_pane = &trg.pane(i);
    return _trg_pane;
  }

  /** Construct a element-wise accessor from nodal data and pointers */
  Element_var_const
  make_field( const Nodal_data_const &d, 
	      const RFC_Pane_transfer *pn, 
	      const ENE &ene) {
    return Element_var_const( d, pn->pointer(d.id()), ene);
  };
  
  Element_var
  make_field( Nodal_data &d, 
	      RFC_Pane_transfer *pn, 
	      const ENE &ene) {
    return Element_var( d, pn->pointer(d.id()), ene);
  };

  Element_var_const
  make_field( const Nodal_data_const &d, 
	      const RFC_Pane_transfer *pn, int) {
    RFC_assertion_msg(false, "Should never reach here. Bug in the code?"); 
    return Element_var_const( d, pn->pointer(d.id()), ENE());
  };

  const Array_n_const
  make_field(const Facial_data_const &d, 
	     const RFC_Pane_transfer *pn, 
	     const ENE &ene){
    return d.get_value( pn->pointer(d.id()), ene.id());
  };

  const Array_n_const
  make_field(const Facial_data_const &d, 
	     const RFC_Pane_transfer *pn, 
	     int i){
    return d.get_value( pn->pointer(d.id()), i);
  };

  bool is_nodal( Tag_nodal) const { return true; }
  bool is_nodal( Tag_facial) const { return false; }
  bool is_nodal( const Element_var_const&) const { return true; }
  bool is_nodal( const Element_var &) const { return true; }
  bool is_nodal( const Array_n_const &) const { return false; }
};

RFC_END_NAME_SPACE

#endif // __TRANSFER_BASE_H_







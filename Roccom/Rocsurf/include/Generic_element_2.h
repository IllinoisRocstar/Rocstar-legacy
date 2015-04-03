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
// $Id: Generic_element_2.h,v 1.3 2008/12/06 08:43:23 mtcampbe Exp $

/** \file Generic_element_2.h. Supports linear/quadratic
 *     triangle/quadrilateral element
 */
/* Author: Xiangmin Jiao
 * Date:   Sept. 12, 2002
 */

#ifndef _GENERIC_ELEMENT_2_H_
#define _GENERIC_ELEMENT_2_H_

/** Definition of Generic_element_2, which supports linear/quadratic
 *     triangle/quadrilateral elements. 
 *
 *  Linear/bilinear elements
 *     2 (0,1)             3 __________ 2 (1,1)
 *      |\                  |          |
 *      | \                 |          |
 *      |  \                |          |
 *      |   \               |          |
 *      |____\              |__________|
 *     0      1 (1,0)      0(0,0)       1
 *
 *  Quadratic elements
 *     2 (0,1)             3 ____6_____ 2 (1,1)     3 ____6_____ 2 (1,1)
 *      |\                  |          |       	     |          |
 *      | \                 |          |       	     |          |
 *     5|  \4              7|          |5      	    7|    8     |5
 *      |   \               |          |       	     |          |
 *      |____\              |__________|       	     |__________|
 *     0  3   1 (1,0)      0(0,0) 4     1 (0,1)	    0(0,0) 4     1 (0,1)
 */

#include "surfbasic.h"
#include <cassert>

SURF_BEGIN_NAMESPACE

/** This is a helper of Generic_element_2 for determining the base data 
 * type of a container that supports bracket operator for accessing
 * nodal data using element-wise node index.
 */
template < class Field>
struct Field_traits { 
  typedef typename Field::Value     Value; 
  typedef typename Field::Size      Size;
};

/// Specializatin of Field_traits for arrays.
template < class _V>
struct Field_traits<_V*> {
  typedef _V                         Value;
  typedef unsigned int               Size;
};

/// Specializatin of Field_traits for const arrays.
template < class _V>
struct Field_traits<const _V*> {
  typedef _V                         Value;
  typedef unsigned int               Size;
};

/** Encapsulation of the element-wise computations for two-dimensional
 *  elements. It supports linear/quadratic triangle/quadrilateral elements.
 *  It does not contain any geometric or field data but only defines the
 *  master element. Geometric or field data data should be provided as 
 *  parameters to the member functions as necessary.
 *  NOTE: We normalized the natural (local) coodinates so that they 
 *        always fall between 0 and 1.
 */
class Generic_element_2 {
public:
  typedef Generic_element_2              Self;
  typedef double                         Real;
  typedef unsigned int                   Size;
  typedef SURF::Vector_2<Real>           Vector_2;
  typedef SURF::Vector_3<Real>           Vector_3;
  typedef Vector_2                       Nat_coor;

  enum { MAX_SIZE = 9}; // MAX_NV is maximum number of nodes per element.

public:
  /** Constructor. 
   *  \param ne specifies the number of edges.
   *  \param nn specifies the number of nodes.
   */
  Generic_element_2( Size ne, Size nn=0);

  /// Number of edges
  Size size_of_edges() const { return _nedges; }
  /// Number of nodes
  Size size_of_nodes() const { return _nnodes; }
  /// Degree of the element. 1 for linear and 2 for quadratic.
  Size order() const { return _order; }
  
  /** \name Interpolation \{
   */
  /** Evaluates the shape functions of the element and output the
   *  barycentric coordinates into the array N.
   */
  void shape_func( const Nat_coor &nc, Real N[]) const;

  /// Evaluates the derivatives of the shape functions.
  void shape_func_deriv( const Nat_coor &nc, Vector_2 Np[]) const;

  /// Interpolates the field data at a given point.
  template < class Field, class Value>
  void interpolate( const Field &f, 
		    const Nat_coor &nc, Value *v) const throw(int);

  /// Customized interpolation for primitive data types.
  template < class Field>
  void interpolate( const Field &f, 
		    const Nat_coor &nc, Real *v) const throw(int)
  { interpolate_nopt( f, nc, v); }

  /// Interpolates the field data at a given point on an edge.
  template < class Field, class Value>
  void interpolate( const Field &f, 
		    const Real xi, Value *v) const throw(int);

  /// Customized interpolation for primitive data types.
  template < class Field>
  void interpolate( const Field &f, 
		    const Real xi, Real *v) const throw(int)
  { interpolate_nopt( f, xi, v); }

  /// Interpolates the field data at a given point and return the result.
  template < class Field>
  typename Field_traits<Field>::Value 
  interpolate( const Field &f, const Nat_coor &nc) const throw(int) {
    typename Field_traits<Field>::Value v;
    this->interpolate( f, nc, &v); // Use this-> to get around a stupid bug in SGI C++ compiler
    return v;
  }

  /// Interpolates the field data at a given point on an edge and return the result.
  template < class Field>
  typename Field_traits<Field>::Value 
  interpolate( const Field &f, const Real xi) const throw(int) {
    typename Field_traits<Field>::Value v;
    interpolate( f, xi, &v);
    return v;
  }

  /// Interpolates the field data to the center of the element.
  template < class Field, class Value>
  void interpolate_to_center( const Field &f, Value *v) const throw(int);

  /// Interpolates the field data to the center and return the value.
  template < class Field>
  typename Field_traits<Field>::Value 
  interpolate_to_center( const Field &f) const throw(int) {
    typename Field_traits<Field>::Value v;
    interpolate_to_center( f, &v);
    return v;
  }
  //\}

  /** \name Gradient
   */
  /// Evaluates the Jacobian at a given point.
  template < class Field>
  void Jacobian( const Field &f, const Nat_coor &nc, Vector_3 J[2]) const;

  // This function evaluates the Jacobian at a given point on an edge.
  template < class Field>
  void Jacobian( const Field &f, const Real xi, Vector_3 &v) const;

  /// Evaluates the gradient of the shape functions.
  template < class Pnts>
  void gradients( const Pnts &ps, const Nat_coor &nc, Vector_3 grads[3]) const;

  /// Evaluates the determinant of the Jacobian (dx/dxi,dx/deta).
  template < class Field>
  Real Jacobian_det( const Field &f, const Nat_coor &nc) const;

  /// Evaluates the determinant of the Jacobian (dx/dxi,dx/deta), where
  /// x is (1-alpha)*f1+alpha*f2. Used by Rocface.
  template < class Field>
  Real Jacobian_det( const Field &f1, const Field &f2, 
		     Real alpha, const Nat_coor &nc2) const;
  //\}

  /** \name Gaussian quadrature rules \{
   */
  /// Get the number of Gauss points.
  Size get_num_gp( const Size doa=0) const;

  /// Get the weight associated with a Gauss point.
  Real get_gp_weight( const Size i, const Size doa=0) const;

  /// Get the natrual coordinate associated with a Gauss point.
  void get_gp_nat_coor( const Size i, Nat_coor &nc, const Size doa=0) const;
  //\}

protected:
  // Solver M*x=b using pseudoinverse, where M is 2*3 matrix, b is 2 vector, 
  //   and x is 3 vector.
  void solve( const Vector_3 M[2], Vector_3 x[], const Vector_2 b[]) const;

  /// Nonoptimized version of interpolation.
  template < class Field, class Value>
  void interpolate_nopt( const Field &f, 
			 const Nat_coor &nc, Value *v) const throw(int);

  /// Nonoptimized version of interpolation.
  template < class Field, class Value>
  void interpolate_nopt( const Field &f, 
			 const Real xi, Value *v) const throw(int);

private:
  const Size   _nedges;
  const Size   _nnodes;       // Number of vertices.
  const Size   _order;
};

template < class Field, class Value>
void Generic_element_2::interpolate( const Field &f,
				     const Nat_coor &nc, 
				     Value *v) const throw(int) {
  const Real xi = nc[0], eta = nc[1];

  if ( eta == 0) // Interpolate on the edge.
  { interpolate( f, xi, v); return; }

  *v = f[0];
  switch ( _nnodes) {
  case 3:
    // *v = f[0] + xi*(f[1]-f[0]) + eta*(f[2]-f[0]);
    *v += ((f[1]-f[0])*=xi) += ((f[2]-f[0])*=eta);
    return;
  case 4: {
    //  *v = f[0] + xi * (1.-eta) * (f[1]-f[0]) 
    //    + eta * ( f[3]-f[0] + xi *(f[2]-f[3]));
    *v += ((f[1]-f[0]) *= (xi * (1.-eta))) 
       += ((f[3]-f[0]) *= eta)
       += ((f[2]-f[3]) *= xi*eta);
    return;
  }
  case 6: {
    const Real zeta=1.-xi-eta;

    //  *v = f[0] + xi*( (2.*xi-1.)*(f[1]-f[0]) + 4.*zeta*(f[3]-f[0])) +
    //    eta * ( (2.*eta-1.)*(f[2]-f[0]) + 4.*zeta*(f[5]-f[0])) +
    //    4.*xi*eta*(f[4]-f[0]);
    *v += ((f[1]-f[0]) *= xi * (2.*xi-1.))
       += ((f[3]-f[0]) *= 4.* xi * zeta)
       += ((f[2]-f[0]) *= eta * (2.*eta-1.))
       += ((f[5]-f[0]) *= 4. * eta * zeta)
       += ((f[4]-f[0]) *= 4.*xi*eta);
    return;
  }
  case 8: {
    const Real xi_minus = 1. - xi;
    const Real eta_minus = 1. - eta;

    //  *v = f[0] + xi * eta_minus * (f[1]-f[0]) +
    //     eta * ( f[3]-f[0] + xi *(f[2]-f[3])) -
    //     2.*xi*xi_minus*eta_minus*( (f[0]-f[4])+(f[1]-f[4])) -
    //     2.*xi*xi_minus*eta*( (f[2]-f[6])+(f[3]-f[6])) -
    //     2.*xi*eta*eta_minus*( (f[1]-f[5])+(f[2]-f[5])) -
    //     2.*xi_minus*eta*eta_minus*( (f[0]-f[7])+(f[3]-f[7]));

    *v += ((f[1]-f[0]) *= eta * (2.*eta-1.))
       += ((f[3]-f[0]) *= eta)
       += ((f[2]-f[3]) *= xi * eta)
       -= ((((f[0]-f[4])+=(f[1]-f[4])) *= 2.*xi*xi_minus*eta_minus)
	   += (((f[2]-f[6])+=(f[3]-f[6])) *= 2.*xi*xi_minus*eta)
	   += (((f[1]-f[5])+=(f[2]-f[5])) *= 2.*xi*eta*eta_minus)
	   += (((f[0]-f[7])+=(f[3]-f[7])) *= 2.*xi_minus*eta*eta_minus));
    return;
  }
  default:
    throw(-1);
  }
}

template < class Field, class Value>
void Generic_element_2::interpolate_nopt( const Field &f,
					  const Nat_coor &nc, 
					  Value *v) const throw(int) {
  const Real xi = nc[0], eta = nc[1];

  if ( eta == 0) // Interpolate on the edge.
  { interpolate_nopt( f, xi, v); return; }

  *v = f[0];
  switch ( _nnodes) {
  case 3:
    *v += ((f[1]-f[0])*xi) + ((f[2]-f[0])*eta);
    return;
  case 4: {
    *v += ((f[1]-f[0]) * (xi * (1.-eta))) 
       +  ((f[3]-f[0]) * eta)
       +  ((f[2]-f[3]) * xi*eta);
    return;
  }
  case 6: {
    const Real zeta=1.-xi-eta;

    *v += ((f[1]-f[0]) * xi * (2.*xi-1.))
       +  ((f[3]-f[0]) * 4.* xi * zeta)
       +  ((f[2]-f[0]) * eta * (2.*eta-1.))
       +  ((f[5]-f[0]) * 4. * eta * zeta)
       +  ((f[4]-f[0]) * 4.*xi*eta);
    return;
  }
  case 8: {
    const Real xi_minus = 1. - xi;
    const Real eta_minus = 1. - eta;

    *v += ((f[1]-f[0]) * eta * (2.*eta-1.))
       +  ((f[3]-f[0]) * eta)
       +  ((f[2]-f[3]) * xi * eta)
       -  ((((f[0]-f[4])+(f[1]-f[4])) * 2.*xi*xi_minus*eta_minus)
	   + (((f[2]-f[6])+(f[3]-f[6])) * 2.*xi*xi_minus*eta)
	   + (((f[1]-f[5])+(f[2]-f[5])) * 2.*xi*eta*eta_minus)
	   + (((f[0]-f[7])+(f[3]-f[7])) * 2.*xi_minus*eta*eta_minus));
    return;
  }
  default:
    throw(-1);
  }
}

template < class Field, class Value>
void Generic_element_2::interpolate( const Field &f,
				     const Real xi, 
				     Value *v) const throw(int) {
  *v = f[0];
  switch ( _nnodes) {
  case 3:
  case 4: 
    // *v = f[0] + xi*(f[1]-f[0]);
    *v += ((f[1]-f[0]) *= xi);
    return;
  case 6: {
    // *v = f[0] + xi*( (2.*xi-1.)*(f[1]-f[0]) + 4.*(1.-xi)*(f[3]-f[0]));
    *v += ((f[1]-f[0]) *= xi*(2.*xi-1.))
       += ((f[3]-f[0]) *= 4.*xi*(1.-xi));
    return;
  }
  case 8: {
    // *v = f[0] + xi * (f[1]-f[0]) - 2.*xi*(1.-xi)*( (f[0]-f[4])+(f[1]-f[4]));
    *v += ((f[1]-f[0]) *= xi)
       -= (((f[0]-f[4])+=(f[1]-f[4])) *= 2.*xi*(1.-xi));
    return;
  }
  default:
    throw(-1);
  }
}

template < class Field, class Value>
void Generic_element_2::interpolate_nopt( const Field &f,
					  const Real xi, 
					  Value *v) const throw(int) {
  *v = f[0];
  switch ( _nnodes) {
  case 3:
  case 4: 
    *v += ((f[1]-f[0]) * xi);
    return;
  case 6: {
    *v += ((f[1]-f[0]) * xi*(2.*xi-1.))
       +  ((f[3]-f[0]) * 4.*xi*(1.-xi));
    return;
  }
  case 8: {
    *v += ((f[1]-f[0]) * xi)
       -  (((f[0]-f[4])+(f[1]-f[4])) * 2.*xi*(1.-xi));
    return;
  }
  default:
    throw(-1);
  }
}

template < class Field, class Value>
void Generic_element_2::
interpolate_to_center( const Field &f, Value *v) const throw(int) {
  if ( _order == 1) {
    *v = f[0];
    for ( Size i=1; i<_nnodes; ++i) *v += f[i];
    *v /= _nnodes;
  }
  else {
    Real x = size_of_edges()==3 ? 1./3. : 1./2.;
    interpolate( f, Nat_coor(x, x), v);
  }
}

// This function evaluates the Jacobian at a given point.
template < class Field>
void Generic_element_2::Jacobian( const Field &f, 
				  const Nat_coor &nc, 
				  Vector_3 J[2]) const {
  switch (_nnodes) {
  case 3:
    J[0] = f[1]-f[0]; J[1] = f[2]-f[0];
    return;
  case 4: {
    const Real xi = nc[0], xi_minus = 1. - xi;
    const Real eta = nc[1], eta_minus = 1. - eta;

    //  J[0] = eta_minus * ( f[1] - f[0]) + eta * ( f[2] - f[3]);
    J[0] = ((f[1]-f[0]) *= eta_minus) += (( f[2]-f[3]) *= eta );
    //  J[1] = xi_minus * ( f[3] - f[0]) + xi * ( f[2] - f[1]);
    J[1] = ((f[3]-f[0]) *= xi_minus) += (( f[2]-f[1]) *= xi);
    return;
  }
  case 6: {
    const Real xi=nc[0], eta=nc[1], zeta=1.-xi-eta;

    // J[0] = (4.*xi -1.)*(f[1]-f[0]) +
    //   (4.*zeta-4.*xi)*(f[3]-f[0]) + 4.*eta*(f[4] -f[5]);
    J[0] = ((f[1]-f[0]) *= 4.*xi -1.)
        += ((f[3]-f[0]) *= 4.*zeta-4*xi)
        += ((f[4]-f[5]) *= 4.*eta);
    // J[1] = (4.*eta -1.)*(f[2]-f[0]) +
    //   (4.*zeta-4.*eta)*(f[5]-f[0]) + 4.*xi*(f[4] -f[3]);
    J[1] = ((f[2]-f[0]) *= 4.*eta -1.)
        += ((f[5]-f[0]) *= 4*zeta-4*eta)
        += ((f[4]-f[3]) *= 4.*xi);
    return;
  }
  case 8: {
    const Real xi = nc[0], xi_minus = 1. - xi;
    const Real eta = nc[1], eta_minus = 1. - eta;

    //  J[0] = eta_minus * ( f[1] - f[0]) + eta * ( f[2] - f[3]) -
    //    2.*eta_minus*(xi_minus-xi)*( (f[0]-f[4])+(f[1]-f[4])) -
    //    2.*eta*(xi_minus-xi)*( (f[2]-f[6])+(f[3]-f[6])) -
    //    2.*eta*eta_minus*( (f[1]-f[5])+(f[2]-f[5])-(f[0]-f[7])-(f[3]-f[7]));
    J[0] = ((f[1]-f[0]) *= eta_minus) += ((f[2]-f[3]) *= eta)
        -= ((((f[0]-f[4])+=(f[1]-f[4])) *= 2.*eta_minus*(xi_minus-xi))
	    += (((f[2]-f[6])+=(f[3]-f[6])) *= 2.*eta*(xi_minus-xi))
	    += (((f[1]-f[5])+=(f[2]-f[5])-=((f[0]-f[7])+=(f[3]-f[7])))
		*= 2.*eta*eta_minus));

    //  J[1] = xi_minus * ( f[3] - f[0]) + xi * ( f[2] - f[1]) -
    //    2.*xi*(eta_minus-eta)*( (f[1]-f[5])+(f[2]-f[5]))-
    //    2.*xi_minus*(eta_minus-eta)*( (f[0]-f[7])+(f[3]-f[7])) -
    //    2.*xi*xi_minus*( (f[2]-f[6])-(f[3]-f[6])-(f[0]-f[4])+(f[1]-f[4]));
    ((J[1] = f[3]-f[0]) *= xi_minus) += ((f[2]-f[1]) *= xi)
          -= ((((f[1]-f[5])+=(f[2]-f[5])) *= 2.*xi*(eta_minus-eta))
	      += (((f[0]-f[7])+=(f[3]-f[7])) *= 2.*xi_minus*(eta_minus-eta))
	      += (((f[2]-f[6])+=(f[1]-f[4])-=((f[3]-f[6])+=(f[0]-f[4])))
		  *= 2.*xi*xi_minus));
    return;
  }
  default:
    abort(); // Should never reach here
  }
}

// This function evaluates the Jacobian at a given point.
template < class Field>
void Generic_element_2::Jacobian( const Field &f, 
				  const Real xi,
				  Vector_3 &v) const {

  switch ( _nnodes) {
  case 3:
  case 4:
    v = f[1] - f[0]; return;
  case 6:
    // return (4.*xi -1.)*(f[1]-f[0]) + (4.-8.*xi)*(f[3]-f[0]);
    v = (((f[1]-f[0]) *= 4.*xi -1.) += ((f[3]-f[0]) *= 4.-8.*xi)); return;
  case 8:
    // return (f[1]-f[0]) - (2.-4.*xi)*( (f[0]-f[4])+(f[1]-f[4]));
    v = ((f[1]-f[0]) -= (((f[0]-f[4])+=(f[1]-f[4])) *= 2.-4*xi)); return;
  default:
    assert( false);
    v = Vector_3(0,0,0);
    return;
  }
}

// This function evaluates the gradient of the shape functions.
template < class Pnts>
void Generic_element_2::gradients( const Pnts &pnts, 
				   const Nat_coor &nc, 
				   Vector_3 grads[3]) const {
  Vector_3 J[2];
  Jacobian( pnts, nc, J);

  Vector_2 deriv[9];
  shape_func_deriv( nc, deriv);

  solve( J, grads, deriv);
}

// This function evaluates the determinant of the Jacobian (dx/dxi,dx/deta).
template < class Field>
Generic_element_2::Real 
Generic_element_2::Jacobian_det( const Field &f, 
				 const Nat_coor &nc) const {
  Vector_3 J[2];
  Jacobian( f, nc, J);
  
  return std::sqrt( Vector_3::cross_product( J[0], J[1]).squared_norm());
}

// Evaluates the determinant of the Jacobian (dx/dxi,dx/deta), where
// x is (1-alpha)*f1+alpha*f2. Used by Rocface.
template < class Field>
Generic_element_2::Real 
Generic_element_2::Jacobian_det( const Field &f1, const Field &f2, 
				 const Real alpha, const Nat_coor &nc) const {
  Vector_3 J1[2],J2[2];
  if ( alpha!=1.) {
    Jacobian( f1, nc, J1);
  }
  
  if ( alpha!=0.) {
    Jacobian( f2, nc, J2);
    if ( alpha!=1) {
      J2[0] = (1-alpha)*J1[0]+alpha*J2[0]; 
      J2[1] = (1-alpha)*J1[1]+alpha*J2[1]; 
    }
  }
  else {
    J2[0] = J1[0]; J2[1] = J1[1];
  }
  
  return std::sqrt( Vector_3::cross_product( J2[0], J2[1]).squared_norm());
}

SURF_END_NAMESPACE

#endif /*_GENERIC_ELEMENT_2_H_ */







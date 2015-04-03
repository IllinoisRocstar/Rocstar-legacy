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
// $Id: Generic_element_2.C,v 1.3 2008/12/06 08:43:23 mtcampbe Exp $

#include "Generic_element_2.h"
#include <cmath>
#include <cassert>
#include <algorithm>

SURF_BEGIN_NAMESPACE

Generic_element_2::Generic_element_2( Size ne, Size nn)
  : _nedges( ne), _nnodes( std::max(ne,nn)), _order( 1+ (_nedges!=_nnodes)) {
  assert( ne==3 || ne==4);
  assert( _nnodes==ne || _nnodes==ne+ne);
}


// This function evaluates the shape function of an element. It
// computes the barycentric coordinates from the natural coordinates.
void 
Generic_element_2::shape_func( const Nat_coor &nc, Real N[]) const {
  switch ( _nnodes) {
  case 3: {
    N[0] = 1. - nc[0] - nc[1];
    N[1] = nc[0];
    N[2] = nc[1];
    return;
  }
  case 4: {
    const Real xi = nc[0], xi_minus = 1. - xi;
    const Real eta = nc[1], eta_minus = 1. - eta;
    
    N[0] = xi_minus * eta_minus;
    N[1] = xi * eta_minus;
    N[2] = xi * eta;
    N[3] = xi_minus * eta;
    return;
  }
  case 6: {
    const Real xi=nc[0], eta=nc[1], zeta=1.-xi-eta;
    N[0] = 2*zeta*zeta-zeta;
    N[1] = 2*xi*xi-xi;
    N[2] = 2*eta*eta-eta;
    N[3] = 4*zeta*xi;
    N[4] = 4*xi*eta;
    N[5] = 4*eta*zeta;

    return;
  }
  case 8: {
    const Real xi = nc[0], xi_minus = 1. - xi;
    const Real eta = nc[1], eta_minus = 1. - eta;
    
    N[0] = xi_minus * eta_minus * (1. - 2.*xi - 2.*eta);
    N[1] = xi * eta_minus * (-1. + 2.*xi - 2.*eta);
    N[2] = xi * eta * (-3. + 2.*xi + 2.*eta);
    N[3] = xi_minus * eta * (-1. - 2.*xi + 2.*eta);
    N[4] = 4. * xi_minus * eta_minus * xi;
    N[5] = 4. * xi * eta_minus * eta;
    N[6] = 4. * xi * eta * xi_minus;
    N[7] = 4. * xi_minus * eta * eta_minus;

    return;
  }
  default:
    abort(); // Should never reach here
  }
}

// This function evaluates the shape function of an element. It
// computes the barycentric coordinates from the natural coordinates.
void 
Generic_element_2::shape_func_deriv( const Nat_coor &nc, Vector_2 Np[]) const {
  switch ( _nnodes) {
  case 3: {
    Np[0][0] = -1;   Np[0][1] = -1.;
    Np[1][0] = 1;    Np[1][1] = 0;
    Np[2][0] = 0;    Np[2][1] = 1;
    return;
  }
  case 4: {
    const Real xi = nc[0], xi_minus = 1. - xi;
    const Real eta = nc[1], eta_minus = 1. - eta;
    
    Np[0][0] = -eta_minus;   Np[0][1] = -xi_minus;
    Np[1][0] = eta_minus;    Np[1][1] = -xi;
    Np[2][0] = eta;          Np[2][1] = xi;
    Np[3][0] = -eta;         Np[3][1] = xi_minus;

    return;
  }
  case 6: {
    const Real xi=nc[0], eta=nc[1], zeta=1.-xi-eta;

    Np[0][0] = 1.-4.*zeta;   Np[0][1] = 1.-4.*zeta;
    Np[1][0] = 4.*xi-1;      Np[1][1] = 0;
    Np[2][0] = 0;            Np[2][1] = 4*eta-1;
    Np[3][0] = 4*(zeta-xi);  Np[3][1] = -4*xi;
    Np[4][0] = 4*eta;        Np[4][1] = 4*xi;
    Np[5][0] = -4*eta;       Np[5][1] = 4*(zeta-eta);

    return;
  }
  case 8: {
    const Real xi = nc[0], xi_minus = 1. - xi;
    const Real eta = nc[1], eta_minus = 1. - eta;
    
    Np[0][0] = eta_minus * ( -3+4*xi+2*eta);  Np[0][1] = xi_minus * ( -3+4*eta+2*xi);
    Np[1][0] = eta_minus * ( -1+4*xi-2*eta);  Np[1][1] = xi * (1-2*xi+4.*eta);
    Np[2][0] = eta * ( -3+4*xi+2*eta);        Np[2][1] = xi * ( -3+4*eta+2*xi);
    Np[3][0] = eta * ( 1 + 4*xi-2*eta);       Np[3][1] = xi_minus * ( -1-2*xi+4.*eta);
    Np[4][0] = eta_minus * (-8 * xi);         Np[4][1] = -4*xi*xi_minus;
    Np[5][0] = 4*eta_minus*eta;               Np[5][1] = xi*(-8*eta);
    Np[6][0] = eta * (-8 * xi);               Np[6][1] = 4*xi*xi_minus;
    Np[7][0] = -4*eta*eta_minus;              Np[7][1] = xi_minus*(-8*eta);

  }
  default:
    abort(); // Should never reach here
  }
}

Generic_element_2::Size 
Generic_element_2::get_num_gp( const Size doa) const {
  if ( std::max( doa, _order)==1) {
    return _nedges==3 ? 1 : 4;
  }
  else if ( std::max( doa, _order)==2) {
    return _nedges==3 ? 3: 9;
  }
  else {
    assert( std::max( doa, _order)==4 && _nedges==3);
    return _nedges==3 ? 6: 9;
  }
}

Generic_element_2::Real
Generic_element_2::get_gp_weight( const Size i, const Size doa) const {
  if ( std::max( doa, _order)==1) {
    return (_nedges == 3) ? 1./2. : 1./4.; 
  } 
  else if ( std::max( doa, _order)==2) {
    if ( _nedges==3) 
      return 1./6.;
    else {
      if (i==8) return 16./81.;
      else if (i<4) return 25./324.;
      else return 10./81.;
    }
  }
  else {
    assert( std::max( doa, _order)==4 && _nedges==3);

    return (i<3) ? 0.054975871827661 : 0.1116907948390055;
  }
}

// This function evaluates the natural coordinates of 
//  the Gaussian Qadrature points of an element.
void 
Generic_element_2 ::get_gp_nat_coor( const Size i, Nat_coor &nc, 
				     const Size doa) const {
  if ( std::max( doa, _order)==1) {
    if ( _nedges==3)
      nc = Nat_coor( 0.333333333333333, 0.333333333333333);
    else {
      const Real coors[4][2] = { {0.2113248654051871, 0.2113248654051871}, 
				 {0.2113248654051871, 0.7886751345948129},
				 {0.7886751345948129, 0.2113248654051871},
				 {0.7886751345948129, 0.7886751345948129}};
      nc = Nat_coor( coors[i][0], coors[i][1]);
    }
  }
  else if ( std::max( doa, _order)==2) {
    if ( _nedges == 3) {
      const Real coors[3][2] = { {0.666666666666667, 0.166666666666667}, 
				 {0.166666666666667, 0.666666666666667},
				 {0.166666666666667, 0.166666666666667}};
      nc = Nat_coor( coors[i][0], coors[i][1]);
    }
    else {
      const Real coors[9][2] = { {0.112701665379258, 0.112701665379258},
				 {0.887298334620742, 0.112701665379258},
				 {0.887298334620742, 0.887298334620742},
				 {0.112701665379258, 0.887298334620742},
				 {0.5,               0.112701665379258}, 
				 {0.887298334620742, 0.5              },
				 {0.5,               0.887298334620742},
				 {0.112701665379258, 0.5              },
				 {0.5,               0.5              }};
      nc = Nat_coor( coors[i][0], coors[i][1]);
    }
  }
  else {
    assert( std::max( doa, _order)==4 && _nedges==3);

    const Real coors[6][2] = { {0.816847572980459, 0.091576213509771}, 
			       {0.091576213509771, 0.816847572980459},
			       {0.091576213509771, 0.091576213509771},
			       {0.108103018168070, 0.445948490915965},
			       {0.445948490915965, 0.108103018168070},
			       {0.445948490915965, 0.445948490915965}};
    nc = Nat_coor( coors[i][0], coors[i][1]);
  }
}


// J contains row vectors, b and x contain column vectors
void Generic_element_2::
solve( const Vector_3 J[2], Vector_3 x[], const Vector_2 b[]) const {
  Real area_sq = Vector_3::cross_product( J[0], J[1]).squared_norm();
  Vector_2 t[2] = { Vector_2( J[1]*J[1]/area_sq, J[0]*J[1]/-area_sq),
		    Vector_2( J[1]*J[0]/-area_sq,  J[0]*J[0]/area_sq) };

  // Jinv = J'(JJ')^-1, containing column vectors
  Vector_3 Jinv[2] = { J[0]*t[0][0]+=J[1]*t[1][0], J[0]*t[0][1]+=J[1]*t[1][1]};

  for ( unsigned int i=0; i<size_of_nodes(); ++i) {
    x[i] = Jinv[0]*b[i][0] += Jinv[1]*b[i][1];
  }
}

SURF_END_NAMESPACE







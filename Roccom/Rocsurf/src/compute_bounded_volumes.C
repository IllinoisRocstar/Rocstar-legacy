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
// $Id: compute_bounded_volumes.C,v 1.6 2008/12/06 08:43:23 mtcampbe Exp $

/** \file compute_bounded_volumes.C
 *  This file contains implementation for computing the volume bounded 
 *  between two different locations of each face of the surface mesh of 
 *  a given window. Typically, the two locations of the surface correspond
 *  to the surface at two different snapshots of a simulation.
 */
/*  Author: Phillip Alexander
 *  Dates:  Dec. 13, 2002
 */

#include "Rocsurf.h"
#include "roccom_devel.h"
#include "Element_accessors.h"
#include "Generic_element_2.h"
#include <math.h>
#include <vector>

SURF_BEGIN_NAMESPACE

void arrange(Point_3<Real> ps_face[4],
             Point_3<Real> ps[8],
	     int           ind1,
	     int           ind2,
	     int           ind3,
	     int           ind4){
  ps_face[0] = ps[ind1];
  ps_face[1] = ps[ind2];
  ps_face[2] = ps[ind3];
  ps_face[3] = ps[ind4];
}

void arrange(Point_3<Real> ps_face[3],
             Point_3<Real> ps[8],
	     int           ind1,
	     int           ind2,
	     int           ind3){
  ps_face[0] = ps[ind1];
  ps_face[1] = ps[ind2];
  ps_face[2] = ps[ind3];
}


Real get_face_volume(Point_3<Real>  ps_face[], int ne) {

  Generic_element_2    e(ne);
  Vector_2<Real>       nc(0,0);
  Vector_3<Real>       J[2];
  int order = 1;

  int size = e.get_num_gp(order);
  Real volume = 0;
  for (int k = 0; k<size; k++){
    Real weight = e.get_gp_weight( k, order);
    e.get_gp_nat_coor(k, nc, order);

    Point_3<Real> x;
    e.interpolate(ps_face,nc,&x);

    e.Jacobian( ps_face,nc,J);
    // Get the normal to the face
    Vector_3<Real> n = Vector_3<Real>::cross_product(J[0],J[1]);

    Real t =  weight * (n*(Vector_3<Real>&)x);
    
    // compute the volume, and add to running total
    volume += t;
  }
  return volume;
}

void normalize_coor( Point_3<Real> ps_face[], int n, int k) {
  Vector_3<Real> c(0,0,0);

  for ( int i=0; i<n; ++i) {
    c += (Vector_3<Real>&)ps_face[i]; 
  }
  c /= k;
  
  for ( int i=0; i<n; ++i) {
    ps_face[i] -= c; 
  }
}

void Rocsurf::compute_bounded_volumes( const COM::Attribute *old_location,
				       const COM::Attribute *new_location,
				       COM::Attribute *element_volume,
				       void *flag) {

  assert( element_volume != NULL && element_volume ->size_of_components() == 1); 
  assert( element_volume->window() == old_location->window());
  assert( element_volume->window() == new_location->window());

  std::vector< COM:: Pane*> panes;
  element_volume->window() -> panes( panes);

  Point_3<Real>      ps_face4[4];
  Point_3<Real>      ps_face3[3];
  Point_3<Real>      ps[8];

  // ps is used as a container for both the old and new coordinates
  // the following ordering convention is used:
  // the coordinates of the new surface occupy indexes 0 to 3
  // the coordinates of the old surface occupy indexes 4 to 7 
  // both surfaces have coordinates listed in the same order
  //   i.e. ps[0] and ps[4] are new and old coordinates for the same node
  // under these conventions, ps[3] and ps[7] are not used for triangular faces

  std::vector< COM::Pane*>::const_iterator it = panes.begin();
  // Loop through the elements of the pane.
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    Real *ptr_ev = (Real *)(pane.attribute( element_volume->id())->pointer());
    const Point_3<Real> *ptr_new = 
      (const Point_3<Real>*)(pane.attribute( new_location->id())->pointer());
    const Point_3<Real> *ptr_old = 
      (const Point_3<Real>*)(pane.attribute( old_location->id())->pointer());

    Element_node_vectors_k_const<Point_3<Real> > ps_new, ps_old;

    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next(), ++ptr_ev) {
      if ( flag!=NULL && *ptr_ev==0.) continue;

      Real volume = 0;
      // if mesh is triangular, we have two triangular faces, 3 quadrilaterals
      ps_new.set( ptr_new, ene, 1);
      ps_old.set( ptr_old, ene, 1);

      ps[0]=ps_new[0];ps[1]=ps_new[1];ps[2]=ps_new[2];
      ps[4]=ps_old[0];ps[5]=ps_old[1];ps[6]=ps_old[2];

      if (ene.size_of_edges()==3) {
	  
	if ( ps[0]==ps[4] && ps[1]==ps[5] && ps[2]==ps[6]) 
	  { *ptr_ev = 0.; continue; }
	ps[3] = Point_3<Real>(0,0,0);
	normalize_coor( ps, 7, 6);

	// solid has two triangular faces
	arrange(ps_face3,ps,0,2,1);
	volume += get_face_volume(ps_face3, 3);
	arrange(ps_face3,ps,4,5,6);
	volume += get_face_volume(ps_face3, 3);
	// quadrilateral faces
	arrange(ps_face4,ps,0,4,6,2);
	volume += get_face_volume(ps_face4, 4);
      }
      else{  // if not triangular,  mesh is quadrilateral
	ps[3]=ps_new[3]; ps[7]=ps_old[3];
	if ( ps[0]==ps[4] && ps[1]==ps[5] && ps[2]==ps[6] && ps[3]==ps[7]) 
	  { *ptr_ev = 0.; continue; }
	normalize_coor( ps, 8, 8);
	
	arrange(ps_face4,ps,0,3,2,1);
	volume += get_face_volume(ps_face4, 4);
	arrange(ps_face4,ps,4,5,6,7);
	volume += get_face_volume(ps_face4, 4);
	arrange(ps_face4,ps,3,7,6,2);
	volume += get_face_volume(ps_face4, 4);
	arrange(ps_face4,ps,0,4,7,3);
	volume += get_face_volume(ps_face4, 4);
      }
      // Two quarilateral faces are the same in either case
      arrange(ps_face4,ps,1,2,6,5);
      volume += get_face_volume(ps_face4, 4);
      arrange(ps_face4,ps,0,1,5,4);
      volume += get_face_volume(ps_face4, 4);
      
      volume /= 3.;
      *ptr_ev = -volume;
    } 
  }
}

void Rocsurf::compute_swept_volumes( const COM::Attribute *location,
				     const COM::Attribute *disps,
				     COM::Attribute *element_volume,
				     void *flag) {

  assert( element_volume != NULL && element_volume ->size_of_components() == 1); 
  assert( element_volume->window() == location->window());
  assert( element_volume->window() == disps->window());

  std::vector< COM:: Pane*> panes;
  element_volume->window() -> panes( panes);

  Point_3<Real>      ps_face4[4];
  Point_3<Real>      ps_face3[3];
  Point_3<Real>      ps[8];

  // ps is used as a container for both the old and new coordinates
  // the following ordering convention is used:
  // the coordinates of the new surface occupy indexes 0 to 3
  // the coordinates of the old surface occupy indexes 4 to 7 
  // both surfaces have coordinates listed in the same order
  //   i.e. ps[0] and ps[4] are new and old coordinates for the same node
  // under these conventions, ps[3] and ps[7] are not used for triangular faces

  std::vector< COM::Pane*>::const_iterator it = panes.begin();
  // Loop through the elements of the pane.
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    Real *ptr_ev = (Real *)(pane.attribute( element_volume->id())->pointer());
    const Point_3<Real> *ptr_pos = 
      (const Point_3<Real>*)(pane.attribute( location->id())->pointer());
    const Vector_3<Real> *ptr_disp = 
      (const Vector_3<Real>*)(pane.attribute( disps->id())->pointer());

    Element_node_vectors_k_const<Point_3<Real> > pnts;
    Element_node_vectors_k_const<Vector_3<Real> > ds;

    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next(), ++ptr_ev) {
      if ( flag!=NULL && *ptr_ev==0.) continue;

      Real volume = 0;
      // if mesh is triangular, we have two triangular faces, 3 quadrilaterals
      ds.set( ptr_disp, ene, 1);
      pnts.set( ptr_pos, ene, 1);

      ps[0]=pnts[0]+ds[0];ps[1]=pnts[1]+ds[1];ps[2]=pnts[2]+ds[2];
      ps[4]=pnts[0];ps[5]=pnts[1];ps[6]=pnts[2];

      if (ene.size_of_edges()==3) {
	  
	if ( ps[0]==ps[4] && ps[1]==ps[5] && ps[2]==ps[6]) 
	  { *ptr_ev = 0.; continue; }
	ps[3] = Point_3<Real>(0,0,0);
	normalize_coor( ps, 7, 6);

	// solid has two triangular faces
	arrange(ps_face3,ps,0,2,1);
	volume += get_face_volume(ps_face3, 3);
	arrange(ps_face3,ps,4,5,6);
	volume += get_face_volume(ps_face3, 3);
	// quadrilateral faces
	arrange(ps_face4,ps,0,4,6,2);
	volume += get_face_volume(ps_face4, 4);
      }
      else{  // if not triangular,  mesh is quadrilateral
	ps[3]=pnts[3]+ds[3]; ps[7]=pnts[3];
	if ( ps[0]==ps[4] && ps[1]==ps[5] && ps[2]==ps[6] && ps[3]==ps[7]) 
	  { *ptr_ev = 0.; continue; }
	normalize_coor( ps, 8, 8);
	
	arrange(ps_face4,ps,0,3,2,1);
	volume += get_face_volume(ps_face4, 4);
	arrange(ps_face4,ps,4,5,6,7);
	volume += get_face_volume(ps_face4, 4);
	arrange(ps_face4,ps,3,7,6,2);
	volume += get_face_volume(ps_face4, 4);
	arrange(ps_face4,ps,0,4,7,3);
	volume += get_face_volume(ps_face4, 4);
      }
      // Two quarilateral faces are the same in either case
      arrange(ps_face4,ps,1,2,6,5);
      volume += get_face_volume(ps_face4, 4);
      arrange(ps_face4,ps,0,1,5,4);
      volume += get_face_volume(ps_face4, 4);
      
      volume /= 3.;
      *ptr_ev = -volume;
    } 
  }
}

void Rocsurf::compute_center( const COM::Attribute *mesh,
			      Vector_3<Real> &cnt) {
  std::vector< const COM:: Pane*> panes;
  mesh->window() -> panes( panes);

  struct Accumulator {
    Accumulator() : cnt(Vector_3<Real>(0,0,0)), count(0) {}
    
    Vector_3<Real> cnt;
    double  count;
  };
  Accumulator global;

  std::vector< const COM::Pane*>::const_iterator it = panes.begin();
  // Loop through the elements of the pane.
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    const Vector_3<Real> *ptr = 
      (const Vector_3<Real>*)(pane.attribute( COM::COM_NC)->pointer());

    for ( int j=pane.size_of_real_nodes(); j>0; --j, ++ptr) {
      global.cnt += *ptr;
      global.count += 1.;
    }
  }

  // Perform reduction on cnt and count
  Accumulator local=global;

  if ( COMMPI_Initialized()) {
    MPI_Allreduce( &local.cnt[0], &global.cnt[0], 4, MPI_DOUBLE, MPI_SUM, 
		   mesh->window()->get_communicator());
  }
  cnt = global.cnt/global.count;
}


void Rocsurf::
compute_signed_volumes( const COM::Attribute *mesh, double *vol) {
  std::vector< const COM:: Pane*> panes;
  mesh->window() -> panes( panes);

  Vector_3<Real> cnt;
  // First, obtain the center
  compute_center( mesh, cnt);

  Point_3<Real>      normalized_face[4];

  *vol = 0;
  std::vector< const COM::Pane*>::const_iterator it = panes.begin();
  // Loop through the elements of the pane.
  for (int i=0, local_npanes = panes.size(); i<local_npanes; ++i, ++it){ 
    const COM::Pane &pane = **it; 
    const Point_3<Real> *ptr = 
      (const Point_3<Real>*)(pane.attribute( COM::COM_NC)->pointer());

    Element_node_vectors_k_const<Point_3<Real> > ps;

    Element_node_enumerator ene( &pane, 1); 
    for ( int j=pane.size_of_elements(); j>0; --j, ene.next()) {
      ps.set( ptr, ene, 1);

      normalized_face[0]=ps[0]-cnt; 
      normalized_face[1]=ps[1]-cnt;
      normalized_face[2]=ps[2]-cnt; 

      if (ene.size_of_edges()==3) {
	*vol += get_face_volume(normalized_face, 3);
      }
      else{  // if not triangular,  element is quadrilateral
	normalized_face[3]=ps[3]-cnt;
	*vol += get_face_volume(normalized_face, 4);
      }
    } 
  }
      
  // Divide the signed volume by 3.
  *vol /= 3.;
}


SURF_END_NAMESPACE







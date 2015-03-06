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
// $Id: Rocsurf.h,v 1.12 2008/12/06 08:43:23 mtcampbe Exp $

/** \file Rocsurf.h
 */
/*
 *  Las Modified:  Dec. 13, 2002
 */

#ifndef __ROCSURF_H_
#define __ROCSURF_H_

#include "surfbasic.h"
#include "roccom_devel.h"

SURF_BEGIN_NAMESPACE

class Window_manifold_2;

class Rocsurf : public COM_Object {
public:
  // protected:
  Rocsurf() : _wm(NULL), _cookie(SURF_COOKIE) {}
  virtual ~Rocsurf();

  /// Loads Rocsurf onto Roccom with a given module name.
  static void load( const std::string &mname);
  /// Unloads Rocsurf from Roccom.
  static void unload( const std::string &mname);

  /// Interpolates nodal coordinates to element centers.
  static void interpolate_to_centers( const COM::Attribute *x, 
				      COM::Attribute *z);

  /// Integrate a function given by an elemental attribute over surface
  /// z is an array of size equal to number of components of x
  static void integrate( const COM::Attribute *x, double *z);

  /** Computes the area of each face of the surface mesh of window
   *  areas->window and saves the results in attribute areas.
   *  If pnts is present, then use it in place of the nodal coordinates
   */
  static void compute_element_areas( COM::Attribute *areas, 
				     const COM::Attribute *pnts=NULL);

  /// Computes elemental normals of a given window. Normalize the 
  /// normals if to_normalize is NULL (default) or its value is nonzero.
  /// If pnts is present, then use it in place of the nodal coordinates
  static void compute_element_normals( COM::Attribute *nrm,
				       const int *to_normalize=NULL, 
				       const COM::Attribute *pnts=NULL);

  /** Computes the volume bounded between two different locations of
   *  each face of the surface mesh of window volumes->window. Typically, 
   *  the two locations of the surface correspond to the surface at two
   *  different snapshots of a simulation.
   *
   *  \param old_location stores the old nodal coordinates.
   *  \param new_location stores the new nodal coordinates.
   *  \param volumes stores the bounded volume for each element.
   *  \param If flag is present, then only compute volume for elements whose 
   *    corresponding value of the volume attribute was set to nonzero value. 
   */
  static void compute_bounded_volumes( const COM::Attribute *old_location,
				       const COM::Attribute *new_location,
				       COM::Attribute *volumes,
				       void *flag=NULL);

  /** Computes the swept volume by a given displacement of each face of 
   *  the surface mesh of window volumes->window. 
   *
   *  \param old_location stores the nodal coordinates.
   *  \param disps stores the nodal displacement.
   *  \param volumes stores the swept volume for each element.
   *  \param If flag is present, then only compute volume for elements whose 
   *    corresponding value of the volume attribute was set to nonzero value. 
   */
  static void compute_swept_volumes( const COM::Attribute *location,
				     const COM::Attribute *disps,
				     COM::Attribute *volumes,
				     void *flag=NULL);

  /** Computes the center of a body.
   */
  static void compute_center( const COM::Attribute *mesh,
			      Vector_3<double> &cnt);

  /** Computes the signed volume of a body.
   */
  static void compute_signed_volumes( const COM::Attribute *mesh,
				      double *vol);

  /// Obtain a reference to the manifold.
  virtual Window_manifold_2 *manifold() { return _wm; }

  /// Constructs the communication patterns of a distributed mesh.
  /// If the input is pmesh, then use the given pconn.
  /// Otherwise, compute pconn.
  void initialize( const COM::Attribute *pmesh);

  /// Computes nodal or elemental normals of a given window
  void compute_normals( const COM::Attribute *mesh,
			COM::Attribute *nrm,
			const int *scheme=NULL);

  /// Computes nodal or elemental normals of a given window
  void compute_mcn( COM::Attribute *mcn, COM::Attribute *lbmcn);

  /// Serialize the mesh of a given window.
  void serialize_mesh( const COM::Attribute *inmesh, COM::Attribute *outmesh);

  /// Computes edge lengths of a given window.
  void compute_edge_lengths( double *lave, double *lmin, double *lmax);
  
  /// Computes nodal or elemental normals of a given window
  void elements_to_nodes( const COM::Attribute *elem_vals,
			  COM::Attribute *nodal_vals,
			  const COM::Attribute *mesh=NULL,
			  const int *scheme=NULL,
			  const COM::Attribute *elem_weights=NULL,
			  COM::Attribute *nodal_weights=NULL);

protected:

  template <class T>
  static void normalize( T *a, int size) throw(int) {
    T tmp(0);
    for (int i=0; i<size; ++i) tmp += a[i]*a[i]; 
    
    if ( tmp == 0) return;
    tmp = std::sqrt(tmp);
    for (int i=0; i<size; ++i) a[i] /= tmp;
  }

  int validate_object() const {
    if ( _cookie != SURF_COOKIE) return -1;
    else return COM_Object::validate_object();
  }
  
protected:
  enum { SURF_COOKIE=7627873};
  Window_manifold_2   *_wm;
  static const int scheme_vals[];
  int _cookie;
};

SURF_END_NAMESPACE

#endif







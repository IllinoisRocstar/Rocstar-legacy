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
// $Id: Pane_boundary.h,v 1.17 2008/12/06 08:43:20 mtcampbe Exp $

/** \file Pane_boundary.h
 * Utility for detecting boundaries of a pane.
 */
/* Author: Xiangmin Jiao
 * Date:   Aug. 26, 2002
 */

#ifndef _PANE_BOUNDARIES_H_
#define _PANE_BOUNDARIES_H_

#include "roccom_devel.h"
#include "mapbasic.h"
#include "Simple_manifold_2.h"
#include <vector>

MAP_BEGIN_NAMESPACE

class Pane_boundary {
public:
  typedef int                                  Node_ID;
  typedef int                                  Element_ID;
  typedef std::pair<Node_ID, Node_ID>          Node_pair;

  /// Constructors
  Pane_boundary( const COM::Pane *p) : _pane(*p),_pm(NULL) {}

  /// Constructors
  Pane_boundary( const Simple_manifold_2 *pm) 
    : _pane(*pm->pane()),_pm(pm) {}

  /// Determine the border nodes (excluding isolated nodes)
  void determine_border_nodes( std::vector<bool> &is_border,
			       std::vector<bool> &is_isolated,
			       std::vector<Facet_ID > *b=NULL,
			       int ghost_level=0) throw(int);

  /// Compute the minimum squared edge length of given edges.
  double min_squared_edge_len( const std::vector<Facet_ID > &) throw(int);

  /** Determine the nodes at pane boundaries of a given mesh. 
   *  The argument isborder must be a nodal attribute of integer type. 
   *  At return, isborder is set to 1 for border nodes, and 0 for others */
  static void determine_borders( const COM::Attribute *mesh,
				 COM::Attribute *isborder,
				 int ghost_level=0);

protected:
  /// Determine the border nodes for a 3-D mesh.
  /// A ghost level of 0 returns all real border nodes
  /// A ghost level of 1,2,3 returns the border nodes for 
  /// the 1st, 2nd, 3rd... ghost layer (structured meshes)
  /// A ghost level > 0 returns the border ghost nodes 
  /// (unstructured meshes)
  void determine_border_nodes_3( std::vector<bool> &is_border,
				 std::vector<Facet_ID > *b,
				 int ghost_level) throw(int);

  /// Determine the isolated nodes (i.e. not belonging to any element)
  void determine_isolated_nodes( std::vector<bool> &is_isolated,
				 int ghost_level) throw(int);

private:
  const COM::Pane   &_pane;
  const Simple_manifold_2 *const _pm;   // Manifold for determing border nodes
};

MAP_END_NAMESPACE

#endif /* _PANE_BOUNDARIES_H_ */







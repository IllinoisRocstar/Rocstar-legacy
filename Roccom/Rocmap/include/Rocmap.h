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
// $Id: Rocmap.h,v 1.20 2009/08/27 14:04:49 mtcampbe Exp $

/** \file Rocmap.h
 */
/*
 *  Las Modified:  Dec. 13, 2002
 */

#ifndef __ROCMAP_H_
#define __ROCMAP_H_

#include "mapbasic.h"
#include "roccom_devel.h"

MAP_BEGIN_NAMESPACE

class Rocmap {
public:
  Rocmap() {}

  /// Loads Rocmap onto Roccom with a given module name.
  static void load( const std::string &mname);
  /// Unloads Rocmap from Roccom.
  static void unload( const std::string &mname);

  /** Compute pane connectivity map between shared nodes.
   *  If pconn was not yet initialized, this routine will allocate memory
   *  for it. Otherwise, this routine will copy up to 
   *  the capacity of the array */
  static void compute_pconn( const COM::Attribute *mesh, 
			     COM::Attribute *pconn);
  
  /** Determine the nodes at pane boundaries of a given mesh. 
   *  The argument isborder must be a nodal attribute of integer type. 
   *  At return, isborder is set to 1 for border nodes, and 0 for others */
  static void pane_border_nodes( const COM::Attribute *mesh, 
				 COM::Attribute *isborder,
				 int *ghost_level=NULL);
  
  /// Get the number of communicating panes.
  static void size_of_cpanes( const COM::Attribute *pconn, const int *pane_id,
			      int *npanes_total, int *npanes_ghost=NULL);

  /// Perform an average-reduction on the shared nodes for the given attribute.
  static void reduce_average_on_shared_nodes(COM::Attribute *att, COM::Attribute *pconn=NULL);

  /// Perform a maxabs-reduction on the shared nodes for the given attribute.
  static void reduce_maxabs_on_shared_nodes(COM::Attribute *att, COM::Attribute *pconn=NULL);

  /// Perform a minabs-reduction on the shared nodes for the given attribute.
  static void reduce_minabs_on_shared_nodes(COM::Attribute *att, COM::Attribute *pconn=NULL);

  /// Update ghost nodal or elemental values for the given attribute.
  static void update_ghosts(COM::Attribute *att, 
			    const COM::Attribute *pconn=NULL);
  
};

MAP_END_NAMESPACE

#endif







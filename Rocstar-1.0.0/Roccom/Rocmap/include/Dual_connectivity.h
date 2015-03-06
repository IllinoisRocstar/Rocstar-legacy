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
// $Id: Dual_connectivity.h,v 1.9 2008/12/06 08:43:20 mtcampbe Exp $

#ifndef __DUAL_CONNECTIVITY_H
#define __DUAL_CONNECTIVITY_H

#include "mapbasic.h"
#include "roccom_devel.h"

MAP_BEGIN_NAMESPACE

/// Constructs the dual connectivity for the whole pane (including ghost
/// nodes and elements), which contains information about
/// incident elements for each node.
class Pane_dual_connectivity {
public:
  /// Constructs the dual connectivity for a given pane.
  explicit Pane_dual_connectivity( const COM::Pane *p, bool with_ghost=true);

  /// Obtain the IDs of the elements incident on a given node
  void incident_elements( int node_id, std::vector<int>& elists);

protected:
  /// Construct dual connectivity for 2-D structured meshes
  void construct_connectivity_str_2();
  /// Construct dual connectivity for unstructured meshes
  void construct_connectivity_unstr();

private:
  const COM::Pane     &_pane;      // Pane object
  bool                 _with_ghost;// Whether to include ghost nodes/elements
  std::vector< int>    _offsets;   // The offsets in _eids for each node
  std::vector< int>    _eids;      // The incident element ids for all nodes
};

MAP_END_NAMESPACE

#endif /* __DUAL_CONNECTIVITY_H */







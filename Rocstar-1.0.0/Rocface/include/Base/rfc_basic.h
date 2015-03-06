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
// $Id: rfc_basic.h,v 1.15 2008/12/06 08:43:26 mtcampbe Exp $

#ifndef RFC_BASIC_H
#define RFC_BASIC_H

#define RFC_BEGIN_NAME_SPACE  namespace RFC {
#define RFC_END_NAME_SPACE    }

#define USE_RFC_NAME_SPACE using namespace RFC;

#include <CGAL/Simple_cartesian.h>
#include "../Rocsurf/include/surfbasic.h"
#include "../Rocsurf/include/Generic_element_2.h"
#include <vector>

RFC_BEGIN_NAME_SPACE

typedef double                                   Real;
typedef SURF::Point_3<Real>                      Point_3;
typedef SURF::Vector_3<Real>                     Vector_3;
typedef SURF::Vector_2<Real>                     Point_2;
typedef SURF::Vector_2<Real>                     Vector_2;
typedef SURF::Point_2<float>                     Point_2S;
typedef SURF::Generic_element_2                  Generic_element;

enum Color       { NONE=-1, BLUE=0, GREEN=1, OVERLAY=2 };
enum Parent_type { PARENT_VERTEX=0, PARENT_EDGE=1, 
		   PARENT_FACE=2, PARENT_NONE=3};

template <class _TT> 
inline void
free_vector( std::vector< _TT> &v) { std::vector<_TT> t; v.swap(t); }

using CGAL::Bbox_3;
using CGAL::IO;
using CGAL::set_ascii_mode;
using CGAL::set_binary_mode;
using CGAL::Tag_false;
using CGAL::Tag_true;

#define RFC_precondition    CGAL_precondition
#define RFC_postcondition   CGAL_postcondition
#define RFC_assertion       CGAL_assertion
#define RFC_postcondition   CGAL_postcondition
#define RFC_assertion_msg   CGAL_assertion_msg
#define RFC_assertion_code  CGAL_assertion_code

RFC_END_NAME_SPACE

#endif // RFC_BASIC_H







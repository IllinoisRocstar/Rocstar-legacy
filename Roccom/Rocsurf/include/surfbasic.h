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
// $Id: surfbasic.h,v 1.4 2008/12/06 08:43:23 mtcampbe Exp $

#ifndef __SURF_BASIC_H_
#define __SURF_BASIC_H_

#define SURF_BEGIN_NAMESPACE  namespace SURF {
#define SURF_END_NAMESPACE    }
#define USE_SURF_NAMESPACE    using namespace SURF;

#include "../Rocmap/include/mapbasic.h"
#include <cstdlib>
#include "Element_accessors.h"

SURF_BEGIN_NAMESPACE

using COM::Element_node_enumerator;
using COM::Element_node_enumerator_str_2;
using COM::Element_node_enumerator_uns;
using COM::Facet_node_enumerator;
using COM::Element_vectors_k_const;
using COM::Element_vectors_k;
using COM::Element_node_vectors_k_const;
using COM::Element_node_vectors_k;

using MAP::Origin;
using MAP::Null_vector;
using MAP::Vector_3;
using MAP::Point_3;
using MAP::Vector_2;
using MAP::Point_2;

typedef double Real;

// Modes of element_to_nodes.
enum { E2N_USER=0, E2N_ONE=1, E2N_AREA=2, E2N_ANGLE=3, E2N_SPHERE=4};

SURF_END_NAMESPACE

#endif







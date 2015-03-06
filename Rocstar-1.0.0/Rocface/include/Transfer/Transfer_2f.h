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
// $Id: Transfer_2f.h,v 1.6 2008/12/06 08:43:27 mtcampbe Exp $

//===================================================================
// This file contains the prototypes for transfering data from nodes
// to faces and from faces to faces.
//
//  Author: Xiangmin Jiao
//  Revision: June 15, 2001
//===================================================================

#ifndef __TRANSFER_2F_H_
#define __TRANSFER_2F_H_

#include "rfc_basic.h"
#include "Transfer_base.h"

RFC_BEGIN_NAME_SPACE

//! Specialization for transfering from nodes to faces.
class Transfer_n2f : public Transfer_base {
  typedef Transfer_n2f         Self;
  typedef Transfer_base        Base;

public:
  //! Constructor. 
  Transfer_n2f( RFC_Window_transfer *s, RFC_Window_transfer *t) 
    : Base(s, t) {}

  /** The main entry to the data transfer algorithm.
   *  \param sf    Souce data
   *  \param tf    Target data
   *  \param alpha Parameter to control interpolation of 
   *               coordinates between the input meshes
   *  \param doa   Degree of accuracy for quadrature rule to be used
   *  \param verb  Verbose level
   */
  void transfer( const Nodal_data_const &sv, Facial_data &tf, 
		 const Real alpha,  int doa=0, bool verb=false);
};

//! Specialization for transfering from faces to faces.
class Transfer_f2f : public Transfer_base {
  typedef Transfer_f2f       Self;
  typedef Transfer_base      Base;

public:
  //! Constructor.
  Transfer_f2f( RFC_Window_transfer *s, RFC_Window_transfer *t) 
    : Base(s, t) {}

  /** The main entry to the data transfer algorithm.
   *  \see Transfer_n2f::transfer
   */
  void transfer( const Facial_data_const &sf, Facial_data &tf,
		 const Real alpha, int doa=0, bool verb=false);
};

RFC_END_NAME_SPACE

#endif // __TRANSFER_2F_H_







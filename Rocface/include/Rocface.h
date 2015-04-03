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
/* $Id: Rocface.h,v 1.18 2008/12/06 08:43:26 mtcampbe Exp $ */

/*==============================================================
//  This file contains the prototype of Rocface.
//  Author:   Xiangmin Jiao
//  Created:  May 14, 2001
//=============================================================*/

#ifndef __ROCFACE_H_
#define __ROCFACE_H_

#include "commpi.h"
#include "roccom.h"

extern "C" void Rocface_load_module(const char *);
extern "C" void Rocface_unload_module(const char *);

#ifdef __cplusplus

#include "roccom_devel.h"
#include "rfc_basic.h"
#include <map>

RFC_BEGIN_NAME_SPACE

class Overlay;
class RFC_Window_transfer;

class Rocface : public COM_Object {
public:
  typedef std::map<std::string, RFC_Window_transfer*>  TRS_Windows;

  struct Control_parameters {
    Control_parameters() : verb(0), snap(1.e-3) {}

    int    verb;
    double snap;
  };

public:
  Rocface( std::string mname);
  ~Rocface();

  static void init( const std::string &);
  static void finalize( const std::string &);

  // set verbose level 
  void set_verbose( int *verbose);

  // read Rocface control file
  void read_control_file( const char *fname);

  // Construct the overlay of two meshes.
  void overlay( const COM::Attribute *mesh1, 
		const COM::Attribute *mesh2,
		const MPI_Comm *_comm=NULL,
		const char *path=NULL);
  
  // Remove the overlay.
  void clear_overlay( const char *mesh1, 
		      const char *mesh2);

  // Write out the overlay in HDF format for read-in later.
  void write_overlay( const COM::Attribute *mesh1, 
		      const COM::Attribute *mesh2, 
		      const char *prefix1=NULL,
		      const char *prefix2=NULL,
		      const char *format=NULL);
  
  // Read in the overlay in HDF format.
  void read_overlay( const COM::Attribute *mesh1,
		     const COM::Attribute *mesh2,
		     const MPI_Comm *comm, 
		     const char *prefix1=NULL,
		     const char *prefix2=NULL,
		     const char *format=NULL);

  void set_tags( const COM::Attribute *src, 
		 const COM::Attribute *trg,
		 const COM::Attribute *tags);

  /// \param alp Alpha for geometry interpolation. Default value is 1.
  /// \param ord Order of accuracy. Default is 1 for transferring to faces 
  ///            and 2 for transferring to nodes.
  /// \param tol Tolerance for iterative linear solvers. Default value is 1.e-6
  /// \param iter Max number of iterations for iterative linear solvers. 
  ///            Default value is 100.
  void least_squares_transfer( const COM::Attribute *att1, 
			       COM::Attribute *att2,
			       const Real *alp=NULL,
			       const int *ord=NULL,
			       Real *tol=NULL, 
			       int *iter=NULL);

  void interpolate( const COM::Attribute *att1, 
		    COM::Attribute *att2);

  void load_transfer( const COM::Attribute *att1, 
		      COM::Attribute *att2, 
		      const Real *_a=NULL,    // Alpha for geometry
		      const int *_o = NULL);  // Order of integration 
protected:
  // Construct a control window.
  void create_control_window();

  static void get_name( const std::string &n1, 
			const std::string &n2, 
			std:: string &aname) {
    aname = n1+"+"+n2; return;
  }

  const RFC_Window_transfer *get_transfer_window( const COM::Attribute *);
  
  RFC_Window_transfer *get_transfer_window( COM::Attribute *);

  /** \param src   Souce data
   *  \param trg   Target data
   *  \param alpha Parameter to control interpolation of 
   *               coordinates between the input meshes
   *  \param order Order of quadrature rule to be used
   *  \param tol   Tolerance of iterative solver
   *  \param iter  Number of iterations of iterative solver.
   *  \param load  Indicates whether to perform a load transfer or not 
   */
  template <class Source_type, class Target_type, bool conserv>
  void transfer( const COM::Attribute *src, COM::Attribute *trg,
		 const Real alpha, const int order=2, 
		 Real *tol=NULL, int *iter=NULL, bool load=false);

  int validate_object() const {
    if ( _cookie != RFC_COOKIE) return -1;
    else return COM_Object::validate_object();
  }
  
protected:
  enum { RFC_COOKIE=7623223};

  std::string          _mname;
  Control_parameters   _ctrl;
  TRS_Windows          _trs_windows;

  int _cookie;
};

RFC_END_NAME_SPACE

#endif /* __cplusplus */
#endif /* __ROCFACE_H_ */







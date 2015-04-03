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
// $Id: Rocface.C,v 1.36 2009/10/08 15:35:59 mtcampbe Exp $

//==============================================================
//  This file contains the class implementation for Rocface.
//  Author:   Xiangmin Jiao
//  Created:  May 14, 2001
//==============================================================

#include "rfc_basic.h"
#include <string>
#include <cstring>
#include "Rocface.h"
#include "Overlay.h"
#include "Transfer_2f.h"
#include "Transfer_2n.h"

RFC_BEGIN_NAME_SPACE

Rocface::Rocface( std::string mname) : _mname(mname), _cookie(RFC_COOKIE) {}


Rocface::~Rocface() {
  while ( !_trs_windows.empty()) {
    TRS_Windows::iterator it = _trs_windows.begin();
    delete it->second;
    _trs_windows.erase( it);
  }
}

void Rocface::
set_verbose( int *verb) 
{ 
  RFC_assertion_msg( verb, "NULL pointer");
  _ctrl.verb = *verb; 
}

// Associate two windows given by a1->window() and a2->window().
void Rocface::
overlay( const COM::Attribute *a1,
	 const COM::Attribute *a2,
	 const MPI_Comm *comm,
	 const char *path) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  std::string n1 = a1->window()->name();
  std::string n2 = a2->window()->name();

  Overlay ovl( a1->window(), a2->window(), path);
  ovl.set_tolerance( _ctrl.snap); // set tolerance for snapping vertices

  // Perform overlay
  ovl.overlay();

  // Create new data structures for data transfer.
  std::string wn1, wn2;
  get_name( n1, n2, wn1); get_name( n2, n1, wn2);
  
  TRS_Windows::iterator it1 = _trs_windows.find( wn1);
  TRS_Windows::iterator it2 = _trs_windows.find( wn2);
  if ( it1 != _trs_windows.end()) {
    RFC_assertion( it2 != _trs_windows.end());
    delete it1->second; delete it2->second;
  }
  else {
    it1 = _trs_windows.
      insert( TRS_Windows::value_type( wn1, NULL)).first;
    RFC_assertion( it2 == _trs_windows.end());
    it2 = _trs_windows.
      insert( TRS_Windows::value_type( wn2, NULL)).first;
  }

  MPI_Comm com = (comm==NULL)?MPI_COMM_WORLD:*comm;
  it1->second = new RFC_Window_transfer(const_cast<COM::Window*>(a1->window()),
					BLUE, com);
  it2->second = new RFC_Window_transfer(const_cast<COM::Window*>(a2->window()),
					GREEN, com);

  ovl.export_windows( it1->second, it2->second);
}

// Destroy the overlay of two windows.
void Rocface::
clear_overlay( const char *m1,
	       const char *m2) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  std::string n1 = m1;
  std::string n2 = m2;

  // Create new data structures for data transfer.
  std::string wn1, wn2;
  get_name( n1, n2, wn1); get_name( n2, n1, wn2);
  
  TRS_Windows::iterator it1 = _trs_windows.find( wn1);
  if ( it1 != _trs_windows.end()) {
    delete it1->second; _trs_windows.erase( it1); 

    TRS_Windows::iterator it2 = _trs_windows.find( wn2);
    RFC_assertion( it2 != _trs_windows.end());
    delete it2->second; _trs_windows.erase( it2);
  }
  else {
    std::cerr << "Rocface: ERROR: The overlay of window \"" << n1 
	      << "\" and window \"" << n2 << "\" does not exist for deleting"
	      << std::endl;
    RFC_assertion(false); MPI_Abort(MPI_COMM_WORLD, -1);
  }
}

// Read in the two windows in binary or Rocin format.
void Rocface::
read_overlay( const COM::Attribute *a1, 
	      const COM::Attribute *a2,
	      const MPI_Comm *comm, 
	      const char *prefix1, 
	      const char *prefix2, 
	      const char *format) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  std::string n1 = a1->window()->name();
  std::string n2 = a2->window()->name();

  // Create new data structures for data transfer.
  std::string wn1, wn2;
  get_name( n1, n2, wn1); get_name( n2, n1, wn2);
  
  TRS_Windows::iterator it1 = _trs_windows.find( wn1);
  TRS_Windows::iterator it2 = _trs_windows.find( wn2);

  if ( it1 != _trs_windows.end()) {
    RFC_assertion( it2 != _trs_windows.end());
    delete it1->second; delete it2->second;
  }
  else {
    it1 = _trs_windows.
      insert( TRS_Windows::value_type( wn1, NULL)).first;
    it2 = _trs_windows.
      insert( TRS_Windows::value_type( wn2, NULL)).first;
  }

  
  MPI_Comm com = (comm==NULL)?a1->window()->get_communicator():*comm;
  it1->second = new RFC_Window_transfer(const_cast<COM::Window*>(a1->window()),
					BLUE, com, prefix1, format);
  COM_assertion(comm||com==a2->window()->get_communicator());
  it2->second = new RFC_Window_transfer(const_cast<COM::Window*>(a2->window()),
					GREEN, com, prefix2, format);

  if ( prefix1 == NULL) prefix1 = n1.c_str();
  if ( prefix2 == NULL) prefix2 = n2.c_str();

  if ( it1->second->comm_rank()==0) {
    std::cout << "RFACE: Reading in subdivision of window " << n1 
	      << " from files with prefix \"" 
	      << prefix1 << "\"...." << std::flush;
  }
  it1->second->read_sdv( prefix1, format);
  if ( it1->second->comm_rank()==0) {
    std::cout << "Done" << std::endl;
  }

  if ( it2->second->comm_rank()==0) {
    std::cout << "RFACE: Reading in subdivision of window " << n2 
	      << " from files with prefix \"" 
	      << prefix2 << "\"...." << std::flush;
  }
  it2->second->read_sdv( prefix2, format);
  if ( it2->second->comm_rank()==0) {
    std::cout << "Done" << std::endl;
  }
}

// Write out the two windows in binary or Rocout format.
// Precondition: The overlay has been computed previously.
void Rocface::
write_overlay( const COM::Attribute *a1, 
	       const COM::Attribute *a2,
	       const char *prefix1, 
	       const char *prefix2, 
	       const char *format) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  std::string n1 = a1->window()->name();
  std::string n2 = a2->window()->name();

  // Create new data structures for data transfer.
  std::string wn1, wn2;
  get_name( n1, n2, wn1); get_name( n2, n1, wn2);
  
  TRS_Windows::iterator it1 = _trs_windows.find( wn1);
  if ( it1 == _trs_windows.end()) {
    std::cerr << "RFACE: ERROR: The overlay of window \"" << n1 
	      << "\" and window \"" << n2 << "\" does not exist for output"
	      << std::endl;
    RFC_assertion( false); MPI_Abort( MPI_COMM_WORLD, -1);
  }

  TRS_Windows::iterator it2 = _trs_windows.find( wn2);
  RFC_assertion( it2 != _trs_windows.end());

  if ( prefix1 == NULL) prefix1 = n1.c_str();
  if ( prefix2 == NULL) prefix2 = n2.c_str();

  if ( it1->second->comm_rank()==0) {
    std::cout << "RFACE: Writing subdivision of window \"" 
	      << n1 << "\"...." << std::flush;
  }
  if ( format && std::strcmp( format, "Tecplot")==0) {
    it1->second->write_tec_ascii( (std::string(prefix1)+"_orig").c_str());
    it1->second->write_tec_sub( prefix1);
  }
  else 
    it1->second->write_sdv( prefix1, format);

  if ( it1->second->comm_rank()==0) {
    std::cout << "Done" << std::endl;
  }

  if ( it2->second->comm_rank()==0) {
    std::cout << "Writing subdivision of window \"" 
	      << n2 << "\"...." << std::flush;
  }

  if ( format && std::strcmp( format, "Tecplot")==0) {
    it2->second->write_tec_ascii( (std::string(prefix2)+"_orig").c_str());
    it2->second->write_tec_sub( prefix2);
  }
  else
    it2->second->write_sdv( prefix2, format);

  if ( it2->second->comm_rank()==0) {
    std::cout << "Done" << std::endl;
  }
}

const RFC_Window_transfer *
Rocface::get_transfer_window( const COM::Attribute *attr) {
  return _trs_windows.find( attr->window()->name())->second;
}
RFC_Window_transfer *
Rocface::get_transfer_window( COM::Attribute *attr) {
  return _trs_windows.find( attr->window()->name())->second;
}

void Rocface::
set_tags( const COM::Attribute *src, const COM::Attribute *trg,
	  const COM::Attribute *tags) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  RFC_assertion( tags && tags->data_type() == COM_INT || 
		 tags->data_type() == COM_INTEGER );

  std::string n1 = src->window()->name();
  std::string n2 = trg->window()->name();
  
  std::string wn2;
  get_name( n2, n1, wn2);
  
  TRS_Windows::iterator it2 = _trs_windows.find( wn2);

  if ( it2 == _trs_windows.end()) {
    std::cerr << "Rocface: ERROR: The overlay of window \"" << n1 
	      << "\" and window \"" << n2 << "\" does not exist"
	      << std::endl;
    RFC_assertion( false); MPI_Abort( MPI_COMM_WORLD, -1);
  }

  // Loop through the panes
  it2->second->set_tags( tags);
}

/****************************************************************
 * Template traits for data transfer and specializations
 ****************************************************************/

/// Defines the interface. Implementations only present in specializations.
template <class Source_type, class Target_type, bool conserv>
class Transfer_traits {
 public:
  typedef void *Transfer_type;

  /** \param trans Object that performs data transfer
   *  \param sf    Souce data
   *  \param tf    Target data
   *  \param alpha Parameter to control interpolation of 
   *               coordinates between the input meshes
   *  \param order Order of quadrature rule to be used
   *  \param tol   Tolerance of iterative solver
   *  \param iter  Number of iterations of iterative solver.
   *  \param verbose verbose level
   *  \param load  Indicates whether to perform a load transfer or not 
   */
  static void transfer( Transfer_type &trans, 
			Source_type &sf, Target_type &tf, 
			const Real alpha, const int order, 
			Real *tol, int *iter, 
			const int verbose, const bool load);
};

// Wrapper for nonconservative interpolation.
template <>
class Transfer_traits<Nodal_data_const, Nodal_data, false> {
 public:
  typedef Interpolator   Transfer_type;

  static void transfer( Transfer_type &trans, 
			Nodal_data_const &sf, Nodal_data &tf, 
			const Real alpha, const int order, 
			Real *tol, int *iter, 
			const int verbose, const bool) {
    trans.transfer( sf, tf, verbose);
  }
};

// Wrapper for conservative nodes-to-faces transfer.
template <>
class Transfer_traits<Nodal_data_const, Facial_data, true> {
 public:
  typedef Transfer_n2f      Transfer_type;

  static void transfer( Transfer_type &trans, 
			Nodal_data_const &sf, Facial_data &tf, 
			const Real alpha, const int order, 
			Real *tol, int *iter, 
			const int verbose, const bool) {
    trans.transfer( sf, tf, alpha, order, verbose);
  }
};

// Wrapper for conservative faces-to-faces transfer.
template <>
class Transfer_traits<Facial_data_const, Facial_data, true> {
 public:
  typedef Transfer_f2f      Transfer_type;

  static void transfer( Transfer_type &trans, 
			Facial_data_const &sf, Facial_data &tf, 
			const Real alpha, const int order, 
			Real *tol, int *iter, 
			const int verbose, const bool) {
    trans.transfer( sf, tf, alpha, order, verbose);
  }
};

// Wrapper for conservative faces-to-nodes transfer.
template <>
class Transfer_traits<Facial_data_const, Nodal_data, true> { 
 public:
 typedef Transfer_f2n      Transfer_type;

  /// See above
  static void transfer( Transfer_type &trans, 
			Facial_data_const &sf, Nodal_data &tf, 
			const Real alpha, const int order, 
			Real *tol, int *iter, 
			const int verbose, const bool load) {
    if ( !load)
      trans.transfer( sf, tf, alpha, tol, iter, order, verbose);
    else
      trans.comp_loads( sf, tf, alpha, order, verbose);
  }
};

// Wrapper for conservative nodes-to-nodes transfer.
template <>
class Transfer_traits<Nodal_data_const, Nodal_data, true> {
public:
  typedef Transfer_n2n      Transfer_type;

  static void transfer( Transfer_type &trans, 
			Nodal_data_const &sf, Nodal_data &tf, 
			const Real alpha, const int order, 
			Real *tol, int *iter, 
			const int verbose, const bool load) {
    if ( !load)
      trans.transfer( sf, tf, alpha, tol, iter, order, verbose);
    else
      trans.comp_loads( sf, tf, alpha, order, verbose);
  }
};

// Template implementation for transfering data between meshes.
template <class Source_type, class Target_type, bool conserv>
void Rocface::transfer( const COM::Attribute *src, COM::Attribute *trg,
			const Real alpha, const int order, 
			Real *tol, int *iter, bool load) {
  typedef Transfer_traits<Source_type, Target_type, conserv>  Traits;

  std::string n1 = src->window()->name();
  std::string n2 = trg->window()->name();

  std::string wn1, wn2;
  get_name( n1, n2, wn1); get_name( n2, n1, wn2);
  
  TRS_Windows::iterator it1 = _trs_windows.find( wn1);
  TRS_Windows::iterator it2 = _trs_windows.find( wn2);

  if ( it1 == _trs_windows.end() || it2 == _trs_windows.end()) {
    std::cerr << "RFACE: ERROR: The overlay of window \"" << n1 
	      << "\" and window \"" << n2 << "\" does not exist"
	      << std::endl;
    RFC_assertion( false); MPI_Abort( MPI_COMM_WORLD, -1);
  }

  if ( !it1->second->replicated()) {
    it1->second->replicate_metadata( *it2->second);
  }

  Target_type tf( trg);
  Source_type sf( src);

  RFC_Window_transfer *w1=it1->second, *w2=it2->second;
  typename Traits::Transfer_type trans( w1, w2);

  // Print min, max, and integral before transfer
  if ( _ctrl.verb) {
    if ( w2->comm_rank()==0) {
      if (conserv) 
	std::cout << "RFACE: Conservatively transferring "; 
      else
	std::cout << "RFACE: Interpolating "; 
      std::cout << " from " << w1->name()+"."+src->name() 
		<< " to " << w2->name()+"."+trg->name() << std::endl; 
    }
    Vector_n min_v( sf.dimension()), max_v(sf.dimension());
    trans.minmax( *w1, sf, min_v, max_v);
    
    Vector_n integral(sf.dimension(),0);
    trans.integrate( *w1, sf, integral, order);
    
    if ( w1->comm_rank()==0) {
      std::cout << "RFACE: Before transfer\nRFACE:\tminimum:  " << min_v
		<< "\nRFACE:\tmaximum:  " << max_v;
      if ( !load) 
	std::cout << "\nRFACE:\tintegral: " << std::setprecision(10)
		  << integral;
      std::cout << std::endl;
    }
  }

  // Perform data transfer
  Traits::transfer( trans, sf, tf, alpha, order, tol, iter, _ctrl.verb, load);

  // Print min, max, and integral after transfer
  if ( _ctrl.verb) {
    Vector_n min_v( sf.dimension()), max_v(sf.dimension());	
    trans.minmax( *w2, tf, min_v, max_v);
    
    Vector_n integral(sf.dimension(),0);			
    trans.integrate( *w2, tf, integral, order);
    
    if ( w2->comm_rank()==0) {
      std::cout << "RFACE: After transfer\nRFACE:\tminimum:  " << min_v
		<< "\nRFACE:\tmaximum:  " << max_v;
      if ( !load) 
	std::cout << "\nRFACE:\tintegral: " << std::setprecision(10)
		  << integral;
      std::cout << std::endl;
    }
  }

  // Reset the tags, which indicate which nodes/elements should receive values
  w2->set_tags( NULL);
}

// Transfer data from a window to another using the least squares
// data transfer formulation.
void Rocface::
least_squares_transfer( const COM::Attribute *src, 
			COM::Attribute *trg,
			const Real *alp_in,
			const int *ord_in,
			Real *tol_io,
			int  *iter_io)
{
  COM_assertion_msg( validate_object()==0, "Invalid object");

  Real   alpha = (alp_in == NULL) ? 1. : *alp_in;
  int    order = (ord_in == NULL) ? 1+trg->is_nodal() : *ord_in;

  COM_assertion( alpha>=0 && alpha<=1);

  if ( trg->is_nodal()) {
    Real   tol = (tol_io == NULL) ? 1.e-6 : *tol_io;
    int    iter = (iter_io == NULL) ? 100 : *iter_io;

    if ( src->is_nodal()) {
      transfer<Nodal_data_const, Nodal_data, true>
	( src, trg, alpha, order, &tol, &iter);
    }
    else {
      transfer<Facial_data_const, Nodal_data, true>
	( src, trg, alpha, order, &tol, &iter);
    }
    
    if (tol_io != NULL) *tol_io = tol;
    if (iter_io != NULL) *iter_io = iter;
  }
  else {
    if ( src->is_nodal()) {
      transfer<Nodal_data_const, Facial_data, true>
	( src, trg, alpha, order);
    }
    else {
      transfer< Facial_data_const, Facial_data, true>
	( src, trg, alpha, order);
    }
  }
}

// Transfer data from a window to another using the traditional interpolation.
void Rocface::
interpolate( const COM::Attribute *src, 
	     COM::Attribute *trg) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  RFC_assertion( trg->is_nodal() && src->is_nodal());

  transfer< Nodal_data_const, Nodal_data, false>
    ( src, trg, 1.);
}

// Transfer data from a window to another using load transfer.
void Rocface::
load_transfer( const COM::Attribute *src, 
	       COM::Attribute *trg,
	       const Real *_a,    // Alpha for geometry interpolation
	       const int *_o)     // Order of integration 
{
  RFC_assertion( trg->is_nodal() && src->is_nodal());

  const Real alpha = (_a == NULL)?-1.:*_a; // Alpha for geometry interpolation
  const int  order = (_o == NULL)?1:*_o;   // Order of integration

  if ( src->is_nodal()) {
    transfer<Nodal_data_const, Nodal_data, true>
      ( src, trg, alpha, order, NULL, NULL, true);
  }
  else {
    transfer<Facial_data_const, Nodal_data, true>
      ( src, trg, alpha, order, NULL, NULL, true);
  }
}

// Register the public functions of the component to Roccom.
void Rocface::init(const std::string &mname) {

  Rocface *rfc = new Rocface( mname);

  COM_Type types[8];
  std::string glb=mname+".global";

  COM_new_window( mname.c_str());
  COM_new_attribute( glb.c_str(), 'w', COM_VOID, 1, "");
  COM_set_object( glb.c_str(), 0, rfc);
  
  types[0] = COM_RAWDATA;
  types[1] = types[2] = COM_METADATA; 
  types[3] = COM_MPI_COMM;
  types[4] = COM_STRING;
    
  COM_set_member_function( (mname+".overlay").c_str(), 
			   (Member_func_ptr)(&Rocface::overlay), 
			   glb.c_str(), "biiII", types);

  types[4] = types[5] = types[6] = COM_STRING;
  COM_set_member_function( (mname+".read_overlay").c_str(), 
			   (Member_func_ptr)(&Rocface::read_overlay), 
			   glb.c_str(), "biiiIII", types);

  types[3] = types[1] = types[2] = COM_METADATA; 
  COM_set_member_function( (mname+".set_tags").c_str(), 
			   (Member_func_ptr)(&Rocface::set_tags), 
			   glb.c_str(), "biii", types);
  
  types[3] = types[5] = COM_DOUBLE;
  types[4] = types[6] = COM_INT; 
  COM_set_member_function( (mname+".least_squares_transfer").c_str(), 
			   (Member_func_ptr)(&Rocface::least_squares_transfer),
			   glb.c_str(), "bioIIBB", types);

  COM_set_member_function( (mname+".interpolate").c_str(), 
			   (Member_func_ptr)(&Rocface::interpolate), 
			   glb.c_str(), "bio", types);

  types[4] = COM_INT;
  COM_set_member_function( (mname+".load_transfer").c_str(), 
			   (Member_func_ptr)(&Rocface::load_transfer), 
			   glb.c_str(), "bioII", types);


  types[3] = types[4] = types[5] = COM_STRING; 
  COM_set_member_function( (mname+".write_overlay").c_str(),
			   (Member_func_ptr)(&Rocface::write_overlay), 
			   glb.c_str(), "biiIII", types);

  types[1] = types[2] = COM_STRING; 
  COM_set_member_function( (mname+".clear_overlay").c_str(), 
			   (Member_func_ptr)(&Rocface::clear_overlay), 
			   glb.c_str(), "bii", types);

  types[1] = COM_STRING;
  COM_set_member_function( (mname+".read_control_file").c_str(), 
			   (Member_func_ptr)(&Rocface::read_control_file), 
			   glb.c_str(), "bi", types);


  types[1] = COM_INT;
  COM_set_member_function( (mname+".set_verbose").c_str(), 
 			   (Member_func_ptr)(&Rocface::set_verbose), 
  			   glb.c_str(), "bi", types);

  COM_window_init_done( mname.c_str());
}

extern "C" void Rocin_load_module(const char *);
extern "C" void Rocin_unload_module(const char *);

// Read Rocface control file.
void Rocface::
read_control_file( const char *fname) {
  std::string ctrlname = _mname+"__CTRL";

  // Create a default window
  COM_new_window(ctrlname.c_str());

  // Set verbosity level
  COM_new_attribute( (ctrlname+".verbosity").c_str(), 'w', COM_INT, 1, "");
  COM_set_array( (ctrlname+".verbosity").c_str(), 0, &_ctrl.verb);

  // Set snap threshold
  COM_new_attribute( (ctrlname+".snap_tolerance").c_str(), 'w', COM_DOUBLE, 1, "");
  COM_set_array( (ctrlname+".snap_tolerance").c_str(), 0, &_ctrl.snap);

  // Done initialization.
  COM_window_init_done( ctrlname.c_str());

  // Read in using Rocin.
  Rocin_load_module( "RFC_CNTRL_IN");
  int hdl_read = COM_get_function_handle( "RFC_CNTRL_IN.read_parameter_file");
  COM_call_function( hdl_read, fname, ctrlname.c_str());
  Rocin_unload_module( "RFC_CNTRL_IN");

  COM_delete_window( ctrlname.c_str());
}

// Deregister the component from Roccom.
void Rocface::finalize( const std::string &mname) {

  Rocface *rfc;

  std::string glb=mname+".global";
  COM_get_object( glb.c_str(), 0, &rfc);

  delete rfc;
  COM_delete_window( mname.c_str());
}

RFC_END_NAME_SPACE

USE_RFC_NAME_SPACE

// C/C++ binding
extern "C" void Rocface_load_module( const char *name)
{ Rocface::init( std::string(name)); }
extern "C" void Rocface_unload_module( const char *name) 
{ Rocface::finalize( std::string(name)); }

// Fortran binding
extern "C" void rocface_load_module( const char *name, long int length)
{ Rocface::init( std::string(name, length)); }
extern "C" void rocface_unload_module( const char *name, long int length) 
{ Rocface::finalize( std::string(name, length)); }

extern "C" void ROCFACE_LOAD_MODULE( const char *name, long int length)
{ Rocface::init( std::string(name, length)); }
extern "C" void ROCFACE_UNLOAD_MODULE( const char *name, long int length) 
{ Rocface::finalize( std::string(name, length)); }

extern "C" void rocface_load_module_( const char *name, long int length)
{ Rocface::init( std::string(name, length)); }
extern "C" void rocface_unload_module_( const char *name, long int length) 
{ Rocface::finalize( std::string(name, length)); }

extern "C" void ROCFACE_LOAD_MODULE_( const char *name, long int length)
{ Rocface::init( std::string(name, length)); }
extern "C" void ROCFACE_UNLOAD_MODULE_( const char *name, long int length) 
{ Rocface::finalize( std::string(name, length)); }







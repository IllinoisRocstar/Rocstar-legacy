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
// $Id: Rocsurf.C,v 1.12 2008/12/06 08:43:23 mtcampbe Exp $

#include "Rocsurf.h"
#include "roccom.h"
#include "Manifold_2.h"

SURF_BEGIN_NAMESPACE

const int Rocsurf::scheme_vals[] = {E2N_USER, E2N_ONE, E2N_AREA, E2N_ANGLE};

Rocsurf::~Rocsurf() { if (_wm) delete _wm; }

void Rocsurf::initialize( const COM::Attribute *mesh) {
  COM_assertion_msg( validate_object()==0, "Invalid object");
  COM_assertion_msg( !mesh || mesh->id()==COM::COM_MESH || 
		     mesh->id()==COM::COM_PMESH,
		     "Input argument must be a mesh or pmesh");

  if ( _wm) delete _wm;

  _wm = new Window_manifold_2( const_cast<COM::Attribute*>(mesh));

  _wm->init_communicator();
}

// Evaluate nodal normals
void Rocsurf::compute_normals( const COM::Attribute *mesh,
			       COM::Attribute *nrm,
			       const int *scheme) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  if ( _wm == NULL) initialize( mesh);

  if ( scheme) 
    _wm->compute_normals( nrm, *scheme);
  else
    _wm->compute_normals( nrm);
}

// Evaluate nodal normals
void Rocsurf::compute_mcn( COM::Attribute *mcn, 
			   COM::Attribute *lbmcn) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  COM_assertion_msg( _wm, "initialization must be called first before calling compute_mcn");
  
  _wm->compute_mcn( mcn, lbmcn);
}

void Rocsurf::elements_to_nodes( const COM::Attribute *elem_vals,
				COM::Attribute *nodal_vals,
				const COM::Attribute *mesh,
				const int *scheme,
				const COM::Attribute *elem_weights,
				COM::Attribute *nodal_weights) 
{
  COM_assertion_msg( validate_object()==0, "Invalid object");

  if ( _wm == NULL) initialize( mesh);

  _wm->elements_to_nodes( elem_vals, nodal_vals, scheme?*scheme:E2N_AREA,
			  elem_weights, nodal_weights);
}

void Rocsurf::compute_edge_lengths( double *lave, double *lmin, double *lmax) {

  Window_manifold_2::PM_iterator it=_wm->pm_begin(), iend=_wm->pm_end();

  double local_lmin=HUGE_VAL, local_lmax=0, local_lsum=0, local_weights=0;

  // Loop through the panes to identify strong edges
  for ( ; it!=iend; ++it) {
    for (int i=0, nf=it->size_of_real_faces(); i<nf; ++i) {
      Halfedge h=Halfedge( &*it, Edge_ID( i+1, 0), ACROSS_PANE), h0=h;
      do {
	Halfedge hopp=h.opposite();

	if ( hopp.is_border() || h < hopp) {
	  double  l = h.tangent().norm();

	  local_lmin = std::min( local_lmin, l);
	  local_lmax = std::max( local_lmax, l);
	  local_lsum += l;
	  local_weights += 1;
	}
      } while ( (h=h.next())!=h0);
    }
  }

  double global_lmin=local_lmin, global_lmax=local_lmax, 
    global_lsum=local_lsum, global_weighs=local_weights;
  
  if ( COMMPI_Initialized()) {
    MPI_Comm comm = _wm->window()->get_communicator();
    MPI_Allreduce( &local_lmin, &global_lmin, 1, MPI_DOUBLE, MPI_MIN, comm);
    MPI_Allreduce( &local_lmax, &global_lmax, 1, MPI_DOUBLE, MPI_MAX, comm);
    MPI_Allreduce( &local_lsum, &global_lsum, 1, MPI_DOUBLE, MPI_SUM, comm);
    MPI_Allreduce( &local_weights, &global_weighs, 1, MPI_DOUBLE, MPI_SUM, comm);
  }

  if ( lave) *lave = (global_weighs>0)? (global_lsum / global_weighs) : 0.;
  if ( lmin) *lmin = global_lmin;
  if ( lmax) *lmax = global_lmax;
}

void Rocsurf::serialize_mesh( const COM::Attribute *inmesh, COM::Attribute *outmesh) {
  COM_assertion_msg( validate_object()==0, "Invalid object");

  if ( _wm == NULL) initialize( inmesh);

  COM::Window *outwin = outmesh->window();

  // Serialize input mesh and put into output mesh
  _wm->serialize_window( outwin);
}

void Rocsurf::load( const std::string &mname) {
  Rocsurf *surf = new Rocsurf();

  COM_new_window( mname.c_str());

  std::string glb=mname+".global";

  COM_new_attribute( glb.c_str(), 'w', COM_VOID, 1, "");
  COM_set_object( glb.c_str(), 0, surf);

  COM_Type types[7];

  types[0] = COM_METADATA; types[1] = COM_METADATA; 
  types[2] = COM_METADATA; types[3] = COM_VOID;
  COM_set_function( (mname+".interpolate_to_centers").c_str(),
		    (Func_ptr)interpolate_to_centers, "io", types);

  COM_set_function( (mname+".compute_element_areas").c_str(), 
		    (Func_ptr)compute_element_areas, "oI", types);
  
  COM_set_function( (mname+".compute_bounded_volumes").c_str(), 
		    (Func_ptr)compute_bounded_volumes, "iioI", types);
  
  COM_set_function( (mname+".compute_swept_volumes").c_str(), 
		    (Func_ptr)compute_swept_volumes, "iioI", types);
  
  types[0] = COM_METADATA; types[1] = COM_DOUBLE; 
  COM_set_function( (mname+".integrate").c_str(), 
		    (Func_ptr)integrate, "io", types);
  
  COM_set_function( (mname+".compute_signed_volumes").c_str(), 
		    (Func_ptr)compute_signed_volumes, "io", types);

  types[1] = COM_INTEGER; 
  COM_set_function( (mname+".compute_element_normals").c_str(), 
		    (Func_ptr)compute_element_normals, "oII", types);


  types[0] = COM_RAWDATA; types[1] = types[2] = COM_METADATA;
  COM_set_member_function( (mname+".initialize").c_str(), 
			   (Member_func_ptr)(&Rocsurf::initialize), 
			   glb.c_str(), "bi", types);

  types[3] = COM_INT; 
  COM_set_member_function( (mname+".compute_normals").c_str(), 
			   (Member_func_ptr)(&Rocsurf::compute_normals), 
			   glb.c_str(), "bioI", types);
  

  COM_set_member_function( (mname+".compute_mcn").c_str(), 
			   (Member_func_ptr)(&Rocsurf::compute_mcn), 
			   glb.c_str(), "boo", types);

  types[3] = types[5] = types[6] = COM_METADATA; types[4] = COM_INT; 
  COM_set_member_function( (mname+".elements_to_nodes").c_str(),
			   (Member_func_ptr)(&Rocsurf::elements_to_nodes),
			   glb.c_str(), "bioiIIO", types);

  COM_new_attribute((mname+".E2N_USER").c_str(), 'w', COM_INT, 1, "");
  COM_set_array_const((mname+".E2N_USER").c_str(), 0, &scheme_vals[E2N_USER]);
  COM_new_attribute((mname+".E2N_ONE").c_str(), 'w', COM_INT, 1, "");
  COM_set_array_const((mname+".E2N_ONE").c_str(), 0, &scheme_vals[E2N_ONE]);
  COM_new_attribute((mname+".E2N_AREA").c_str(), 'w', COM_INT, 1, "");
  COM_set_array_const((mname+".E2N_AREA").c_str(), 0, &scheme_vals[E2N_AREA]);
  COM_new_attribute((mname+".E2N_ANGLE").c_str(), 'w', COM_INT, 1, "");
  COM_set_array_const((mname+".E2N_ANGLE").c_str(), 0, &scheme_vals[E2N_ANGLE]);

  types[1] = types[2] = types[3] = COM_DOUBLE;
  COM_set_member_function( (mname+".compute_edge_lengths").c_str(),
			   (Member_func_ptr)(&Rocsurf::compute_edge_lengths),
			   glb.c_str(), "boOO", types);

  types[1] = types[2] = COM_METADATA;
  COM_set_member_function( (mname+".serialize_mesh").c_str(),
			   (Member_func_ptr)(&Rocsurf::serialize_mesh),
			   glb.c_str(), "bio", types);

  COM_window_init_done( mname.c_str());
}

void Rocsurf::unload( const std::string &mname) {
  Rocsurf *surf;
  std::string glb=mname+".global";

  COM_get_object( glb.c_str(), 0, &surf);
  delete surf;

  COM_delete_window( mname.c_str());
}

extern "C" void Rocsurf_load_module( const char *mname) 
{ Rocsurf::load( mname); }

extern "C" void Rocsurf_unload_module( const char *mname) 
{ Rocsurf::unload( mname); }

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// Fortran bindings
extern "C" void rocsurf_load_module( const char *mname, long int length) 
{ Rocsurf::load( std::string(mname, length)); }

extern "C" void rocsurf_unload_module( const char *mname, long int length) 
{ Rocsurf::unload( std::string(mname, length)); }

extern "C" void ROCSURF_LOAD_MODULE( const char *mname, long int length) 
{ Rocsurf::load( std::string(mname, length)); }

extern "C" void ROCSURF_UNLOAD_MODULE( const char *mname, long int length) 
{ Rocsurf::unload( std::string(mname, length)); }

extern "C" void rocsurf_load_module_( const char *mname, long int length) 
{ Rocsurf::load( std::string(mname, length)); }

extern "C" void rocsurf_unload_module_( const char *mname, long int length) 
{ Rocsurf::unload( std::string(mname, length)); }

extern "C" void ROCSURF_LOAD_MODULE_( const char *mname, long int length) 
{ Rocsurf::load( std::string(mname, length)); }

extern "C" void ROCSURF_UNLOAD_MODULE_( const char *mname, long int length) 
{ Rocsurf::unload( std::string(mname, length)); }
#endif // DOXYGEN_SHOULD_SKIP_THIS

SURF_END_NAMESPACE







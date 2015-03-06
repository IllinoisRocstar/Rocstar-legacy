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
// $Id: Window.C,v 1.25 2008/12/06 08:43:25 mtcampbe Exp $

/** \file Window.C
 *  This file contains the implementation of the Window object.
 *  @see roccom_devel.h, Roccom_base.C
 */
/* Author: Xiangmin Jiao
 * Created:  Jan. 10, 2001
 * Last modified: May 9, 2001
 */

#include <iostream>
#include <cstdlib>
#include <cstdio>
#include "Window.h"
#include "roccom_assertion.h"

COM_BEGIN_NAME_SPACE

Window::Window( const std::string &s, MPI_Comm c) 
  : _dummy( this, 0), _name( s), _last_id(COM_NUM_KEYWORDS), 
    _comm(c), _status( STATUS_NOCHANGE)
{
  // Insert keywords into _attr_map
  for ( int i=0; i<COM_NUM_KEYWORDS; ++i) {
    Attribute *a = _dummy.attribute(i);
    if ( !Attribute::is_digit(a->name()[0])) _attr_map[ a->name()] = a;
  }
}

Window::~Window( ) 
{
  for ( Pane_map::iterator it=_pane_map.begin(); it!=_pane_map.end(); ++it)
    delete it->second;
}

void Window::set_function( const std::string &fname, 
			   Func_ptr func,
			   const std::string &intents, 
			   const COM_Type *types, 
			   Attribute *a, 
			   bool is_f90) throw(COM_exception) {
  if ( _func_map.find( fname) != _func_map.end()) 
    throw COM_exception(COM_WARN_DUP_FUNC, append_frame
			(_name+"."+fname,Window::init_function));
  
  _func_map[fname] = Function( func, intents, types, a, is_f90);
}

void Window::set_function( const std::string &fname, 
			   Member_func_ptr func,
			   const std::string &intents, 
			   const COM_Type *types, 
			   Attribute *a, 
			   bool) throw(COM_exception) {
  if ( _func_map.find( fname) != _func_map.end()) 
    throw COM_exception(COM_WARN_DUP_FUNC, append_frame
			(_name+"."+fname,Window::init_function));
  
  _func_map[fname] = Function( func, intents, types, a);
}

Attribute *Window::
new_attribute( const std::string &aname, const char loc, const int type, 
	       int ncomp, const std::string &unit) throw(COM_exception) 
{
  // Special handing for connectivity
  if ( Connectivity::is_element_name( aname) || 
       Attribute::is_digit(aname[0])) 
    throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_NAME,append_frame
			 (_name+"."+aname,Window::new_attribute));
  if ( ncomp<=0)
    throw COM_exception( COM_ERR_INVALID_DIMENSION,append_frame
			 (_name+"."+aname,Window::new_attribute));

  if ( loc != 'w' && loc != 'p' && loc != 'e' && loc != 'n' && loc != 'c')
    throw COM_exception( COM_ERR_UNKNOWN_KEYWORD,append_frame
			 (_name+"."+aname,Window::new_attribute));

  if ( type<COM_MIN_TYPEID || type > COM_MAX_TYPEID)
    throw COM_exception( COM_ERR_UNKNOWN_DATATYPE,append_frame
			 (_name+"."+aname,Window::new_attribute));

  // Redefine an attribute if it pre-exists, but ncomp cannot be increased.
  Attr_map::const_iterator it = _attr_map.find( aname);

  if ( it != _attr_map.end() && ncomp > it->second->size_of_components()) {
    COM_assertion_msg( it->second->id()>=COM_NUM_KEYWORDS,
		       "Cannot increase size of keywords");
    delete_attribute( it->first);
    it = _attr_map.end();
  }

  int id = (it==_attr_map.end())?_last_id:it->second->id();

  // Insert the object into both the set and the map.
  Attribute *a = ((Pane_friend&)_dummy).
    new_attribute( aname, id, loc, type, ncomp, unit);
  _attr_map[aname] = a;
  
  // Propagete onto all existing panes.
  for (Pane_map::iterator it=_pane_map.begin(); it!=_pane_map.end(); ++it) {
    Pane_friend *pn = (Pane_friend*)it->second;
    pn->new_attribute( aname, id, loc, type, ncomp, unit);
  }

  // Update last available id
  if ( it == _attr_map.end()) _last_id = id+ncomp+(ncomp>1);
  
  if ( _status == STATUS_NOCHANGE) _status = STATUS_CHANGED;
  return a;
}

void Window::
delete_attribute( const std::string &aname) throw(COM_exception)
{
  Attr_map::iterator it = _attr_map.find( aname);
  if ( it == _attr_map.end())
    throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST,append_frame
			 (_name+"."+aname,Window::delete_attribute));

  Attribute *attr = it->second;
  int id = attr->id(), ncomp=attr->size_of_components();

  if ( id < COM_NUM_KEYWORDS && id != COM_ATTS)
    throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_NAME,append_frame
			 (_name+"."+aname,Window::delete_attribute));

  // Remove the object from both the set
  ((Pane_friend&)_dummy).delete_attribute( id);

  // Remove from all panes.
  for (Pane_map::iterator it=_pane_map.begin(); it!=_pane_map.end(); ++it) {
    Pane_friend *pn = (Pane_friend*)it->second;
    pn->delete_attribute( id);
  }

  // Update last id
  if (id != COM_ATTS) {
    _attr_map.erase( it); // Remove from _attr_map
    if ( _last_id == id + ncomp+(ncomp>1)) {
      _last_id = id;
      if ( _status==STATUS_NOCHANGE) _status = STATUS_CHANGED; 
    }
    else
      _status = STATUS_SHRUNK;
  }
  else if (_last_id>COM_NUM_KEYWORDS) {
    _attr_map.clear();
    for ( int i=0; i<COM_NUM_KEYWORDS; ++i) {
      Attribute *a = _dummy.attribute(i);
      if ( !Attribute::is_digit(a->name()[0])) _attr_map[ a->name()] = a;
    }

    _status = STATUS_CHANGED;
    _last_id = COM_NUM_KEYWORDS;
  }
}

void Window::set_size( const std::string &aname, int pid,
		       int nitems, int ng) throw( COM_exception) 
{ 
  if ( Connectivity::is_element_name( aname)) {
    Pane_friend &pn = (Pane_friend&)pane(pid,true);
    Connectivity *con = pn.connectivity( aname,true);
    pn.set_size( con, nitems, ng);
  }
  else {
    Attribute *a = attribute(aname);

    if ( a == NULL)
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST,append_frame
			   (_name+"."+aname,Window::set_size));
  
    if ( a->is_windowed()) 
      ((Pane_friend&)_dummy).set_size( a, nitems, ng);
    else if ( pid==0) {
      // Loop through the panes to set the sizes.
      for (Pane_map::iterator it=_pane_map.begin(), iend=_pane_map.end();
	   it != iend; ++it) {
	((Pane_friend*)it->second)->set_size
	  ( it->second->attribute( a->id()), nitems, ng);
      }
    }
    else 
      ((Pane_friend&)pane(pid,true)).set_size( a, nitems, ng); 
  }
}

void Window::set_array(const std::string &aname, const int pane_id,
		       void *addr, int strd, int cap, bool is_const) 
  throw(COM_exception)
{
  if ( Connectivity::is_element_name( aname)) {
    Pane &pn = pane(pane_id,true);
    Connectivity *con = ((Pane_friend&)pn).connectivity( aname,true);
    
    if ( con==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::set_array));

    reinit_conn( con, is_const?Pane::OP_SET_CONST:Pane::OP_SET, 
		 &(int*&)addr, strd, cap);
  }
  else {
    Attribute *a = pane( pane_id,true).attribute( aname);
    if ( a==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::set_array));

    reinit_attr( a, is_const?Pane::OP_SET_CONST:Pane::OP_SET, 
		 &addr, strd, cap);
  }
}

void Window::alloc_array(const std::string &aname, const int pane_id,
			 void **addr, int strd, int cap) throw(COM_exception)
{
  if ( Connectivity::is_element_name( aname)) {
    Pane &pn = pane(pane_id,true);
    Connectivity *con = ((Pane_friend&)pn).connectivity( aname);
    
    if ( con==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::alloc_array));

    reinit_conn( con, Pane::OP_ALLOC, (int**)addr, strd, cap);
  }
  else {
    Attribute *a = pane( pane_id,true).attribute( aname);

    if ( a==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::alloc_array));
    reinit_attr( a, Pane::OP_ALLOC, addr, strd, cap);
  }
}

void Window::resize_array(const std::string &aname, const int pane_id,
			  void **addr, int strd, int cap) throw(COM_exception)
{
  if ( Connectivity::is_element_name( aname)) {
    Pane &pn = pane(pane_id,true);
    Connectivity *con = ((Pane_friend&)pn).connectivity( aname);
    
    if ( con==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::resize_array));

    reinit_conn( con, Pane::OP_RESIZE, (int**)addr, strd, cap);
  }
  else {
    Attribute *a = pane( pane_id,true).attribute( aname);

    if ( a==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::resize_array));
    reinit_attr( a, Pane::OP_RESIZE, addr, strd, cap);
  }
}

void Window::append_array( const std::string &aname, const int pane_id,
			   const void *val, int v_strd, int v_size) throw(COM_exception)
{
  COM_assertion_msg( !Connectivity::is_element_name( aname),
		     "append_array supports only window and pane attributes");
  
  Attribute *a = pane( pane_id,true).attribute( aname);

  if ( a==NULL) 
    throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			 (name()+"."+aname,Window::append_array));
  try {
    // Set size to 0 if not yet set.
    if ( !a->size_set()) a->set_size(0); 
    // resize the target attribute.
    reinit_attr( a, Pane::OP_RESIZE, NULL, a->stride(), 
		 a->size_of_items()+v_size);
  }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Window::append_array);
    throw ex;
  }
  
  // copy the array to the back of the target array
  a->append_array( val, v_strd, v_size);
}

void Window::dealloc_array( const std::string &aname, 
			    const int pane_id) throw(COM_exception)
{
  if ( Connectivity::is_element_name( aname)) {
    Pane &pn = pane(pane_id);
    Connectivity *con = ((Pane_friend&)pn).connectivity( aname);
    
    if ( con==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::dealloc_array));

    reinit_conn( con, Pane::OP_DEALLOC);
  }
  else {
    Attribute *a = pane( pane_id).attribute( aname);

    if ( a==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname,Window::dealloc_array));
    reinit_attr( a, Pane::OP_DEALLOC);
  }
}

Attribute *Window::inherit( Attribute *from, const std::string &aname,
			    int mode, bool withghost,
			    const Attribute *cond, int val) 
  throw(COM_exception) 
{
  Attribute *a = ((Pane_friend&)_dummy).inherit(from, aname, mode, withghost);
  if ( from->is_windowed()) return a;

  // Copy the panes
  Pane_map &ps = from->window()->_pane_map;

  bool with_conn = from->id()==COM_CONN || from->id()==COM_MESH || 
    from->id()==COM_PMESH || from->id()==COM_ALL;

  bool insert_pane = (mode != Pane::INHERIT_COPY) && with_conn;

  if ( with_conn && aname.size() && from->name() != aname)
    throw COM_exception( COM_ERR_INCOMPATIBLE_ATTRS, append_frame
			 (from->fullname()+" and " +aname,
			  Window::inherit));

  if ( cond && cond->window()!=from->window()) {
    throw COM_exception( COM_ERR_INCOMPATIBLE_ATTRS, append_frame
			 (from->fullname()+" and "+cond->fullname(),
			  Window::inherit));
  }

  for ( Pane_map::iterator ppit = ps.begin(); ppit!=ps.end(); ++ppit) {
    Pane *ppn=ppit->second;
    const int *cnd = NULL;
    if ( cond) {
      const Attribute *cond_pn = ppn->attribute( cond->id());
      cnd = (const int*)cond_pn->pointer();
    }

    // Inherit the pane if the condition was set to an variable (cnd!=NULL),
    // or set to to pane ID (cnd==NULL && val!=0), or not set.
    if ( !cond && (val==0 || val==ppn->id()) || cnd && *cnd == val) {
      // Create new panes only when we are inheriting the mesh.
      Pane_friend *pn;
      if ( insert_pane)
	pn = (Pane_friend*)( &pane( ppit->first, true));
      else {
	Pane_map::iterator it = _pane_map.find( ppit->first);
	pn = ( it!=_pane_map.end())?(Pane_friend*)(it->second):NULL;
      }
 
      if ( pn)
	pn->inherit( ppn->attribute( from->id()), aname, mode, withghost);
    }
  }
  return a;
}

void Window::init_done( bool pane_changed) throw(COM_exception) {
  // Loop through the attributes.
  if ( _status == STATUS_SHRUNK) {
    int max_id=0;
    for (Attr_map::iterator it=_attr_map.begin(); it!=_attr_map.end(); ++it) {
      int id = it->second->id(), ncomp=it->second->size_of_components();
      max_id = std::max( max_id, id+ncomp+(ncomp>1));
    }
    if (max_id < _last_id) _last_id = max_id;
  }

  int npanes = _pane_map.size();
  std::vector< int> pane_ids; pane_ids.reserve(npanes);

  for (Pane_map::iterator it=_pane_map.begin(); it!=_pane_map.end(); ++it) {
    it->second->init_done();
    pane_ids.push_back( it->second->id());
  }
  
  _status = STATUS_NOCHANGE;

  if ( !pane_changed) {
    if ( npanes>int(_pane_map.size())) {
      throw COM_exception( COM_ERR_INIT_DONE_PANEMAP, append_frame
			   (_name,Window::init_done));
    }
    return;
  }

  // communicate pane mapping
  int flag; MPI_Initialized( &flag);
  if ( _comm == MPI_COMM_NULL) flag = 0;

  // Compute proc_map
  int nprocs;
  if ( flag) MPI_Comm_size( _comm, &nprocs); else nprocs = 1;

  // Obtain the number of panes.
  std::vector<int> npanes_all(nprocs);
  if ( flag)
    MPI_Allgather( &npanes, 1, MPI_INT, &npanes_all[0], 1, MPI_INT, _comm);
  else
    npanes_all[0] = npanes;
  
  std::vector<int> disps(nprocs+1); 
  disps[0]=0;
  for ( int i=0; i<nprocs; ++i) disps[i+1] = disps[i]+npanes_all[i];

  std::vector<int> pane_ids_all( disps[nprocs]);
  if ( flag) 
    MPI_Allgatherv( &pane_ids[0], npanes, MPI_INT, &pane_ids_all[0], 
		    &npanes_all[0], &disps[0], MPI_INT, _comm);
  else
    pane_ids_all = pane_ids;

  // Build process map
  _proc_map.clear();
  for ( int p=0; p<nprocs; ++p) {
    for ( int j=disps[p], jn=disps[p+1]; j<jn; ++j)
      _proc_map[ pane_ids_all[j]] = p;
  }
}


int Window::owner_rank( const int pane_id) const {
  COM_assertion_msg( _pane_map.size()<=_proc_map.size(),
		     "init_done must be called before owner_rank is called");
  
  Proc_map::const_iterator it = _proc_map.find( pane_id);
  if ( it==_proc_map.end()) return -1;
  else return it->second;
}

Attribute *Window::
get_attribute( const std::string &aname, char *loc, int *type, 
	       int *ncomp, std::string *unit) const throw(COM_exception) {
  // Special handing for connectivity tables
  if ( Connectivity::is_element_name( aname)) {
    // Set the arguments if not NULL.
    if ( loc) *loc = 'p';
    if ( type) *type = COM_INT;
    // Return the number of nodes.
    if ( ncomp) *ncomp = Connectivity::get_size_info(aname)
		  [Connectivity::SIZE_NNODES];
    if ( unit) *unit = "";
    return NULL; // A connectivity is not defined in Window scope.
  }
  else {
    // Obtain reference to the attribute
    Attr_map::const_iterator it = _attr_map.find( aname);
    if ( it == _attr_map.end())
      throw COM_exception(COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			  (_name+"."+aname,Window::get_attribute));
    Attribute &a = *it->second;

    // Set the arguments if not NULL.
    if ( loc) *loc = a.location();
    if ( type) *type = a.data_type();
    if ( ncomp) *ncomp = a.size_of_components();
    if ( unit) *unit = a.unit();
    return &a;
  }
}

template <class Attr>
void get_size_common( const Attr *a, int pid, int *nitem, int *ng) 
  throw( COM_exception)
{
  if ( pid==0 && a->location() != 'w')
    throw COM_exception( COM_ERR_NOT_A_WINDOW_ATTRIBUTE, append_frame
			 ( a->fullname(), Window::get_size));

  if ( nitem) *nitem = a->size_of_items();
  if ( ng)    *ng = a->size_of_ghost_items();
}

void Window::
get_size( const std::string &aname, int pid,
	  int *nitem, int *ng) const throw( COM_exception) {
  const Pane_friend *pn;
  try { pn = &(Pane_friend&)pane(pid); }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Window::get_size);
    throw ex;
  }
    
  // Special handing for connectivity
  if ( Connectivity::is_element_name( aname)) {
    // Get the corresponding attribute object.
    const COM::Connectivity *conn = pn->connectivity( aname);
    if ( conn==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   ( name()+"."+aname, Window::get_size));
    get_size_common( conn, pid, nitem, ng);
  }
  else {
    // Get the corresponding attribute object.
    const COM::Attribute *attr = pn->attribute( aname);
    if ( attr==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   ( name()+"."+aname, Window::get_size));

    get_size_common( attr, pid, nitem, ng);
  }
}

int Window::
get_status( const std::string &aname, int pid) const throw( COM_exception) {
  // If aname is empty, then check the status of the pane.
  if ( aname.empty()) {
    Pane_map::const_iterator pit = _pane_map.find( pid);
    if ( pit == _pane_map.end()) return -1;
    else return 0;
  }

  const Pane_friend *pn;
  try { pn = &(Pane_friend&)pane(pid); }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Window::get_status);
    throw ex;
  }
    
  // Special handing for connectivity
  if ( Connectivity::is_element_name( aname)) {
    // Get the corresponding attribute object.
    const COM::Connectivity *conn = pn->connectivity( aname);
    if ( conn==NULL) return -1;
    else return conn->status();
  }
  else {
    // Get the corresponding attribute object.
    const COM::Attribute *attr = pn->attribute( aname);
    if ( attr==NULL) return -1;
    else return attr->status();
  }
}

void Window::
get_parent( const std::string &aname, int pid, 
	    std::string &parent) const throw( COM_exception) {

  const Pane_friend *pn;
  try { 
    pn = &(Pane_friend&)pane(pid); 
    
    // Special handing for connectivity
    if ( Connectivity::is_element_name( aname)) {
      // Get the corresponding attribute object.
      const COM::Connectivity *conn = pn->connectivity( aname);
      if ( conn==NULL) 
	throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			     (name()+"."+aname,Window::get_parent));

      conn = conn->parent();
      if ( conn==NULL) parent.clear();
      else parent = conn->fullname();
    }
    else {
      // Get the corresponding attribute object.
      const COM::Attribute *attr = pn->attribute( aname);
      if ( attr==NULL) 
	throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			     (name()+"."+aname,Window::get_parent));

      if ( attr==NULL) parent.clear();
      else parent = attr->fullname();
    }
  } catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Window::get_parent);
    throw ex;
  }
}

template <class Attr>
void get_array_common( const Attr *a, int pid, 
		       Window::Pointer_descriptor &addr, 
		       int *strd, int *cap, bool is_const) throw(COM_exception) {
  if ( !is_const && a->is_const() )
    throw COM_exception( COM_ERR_ATTRIBUTE_CONST, append_frame
			 (a->fullname(),Window::get_array));

  if ( pid==0 && a->location() != 'w')
    throw COM_exception( COM_ERR_NOT_A_WINDOW_ATTRIBUTE, append_frame
			 ( a->fullname(), Window::get_array));

  addr.ptr = (void*)(a->pointer());
  if ( addr.dim==1) {
    addr.n1 = a->capacity()*a->stride();
  }
  else if ( addr.dim == 2) {
    if ( a->stride()>=a->size_of_components()) {
      addr.n1 = a->stride();
      addr.n2 = a->capacity();
    }
    else {
      addr.n1 = a->capacity();
      addr.n2 = a->size_of_components();
    }
  }
  else {
    // Scalars must have capacity 1!
    if ( addr.dim==0 && (a->capacity()!=1 || a->size_of_components()!=1))
      throw COM_exception( COM_ERR_INVALID_SIZE, append_frame
			   (a->fullname(),Window::get_array));
    if ( addr.dim>2)
      throw COM_exception( COM_ERR_INVALID_DIMENSION, append_frame
			   (a->fullname(),Window::get_array));
  }
    
  if ( cap)  *cap = a->capacity();
  if ( strd) *strd = a->stride();
}

void Window::get_array(const std::string &aname, const int pane_id,
		       Pointer_descriptor &addr,
		       int *strd, int *cap, bool is_const) throw(COM_exception)
{
  Pane_friend *pn;
  try { pn = &(Pane_friend&)pane(pane_id); }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Window::get_array);
    throw ex;
  }
    
  if ( Connectivity::is_element_name( aname)) {
    // Define as const reference to avoid exception.
    const Connectivity *con=pn->connectivity( aname);

    if ( con==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST,append_frame
			   (name()+"."+aname,Window::get_array));

    get_array_common( con, pane_id, addr, strd, cap, is_const);
  }
  else {
    // Define as const reference to avoid exception.
    const Attribute *a = pn->attribute( aname);

    if ( a==NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST,append_frame
			   (name()+"."+aname,Window::get_array));
    get_array_common( a, pane_id, addr, strd, cap, is_const);
  }
}

template <class Attr>
inline void copy_array_common( const Attr *a, int pid, void *val, int v_strd, 
			       int v_size, int offset) throw(COM_exception) {

  if ( pid==0 && a->location() != 'w')
    throw COM_exception( COM_ERR_NOT_A_WINDOW_ATTRIBUTE, append_frame
			 ( a->fullname(), Window::copy_size));

  const_cast<Attr*>(a)->copy_array( val, v_strd, v_size, 
				    offset, Attribute::COPY_OUT);
}

void Window::copy_array(const std::string &aname, const int pane_id,
			void *val, int v_strd, int v_size, 
			int offset) const throw(COM_exception)
{
  const Pane_friend *pn;
  try { pn = &(Pane_friend&)pane(pane_id); }
  catch ( COM_exception ex) {
    ex.msg = append_frame( ex.msg, Window::copy_array);
    throw ex;
  }

  if ( Connectivity::is_element_name( aname)) {
    const Connectivity *conn = pn->connectivity( aname);
    if ( conn == NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname, Window::copy_array));

    copy_array_common( conn, pane_id, 
		       val, v_strd, v_size, offset);
  }
  else {
    const Attribute *attr = pn->attribute( aname);
    if ( attr == NULL) 
      throw COM_exception( COM_ERR_ATTRIBUTE_NOTEXIST, append_frame
			   (name()+"."+aname, Window::copy_array));

    copy_array_common( attr, pane_id, 
		       val, v_strd, v_size, offset);
  }
}

void Window::
reinit_attr( Attribute *a, OP_Init op, void **addr, 
	     int strd, int cap) throw (COM_exception)
{
  int aid = a->id();

  if ( a->location()=='w') 
    // Initialize window attributes in dummy pane
    ((Pane_friend&)_dummy).reinit_attr( aid, op, addr, strd, cap);
  else {
    // Initialize other attributes in regular panes
    Pane *pn = a->pane();
    if ( pn->id()>0)
      ((Pane_friend*)pn)->reinit_attr( aid, op, addr, strd, cap);
    else {
      if ( aid == COM_ALL || aid == COM_ATTS)
	((Pane_friend&)_dummy).reinit_attr( aid, op, addr, strd, cap);

      COM_assertion( op != Pane::OP_SET && op != Pane::OP_SET_CONST );
      // Loop through the panes to initialize each pane
      for (Pane_map::iterator it=_pane_map.begin(), iend=_pane_map.end();
	   it != iend; ++it) {
	((Pane_friend*)it->second)->reinit_attr( aid, op, NULL, strd, cap);
      }
      if ( addr) *addr= NULL; // Do not return any address.
      return;
    }
  }
}

void Window::
reinit_conn( Connectivity *con, OP_Init op, int **addr, 
	     int strd, int cap) throw (COM_exception)
{

  Pane *pn = con->pane();
  if ( pn->id()==0) {
    COM_assertion( op != Pane::OP_SET && op != Pane::OP_SET_CONST);
    // Loop through the panes to proecess the connectivity in each pane.
    for (Pane_map::iterator it=_pane_map.begin(), iend=_pane_map.end();
	 it != iend; ++it) {
      Connectivity *c=((Pane_friend*)it->second)->connectivity( con->name());
      if ( c && !c->parent()) 
	((Pane_friend*)it->second)->reinit_conn( c, op, NULL, strd, cap);
    }
    if ( addr) *addr= NULL; // Do not return address.
    return;
  }
  else {
    ((Pane_friend*)pn)->reinit_conn( con, op, addr, strd, cap);
  }
}

Pane &Window::pane( const int pid, bool insert) throw( COM_exception)
{
  if ( pid==0) return _dummy;

  COM_assertion( pid>0);
  Pane_map::iterator pit = _pane_map.find( pid);
  if ( pit == _pane_map.end()) {
    if (insert) 
      return *(_pane_map[pid] = new Pane( &_dummy, pid));
    else {
        // print missing pane ID and known IDs in the pane map
      std::cerr << "No such Pane ID: " << pid << std::endl;
      std::cerr << "While the known Pane IDs are: [ ";
      Pane_map::iterator pit;
      for ( pit=_pane_map.begin(); pit != _pane_map.end(); ++pit)
        std::cerr << pit->first << " ";
      std::cerr << "]" << std::endl;
      throw COM_exception( COM_ERR_PANE_NOTEXIST, append_frame
			   (_name,Window::pane));
    }
  }

  return *pit->second;
}

const Pane &Window::pane( const int pid) const throw( COM_exception)
{
  if ( pid==0) return _dummy;

  COM_assertion( pid>0);
  Pane_map::const_iterator pit = _pane_map.find( pid);
  if ( pit == _pane_map.end()) {
    throw COM_exception( COM_ERR_PANE_NOTEXIST, append_frame
			 (_name,Window::pane));
  }

  return *pit->second;
}

// Obtain the panes of the window on a give process.
void Window::panes( std::vector<int> &pane_ids, int rank) 
{
   
  pane_ids.clear();

  if ( rank == -2) {
    pane_ids.reserve( _pane_map.size());

    for ( Pane_map::iterator it=_pane_map.begin(); it != _pane_map.end(); ++it)
      pane_ids.push_back( it->first);
  }
  else {
    COM_assertion_msg( _status == STATUS_NOCHANGE, 
		       "Can only obtain panes after calling window_init_done");

    std::map<int,int>::const_iterator it, iend;
    for ( it=_proc_map.begin(), iend=_proc_map.end(); it!= iend; ++it) {
      if ( rank==-1 || it->second == rank) 
	pane_ids.push_back( it->first);
    }
  }
}

// Obtain all the panes of the window.
void Window::panes( std::vector<Pane*> &ps) 
{
  ps.reserve( _pane_map.size());
  Pane_map::iterator it=_pane_map.begin();
  for ( ; it != _pane_map.end(); ++it)
    ps.push_back( it->second);
}

Attribute* Window::
attribute( const std::string &aname) throw(COM_exception) 
{
  if ( !Attribute::is_digit(aname[0])) {
    Attr_map::iterator it = _attr_map.find( aname);
    if ( it == _attr_map.end())
      return NULL;
    else 
      return it->second;
  }
  else {
    std::string::size_type start = aname.find( '-'); 
    if ( start == aname.npos) 
      throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_NAME, append_frame
			   (_name+"."+aname,Window::attribute));
			   
    Attr_map::iterator it = _attr_map.find( &aname[start+1]);

    if ( it != _attr_map.end()) {
      Attribute *att = it->second;
      if ( att->size_of_components()>1) { 
	// Get the subcomponent
	int i = std::atoi( aname.c_str()); 

	if ( i<=0 || i>att->size_of_components()) 
	  throw COM_exception( COM_ERR_INVALID_ATTRIBUTE_NAME, append_frame
			       (_name+"."+aname,Window::attribute));
	return attribute( att->id()+i);
      }
    }
    
    return NULL;
  }
}

Function* Window::
function( const std::string &fname) 
{
  Func_map::iterator it = _func_map.find( fname);
  if ( it == _func_map.end())
    return NULL;
  else 
    return &it->second;
}

COM_END_NAME_SPACE







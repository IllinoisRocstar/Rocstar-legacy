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
#include <functional>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include "Rocblas.h"

//Function object that implements an assignment.
template <class T_src, class T_trg>
struct Rocblas::assn : std::unary_function<T_src,T_trg> {
  void operator()(T_trg &x, const T_src &y) 
  { x = y; }
};

//Function object that implements a random number generator.
template <class T>
struct Rocblas::random : std::unary_function<T,T> {
  void operator()(T &z, const T &a) 
  { int s = std::rand(); z = s%a; }
};

template <>
struct Rocblas::random<double> : std::unary_function<double,double> {
  void operator()(double &z, const double &a) 
  { int s = std::rand(); z = (a*s)/RAND_MAX; }
};

//Function object that implements a swap.
template <class T>
struct Rocblas::swapp : std::unary_function<T,T> {
  void operator()(T &x, T &y) {
    T temp = x;
    x = y;
    y = temp;
  }
};

//Function object that implements a max operation.
template <class T>
struct Rocblas::maxv : std::unary_function<T,T> {
  void operator()(const T &x, T &y) {
    y = std::max( x, y);
  }
};

//Function object that implements a min operation.
template <class T>
struct Rocblas::minv : std::unary_function<T,T> {
  void operator()(const T &x, T &y) {
    y = std::min( x, y);
  }
};

//Function object that implements a sum operation.
template <class T>
struct Rocblas::sumv : std::unary_function<T,T> {
  void operator()(const T &x, T &y) {
    y = x + y;
  }
};

//Function object that implements a negation.
template <class T>
struct Rocblas::nega : std::unary_function<T,T> {
  void operator()(T &x, const T &y) {
    x = -y;
  }
};

//Function object that implements sqrt.
template <class T>
struct Rocblas::sqrta : std::unary_function<T,T> {
  void operator()(T &x, const T &y) {
    x = (T)std::sqrt((double)y);
  }
};

//Function object that implements acos.
template <class T>
struct Rocblas::acosa : std::unary_function<T,T> {
  void operator()(T &x, const T &y) {
    x = (T)std::acos((double)y);
  }
};

template < class T1, class T2> bool compare_types() { return true; }
template <> bool compare_types<int,int>() { return false; }
template <> bool compare_types<char,char>() { return false; }
template <> bool compare_types<double,double>() { return false; }

// Performs the operation:  y op z
template <class FuncType, int ytype>
void Rocblas::gen2arg( Attribute *z, void *yin, FuncType opp) {
  typedef typename FuncType::argument_type  argument_type;
  typedef typename FuncType::result_type    result_type;
  
  int num_dims = z->size_of_components();

  std::vector<Pane*> zpanes;
  std::vector<Pane*>::iterator zit, zend;
  z->window()->panes(zpanes);

  std::vector<Pane*> ypanes;
  Pane **yit=NULL;
  Attribute *y=NULL;
  argument_type *yval=NULL;
  
  if ( ytype == BLAS_VOID) {
    yval = reinterpret_cast<argument_type *>( yin);

    COM_assertion_msg( yval, std::string("Caught NULL pointer in scalar \
operand when processing "+z->fullname()).c_str());
  }
  else {
    y = reinterpret_cast<Attribute *>(yin);
    bool type_coercion = compare_types<argument_type,result_type>();

    COM_assertion_msg( type_coercion || 
		       COM_compatible_types(z->data_type(), y->data_type()),
		       (std::string("Incompatible data types between ")+
			z->fullname()+" and "+y->fullname()).c_str());

    if ( !y->is_windowed()) {
      y->window()->panes(ypanes);
      COM_assertion_msg(zpanes.size() == ypanes.size(),
			(std::string("Numbers of panes do not match between ")+
			 y->window()->name()+" and "+z->window()->name()).c_str());
      yit = &ypanes[0];
    }
    else {
      if ( y->size_of_items()!=1) {
	std::cout << "Rocbals Error: The size-of-items of attribute "
		  << y->fullname() << " on pane " << y->pane()->id()
		  << " is " << y->size_of_items() << std::endl;
	COM_assertion_msg( y->size_of_items()==1, "If an argument is a window \
attribute, then its number of items must be 1.");
      }

      yval = reinterpret_cast<argument_type *>( y->pointer());

      COM_assertion_msg( yval, (std::string("Caught NULL pointer in ")+
				y->fullname()).c_str());
    }
  }

  const int ynum_dims = ytype != BLAS_VOID?y->size_of_components():0;
  COM_assertion_msg( ytype==BLAS_VOID || (ynum_dims==1 || ynum_dims==num_dims),
		     (std::string("Numbers of components do not match between ")+
		      y->fullname()+" and "+z->fullname()).c_str());

  if ( z->is_windowed()) {
    COM_assertion_msg( ytype == BLAS_VOID || y->is_windowed(),
		       (std::string("Wrong type of operand")+
			y->fullname()).c_str());
    int zs=get_stride<BLAS_VEC2D>(z);
    int ys=get_stride<ytype>(y);
    result_type *zval = (result_type *)z->pointer();
    int   length = z->size_of_items();

    COM_assertion_msg( length==0 || ytype == BLAS_VOID || zval && yval, 
		       (std::string("Caught NULL pointer in w-attribute ")+
			z->fullname()+" or "+y->fullname()).c_str());

    if (  (length==1 || num_dims == zs) && ytype != BLAS_VEC) {
      for ( Size i = 0, s = num_dims*length; i < s; ++i, ++zval)
	opp( *zval, getref<argument_type,ytype,0>(yval,i,0,1));
    }
    else {
      for(int i = 0; i < num_dims; ++i) {
	Attribute *z_i = num_dims==1?z:z+i+1;
	zval = (result_type *)z_i->pointer();
	zs = get_stride<BLAS_VEC2D>(z_i);

	if ( ytype != BLAS_VOID) {
	  Attribute *y_i=ynum_dims==1?y:y+i+1;
	  yval = reinterpret_cast<argument_type *>( y_i->pointer());
	  ys = get_stride<ytype>( y_i);
	}

	for(int j=0; j<length; ++j, zval+=zs)
	  opp( *zval, getref<argument_type,ytype,1>(yval,j,i,ys));
      }
    }
    return;
  }
  // otherwise:
  
  for( zit=zpanes.begin(), zend=zpanes.end(); zit!=zend; 
       ++zit, yit+=( ytype!=BLAS_VOID&&yit)) {
    Attribute *pz = (*zit)->attribute(z->id());
    const int  length = pz->size_of_items();
    int  zstrd=get_stride<BLAS_VEC2D>(pz);
    const bool zstg = length>1 && num_dims!=zstrd;

    Attribute *py = ytype!=BLAS_VOID&&yit?(*yit)->attribute(y->id()):y;
    int ystrd = get_stride<ytype>(py);
    const bool ystg = py && (ynum_dims!=num_dims || ynum_dims!=ystrd);
    COM_assertion_msg( ytype!=BLAS_SCNE && ytype!=BLAS_VEC2D ||
		       length == int(py->size_of_items()) || ystrd==0,
		       (std::string("Numbers of items do not match between ")+
			y->fullname()+" and "+z->fullname()+
			" on pane "+to_str((*zit)->id())).c_str());

    // Optimized version for contiguous attributes
    if ( !zstg && !ystg && ytype != BLAS_VEC &&
	 (ytype != BLAS_SCNE || num_dims==1)) {
      result_type *zval = reinterpret_cast<result_type *>(pz->pointer());

      if ( ytype != BLAS_VOID && yit)
	yval = reinterpret_cast<argument_type *>(py->pointer());

	COM_assertion_msg( length==0 || ytype == BLAS_VOID || zval && yval, 
			   (std::string("Caught NULL pointer in ")+
			    z->fullname()+" or "+y->fullname()+" on pane "+
			    to_str( (*zit)->id())).c_str());

      if ( zval) 
	//Loop for each element/node and for each dimension
	for(int i = 0, s = length*num_dims; i < s; ++i, ++zval)
	  opp(*zval, getref<argument_type,ytype,0>(yval,i,0,1));
    }
    else { // General version
      //Loop for each dimension.
      for(int i = 0; i < num_dims; ++i) {
	Attribute *pz_i = num_dims==1?pz:(*zit)->attribute(z->id()+i+1);
	result_type *zval = (result_type *)pz_i->pointer();
	zstrd=get_stride<BLAS_VEC2D>(pz_i);

	if ( ytype != BLAS_VOID && yit) {
	  Attribute *py_i=ynum_dims==1?py:(*yit)->attribute(y->id()+i+1);
	  yval = reinterpret_cast<argument_type *>( py_i->pointer());
	  ystrd = get_stride<ytype>( py_i);
	}

	COM_assertion_msg( length==0 || ytype == BLAS_VOID || zval && yval, 
			   (std::string("Caught NULL pointer in ")+
			    z->fullname()+" or "+y->fullname()+" on pane "+
			    to_str( (*zit)->id())).c_str());

	if ( zval) 
	  for(int j=0; j < length; ++j, zval+=zstrd) 
	    opp( *zval, getref<argument_type,ytype,1>(yval,j,i,ystrd));
      }
    }
  }
}


//Wrapper for swap.
void Rocblas::swap(Attribute *x, Attribute *y) {
  switch ( x->id()) {
  case COM::COM_ATTS: {
    COM_assertion_msg( y->id() == COM::COM_ATTS,
		       "Aggregate attributes for swap must match.");

    // Obtain all the attributes of their corresponding windows
    std::vector< Attribute*>  x_attrs;
    x->window()->attributes( x_attrs);

    std::vector< Attribute*>  y_attrs;
    y->window()->attributes( y_attrs);
    
    COM_assertion_msg( x_attrs.size() == y_attrs.size(),
		       (std::string("Numbers of attributes do not match between ")+
			x->window()->name()+" and "+y->window()->name()).c_str());
    // Copy all the individual attributes
    for ( int i=x_attrs.size()-1; i>=0; --i)
      swap( x_attrs[i], y_attrs[i]);
    return;
  }
  case COM::COM_MESH:
  case COM::COM_PMESH:
  case COM::COM_CONN:
  case COM::COM_ALL:
    COM_assertion_msg(false, "Only the aggregate attribute atts is supported");
  default: ;
  }

  COM_Type att_type = x->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    gen2arg<swapp<int>,BLAS_VEC2D>(x, y, swapp<int>());
  else if(att_type == COM_CHAR)
    gen2arg<swapp<char>,BLAS_VEC2D>(x, y, swapp<char>());
  else {
    COM_assertion_msg(att_type == COM_DOUBLE || att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       x->fullname()).c_str());
    gen2arg<swapp<double>,BLAS_VEC2D>(x, y, swapp<double>());
  }
}

template < class Op>
void Rocblas::copy_helper( const Attribute *x, Attribute *z) {
  int xncomp = x->size_of_components();

  if ( x->is_windowed()) {
    if ( xncomp == 1)
      gen2arg<Op,BLAS_SCALAR>(z, const_cast<Attribute*>(x), Op());
    else
      gen2arg<Op,BLAS_VEC>(z, const_cast<Attribute*>(x), Op());
  }
  else {
    if ( xncomp == 1)
      gen2arg<Op,BLAS_SCNE>(z, const_cast<Attribute*>(x), Op());
    else
      gen2arg<Op,BLAS_VEC2D>(z, const_cast<Attribute*>(x), Op());
  }
}

//Wrapper for copy.
void Rocblas::copy(const Attribute *x, Attribute *z) {
  switch ( x->id()) {
  case COM::COM_ATTS: {
    COM_assertion_msg( z->id() == COM::COM_ATTS,
		       "Aggregate attributes for copy must match.");

    // Obtain all the attributes of their corresponding windows
    std::vector< const Attribute*>  x_attrs;
    x->window()->attributes( x_attrs);

    std::vector< Attribute*>  z_attrs;
    z->window()->attributes( z_attrs);
    
    // Copy all the individual attributes
    COM_assertion_msg( x_attrs.size() == z_attrs.size(),
		       (std::string("Numbers of attributes do not match between ")+
			x->window()->name()+" and "+z->window()->name()).c_str());

    for ( int i=x_attrs.size()-1; i>=0; --i)
      copy( x_attrs[i], z_attrs[i]);
    return;
  }
  case COM::COM_MESH:
  case COM::COM_PMESH:
  case COM::COM_CONN:
  case COM::COM_ALL:
    COM_assertion_msg(false, "Only the aggregate attribute atts is supported");
  default: ;
  }

  COM_Type src_type = x->data_type(), trg_type = z->data_type();

  if ( src_type == COM_INT || src_type == COM_INTEGER) {
    if ( trg_type == COM_DOUBLE || trg_type == COM_DOUBLE_PRECISION)
      copy_helper<assn<int,double> >( x, z);
    else
      copy_helper<assn<int,int> >( x, z);
  }
  else if ( src_type == COM_CHAR) {
    if ( trg_type == COM_INT || trg_type == COM_INTEGER)
      copy_helper<assn<char,int> >( x, z);
    else if ( trg_type == COM_DOUBLE || trg_type == COM_DOUBLE_PRECISION)
      copy_helper<assn<char,double> >( x, z);
    else
      copy_helper<assn<char,char> >( x, z);
  }
  else {
    COM_assertion_msg( src_type==COM_DOUBLE || src_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			x->fullname()).c_str());
    copy_helper<assn<double,double> >( x, z);
  }
}

//Wrapper for rand.
void Rocblas::rand(const Attribute *x, Attribute *z) {
  switch ( x->id()) {
  case COM::COM_ATTS: {
    COM_assertion_msg( z->id() == COM::COM_ATTS,
		       "Aggregate attributes for copy must match.");

    // Obtain all the attributes of their corresponding windows
    std::vector< const Attribute*>  x_attrs;
    x->window()->attributes( x_attrs);

    std::vector< Attribute*>  z_attrs;
    z->window()->attributes( z_attrs);
    
    // Copy all the individual attributes
    COM_assertion_msg( x_attrs.size() == z_attrs.size(),
		       (std::string("Numbers of attributes do not match between ")+
			x->window()->name()+" and "+z->window()->name()).c_str());

    for ( int i=x_attrs.size()-1; i>=0; --i)
      rand( x_attrs[i], z_attrs[i]);
    return;
  }
  case COM::COM_MESH:
  case COM::COM_PMESH:
  case COM::COM_CONN:
  case COM::COM_ALL:
    COM_assertion_msg(false, "Only the aggregate attribute atts is supported");
  default: ;
  }

  COM_Type att_type = x->data_type();
  const int xncomp = x->size_of_components();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<random<int>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), random<int>());
      else
	gen2arg<random<int>,BLAS_VEC>(z, const_cast<Attribute*>(x), random<int>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<random<int>,BLAS_SCNE>(z, const_cast<Attribute*>(x), random<int>());
      else
	gen2arg<random<int>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), random<int>());
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			x->fullname()).c_str());
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<random<double>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), random<double>());
      else
	gen2arg<random<double>,BLAS_VEC>(z, const_cast<Attribute*>(x), random<double>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<random<double>,BLAS_SCNE>(z, const_cast<Attribute*>(x), random<double>());
      else
	gen2arg<random<double>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), random<double>());
    }
  }
}

//Wrapper for neg.
void Rocblas::neg(const Attribute *x, Attribute *z) {
  COM_Type att_type = x->data_type();
  const int xncomp = x->size_of_components();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<nega<int>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), nega<int>());
      else
	gen2arg<nega<int>,BLAS_VEC>(z, const_cast<Attribute*>(x), nega<int>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<nega<int>,BLAS_SCNE>(z, const_cast<Attribute*>(x), nega<int>());
      else
	gen2arg<nega<int>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), nega<int>());
    }
  }
  else if ( att_type == COM_CHAR) {
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<nega<char>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), nega<char>());
      else
	gen2arg<nega<char>,BLAS_VEC>(z, const_cast<Attribute*>(x), nega<char>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<nega<char>,BLAS_SCNE>(z, const_cast<Attribute*>(x), nega<char>());
      else
	gen2arg<nega<char>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), nega<char>());
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			x->fullname()).c_str());
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<nega<double>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), nega<double>());
      else
	gen2arg<nega<double>,BLAS_VEC>(z, const_cast<Attribute*>(x), nega<double>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<nega<double>,BLAS_SCNE>(z, const_cast<Attribute*>(x), nega<double>());
      else
	gen2arg<nega<double>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), nega<double>());
    }
  }
}

//Wrapper for sqrt.
void Rocblas::sqrt(const Attribute *x, Attribute *z) {
  COM_Type att_type = x->data_type();
  const int xncomp = x->size_of_components();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<sqrta<int>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), sqrta<int>());
      else
	gen2arg<sqrta<int>,BLAS_VEC>(z, const_cast<Attribute*>(x), sqrta<int>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<sqrta<int>,BLAS_SCNE>(z, const_cast<Attribute*>(x), sqrta<int>());
      else
	gen2arg<sqrta<int>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), sqrta<int>());
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			x->fullname()).c_str());
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<sqrta<double>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), sqrta<double>());
      else
	gen2arg<sqrta<double>,BLAS_VEC>(z, const_cast<Attribute*>(x), sqrta<double>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<sqrta<double>,BLAS_SCNE>(z, const_cast<Attribute*>(x), sqrta<double>());
      else
	gen2arg<sqrta<double>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), sqrta<double>());
    }
  }
}

//Wrapper for acos.
void Rocblas::acos(const Attribute *x, Attribute *z) {
  COM_Type att_type = x->data_type();
  const int xncomp = x->size_of_components();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<acosa<int>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), acosa<int>());
      else
	gen2arg<acosa<int>,BLAS_VEC>(z, const_cast<Attribute*>(x), acosa<int>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<acosa<int>,BLAS_SCNE>(z, const_cast<Attribute*>(x), acosa<int>());
      else
	gen2arg<acosa<int>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), acosa<int>());
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			x->fullname()).c_str());
    if ( x->is_windowed()) {
      if ( xncomp == 1)
	gen2arg<acosa<double>,BLAS_SCALAR>(z, const_cast<Attribute*>(x), acosa<double>());
      else
	gen2arg<acosa<double>,BLAS_VEC>(z, const_cast<Attribute*>(x), acosa<double>());
    }
    else {
      if ( xncomp == 1)
	gen2arg<acosa<double>,BLAS_SCNE>(z, const_cast<Attribute*>(x), acosa<double>());
      else
	gen2arg<acosa<double>,BLAS_VEC2D>(z, const_cast<Attribute*>(x), acosa<double>());
    }
  }
}

//Operation wrapper for copy (x is a scalar pointer).
void Rocblas::copy_scalar(const void *x, Attribute *z) {
  COM_Type att_type = z->data_type();

  typedef assn<int,int>          assn_int;
  typedef assn<char,char>        assn_chr;
  typedef assn<double,double>    assn_dbl;
  
  if ( att_type == COM_INT || att_type == COM_INTEGER)
    gen2arg<assn_int,BLAS_VOID>(z, const_cast<void*>(x), assn_int());
  else if ( att_type == COM_DOUBLE || att_type == COM_DOUBLE_PRECISION) {
    gen2arg<assn_dbl,BLAS_VOID>(z, const_cast<void*>(x), assn_dbl());
  }
  else {
    COM_assertion_msg( att_type==COM_CHAR,
		       (std::string("Unsupported data type in ")+
			z->fullname()).c_str());
    gen2arg<assn_chr,BLAS_VOID>(z, const_cast<void*>(x), assn_chr());  
  }
}

// Generate a random number between 0 and $a$ for each entry in z
void Rocblas::rand_scalar(const void *a, Attribute *z) {
  COM_Type att_type = z->data_type();
  
  if ( att_type == COM_INT || att_type == COM_INTEGER)
    gen2arg<random<int>,BLAS_VOID>(z, const_cast<void*>(a), random<int>());
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			z->fullname()).c_str());
    gen2arg<random<double>,BLAS_VOID>(z, const_cast<void*>(a), random<double>());
  }
}

enum { BLAS_MIN=1, BLAS_MAX, BLAS_SUM};

MPI_Op convert2mpiop( int i) {
  switch (i) {
  case BLAS_MIN: return MPI_MIN;
  case BLAS_MAX: return MPI_MAX;
  case BLAS_SUM: return MPI_SUM;
  default: COM_assertion(false); return MPI_SUM; // Should never reach here.
  }
}

//Wrapper for reduce operations (including max, min, sum).
template <class OPint, class OPdbl, int OPMPI>
void Rocblas::reduce_MPI(const Attribute *x, Attribute *z, 
			 const MPI_Comm *comm, int initialI, double initialD) {
  COM_Type att_type = z->data_type();
  const int zncomp = z->size_of_components();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    copy_scalar( &initialI, z);

    if ( z->is_windowed()) {
      if ( zncomp == 1)
	gen2arg<OPint,BLAS_SCALAR>(const_cast<Attribute*>(x), z, OPint());
      else
	gen2arg<OPint,BLAS_VEC>(const_cast<Attribute*>(x), z, OPint());

      if ( comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
	std::vector<int> t((int*)z->pointer(), ((int*)z->pointer())+zncomp);
	MPI_Allreduce( &t[0], z->pointer(), zncomp, MPI_INT, 
		       convert2mpiop(OPMPI), *comm);
      }
    }
    else {
      if ( zncomp == 1)
	gen2arg<OPint,BLAS_SCNE>(const_cast<Attribute*>(x), z, OPint());
      else
	gen2arg<OPint,BLAS_VEC2D>(const_cast<Attribute*>(x), z, OPint());
    }
  }
  else if ( att_type == COM_CHAR) {
    copy_scalar( &initialI, z);

    if ( z->is_windowed()) {
      if ( zncomp == 1)
	gen2arg<OPint,BLAS_SCALAR>(const_cast<Attribute*>(x), z, OPint());
      else
	gen2arg<OPint,BLAS_VEC>(const_cast<Attribute*>(x), z, OPint());

      if ( comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
	std::vector<char> t((char*)z->pointer(), ((char*)z->pointer())+zncomp);
	MPI_Allreduce( &t[0], z->pointer(), zncomp, MPI_CHAR, 
		       convert2mpiop(OPMPI), *comm);
      }
    }
    else {
      if ( zncomp == 1)
	gen2arg<OPint,BLAS_SCNE>(const_cast<Attribute*>(x), z, OPint());
      else
	gen2arg<OPint,BLAS_VEC2D>(const_cast<Attribute*>(x), z, OPint());
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			z->fullname()).c_str());
    copy_scalar( &initialD, z);

    if ( z->is_windowed()) {
      if ( zncomp == 1)
	gen2arg<OPdbl,BLAS_SCALAR>(const_cast<Attribute*>(x), z, OPdbl());
      else
	gen2arg<OPdbl,BLAS_VEC>(const_cast<Attribute*>(x), z, OPdbl());

      if ( comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
	std::vector<double> t( (double*)z->pointer(), 
			       ((double*)z->pointer())+zncomp);
	MPI_Allreduce( &t[0], z->pointer(), zncomp, MPI_DOUBLE, 
		       convert2mpiop(OPMPI), *comm);
      }
    }
    else {
      if ( zncomp == 1)
	gen2arg<OPdbl,BLAS_SCNE>(const_cast<Attribute*>(x), z, OPdbl());
      else
	gen2arg<OPdbl,BLAS_VEC2D>(const_cast<Attribute*>(x), z, OPdbl());
    }
  }
}

//Wrapper for max.
void Rocblas::max_MPI(const Attribute *x, Attribute *z, const MPI_Comm *comm) {
  reduce_MPI< maxv<int>, maxv<double>, BLAS_MAX>( x, z, comm, 
						  -0x7FFFFFFF, -HUGE_VAL);
}

//Wrapper for min.
void Rocblas::min_MPI(const Attribute *x, Attribute *z, const MPI_Comm *comm) {
  reduce_MPI< minv<int>, minv<double>, BLAS_MIN>( x, z, comm, 
						 0x7FFFFFFF, HUGE_VAL);
}

//Wrapper for sum.
void Rocblas::sum_MPI(const Attribute *x, Attribute *z, const MPI_Comm *comm) {
  reduce_MPI< sumv<int>, sumv<double>, BLAS_SUM>( x, z, comm, 0, 0);
}

//Operation wrapper for reduce operations (x is a scalar pointer).
template <class OPint, class OPdbl, int OPMPI>
void Rocblas::reduce_scalar_MPI(const Attribute *x, void *y, 
				const MPI_Comm *comm, int initialI, double initialD) {
  COM_Type att_type = x->data_type();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    *(int*)y = initialI;
    gen2arg<OPint,BLAS_VOID>(const_cast<Attribute*>(x), y, OPint());

    if ( comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
      int t = *(int*)y;
      MPI_Allreduce( &t, y, 1, MPI_INT, convert2mpiop(OPMPI), *comm);
    }
  }
  else if ( att_type == COM_CHAR) {
    *(char*)y = initialI;
    gen2arg<OPint,BLAS_VOID>(const_cast<Attribute*>(x), y, OPint());

    if ( comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
      char t = *(char*)y;
      MPI_Allreduce( &t, y, 1, MPI_CHAR, convert2mpiop(OPMPI), *comm);
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			x->fullname()).c_str());
    *(double*)y = initialD;
    gen2arg<OPdbl,BLAS_VOID>(const_cast<Attribute*>(x), y, OPdbl());

    if ( comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
      double t = *(double*)y;
      MPI_Allreduce( &t, y, 1, MPI_DOUBLE, convert2mpiop(OPMPI), *comm);
    }
  }
}

//Operation wrapper for max (y is a scalar pointer).
void Rocblas::max_scalar_MPI(const Attribute *x, void *y, 
			     const MPI_Comm* comm) {
  reduce_scalar_MPI< maxv<int>, maxv<double>, BLAS_MAX>(x, y, comm,
						        -0x7FFFFFFF, -HUGE_VAL);

}

//Operation wrapper for min (y is a scalar pointer).
void Rocblas::min_scalar_MPI(const Attribute *x, void *y, 
			     const MPI_Comm *comm) {
  reduce_scalar_MPI< minv<int>, minv<double>, BLAS_MIN>( x, y, comm,
							 0x7FFFFFFF, HUGE_VAL);
}

//Operation wrapper for sum (y is a scalar pointer).
void Rocblas::sum_scalar_MPI(const Attribute *x, void *y, 
			     const MPI_Comm *comm) {
  reduce_scalar_MPI< sumv<int>, sumv<double>, BLAS_SUM>( x, y, comm, 0, 0);
}







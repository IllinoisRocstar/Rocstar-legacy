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
#include "Rocblas.h"

// Performs the operation:  z = a*x op y
template <class data_type, int atype>
void Rocblas::axpy_gen( const void *ain, const Attribute *x,
			const Attribute *y, Attribute *z) {
  COM_assertion_msg(!x->is_windowed() && !y->is_windowed() && !z->is_windowed(),
		    "Unsupported attribute type");
  COM_assertion_msg( COM_compatible_types(x->data_type(),y->data_type()),
		     (std::string("Incompatible data types between ")+
		     x->fullname()+" and "+y->fullname()).c_str());
  COM_assertion_msg( COM_compatible_types(x->data_type(),z->data_type()),
		     (std::string("Incompatible data types between ")+
		      x->fullname()+" and "+z->fullname()).c_str());

  int num_dims = z->size_of_components();
  COM_assertion_msg(num_dims == x->size_of_components() && 
		    num_dims == y->size_of_components(),
		    (std::string("Numbers of components do not match between ")+
		     x->fullname()+" and "+y->fullname()).c_str());

  std::vector<const Pane*> apanes, xpanes, ypanes;
  std::vector<Pane*> zpanes;

  x->window()->panes(xpanes);
  y->window()->panes(ypanes);
  z->window()->panes(zpanes);

  COM_assertion_msg( xpanes.size() == ypanes.size(),
		     (std::string("Numbers of panes do not match between ")+
		      x->window()->name()+" and "+y->window()->name()).c_str());
  COM_assertion_msg( xpanes.size() == zpanes.size(),
		     (std::string("Numbers of panes do not match between ")+
		      x->window()->name()+" and "+z->window()->name()).c_str());

  std::vector<Pane*>::iterator zit, zend;
  std::vector<const Pane*>::const_iterator xit, yit;

  const Pane** ait=NULL;
  const Attribute *a=NULL;
  const data_type *aval=NULL;

  if ( atype == BLAS_VOID)
    aval = reinterpret_cast<const data_type *>( ain);
  else {
    a = reinterpret_cast<const Attribute*>(ain);

    if ( !a->is_windowed()) {
      a->window()->panes( apanes);
      COM_assertion_msg(xpanes.size() == ypanes.size(),
			(std::string("Numbers of panes do not match between ")+
			 x->window()->name()+" and "+y->window()->name()).c_str());
      ait = &apanes[0];
    }
    else { 
      COM_assertion_msg( a->size_of_items()==1, "Size of items do not match");
      aval = reinterpret_cast<const data_type *>( a->pointer());

      COM_assertion_msg( aval, (std::string("Caught NULL pointer in ")+
				a->fullname()).c_str());
    }
  }
  
  const int anum_dims = atype!=BLAS_VOID ? a->size_of_components() : 0;
  COM_assertion_msg( atype==BLAS_VOID || (anum_dims==1 || anum_dims==num_dims),
		    (std::string("Numbers of components do not match between ")+
		     a->fullname()+" and "+z->fullname()).c_str());

  for(zit=zpanes.begin(),zend=zpanes.end(),xit=xpanes.begin(),yit=ypanes.begin(); 
      zit != zend; ++zit, ait+=(atype!=BLAS_VOID&&ait), ++xit, ++yit) {
    Attribute *pz = (*zit)->attribute(z->id());
    const int  length = pz->size_of_items();
    int  zstrd=get_stride<BLAS_VEC2D>(pz);
    const bool zstg = num_dims!=zstrd;

    const Attribute *px = (*xit)->attribute(x->id());
    int  xstrd = get_stride<BLAS_VEC2D>(px);
    COM_assertion_msg(length == px->size_of_items() || xstrd==0,
		      (std::string("Numbers of items do not match between ")+
		       x->fullname()+" and "+z->fullname()+
		       " on pane "+to_str((*zit)->id())).c_str());
    const bool xstg = num_dims!=xstrd || xstrd==0;

    const Attribute *py = (*yit)->attribute(y->id());
    int  ystrd = get_stride<BLAS_VEC2D>(py);
    COM_assertion_msg(length == py->size_of_items() || ystrd==0,
		      (std::string("Numbers of items do not match between ")+
		       y->fullname()+" and "+z->fullname()+
		       " on pane "+to_str((*zit)->id())).c_str());

    const bool ystg = num_dims!=ystrd || ystrd==0;

    const Attribute *pa = (atype!=BLAS_VOID&&ait)?(*ait)->attribute(a->id()):a;
    int astrd = get_stride<atype>(pa);
    const bool astg = pa && (anum_dims!=num_dims || anum_dims!=astrd);
    COM_assertion_msg( atype!=BLAS_SCNE && atype!=BLAS_VEC2D ||
		       length == int(pa->size_of_items()) || astrd==0,
		       (std::string("Numbers of items do not match between ")+
			a->fullname()+" and "+z->fullname()+
			" on pane "+to_str((*zit)->id())).c_str());

    // Optimized version for contiguous attributes
    if ( !xstg && !zstg && !ystg && !astg && atype != BLAS_VEC 
	 && (atype != BLAS_SCNE || num_dims==1)) {
      const data_type *xval = (const data_type *)px->pointer();
      const data_type *yval = (const data_type *)py->pointer();
      data_type *zval = (data_type *)pz->pointer();

      // Get address for a if a is not window attribute
      if ( atype != BLAS_VOID && ait)
	aval = reinterpret_cast<const data_type *>(pa->pointer());

      //Loop for each element/node and for each dimension
      for(Size i = 0, s = length*num_dims; i<s; ++i, ++zval, ++xval, ++yval)
	*zval = getref<data_type,atype,0>(aval,i,0,1)* *xval+ *yval;
    }
    else { // General version
      //Loop for each dimension.
      for(int i = 0; i < num_dims; ++i) {
	Attribute *pz_i = num_dims==1?pz:(*zit)->attribute(z->id()+i+1);
	data_type *zval = (data_type *)pz_i->pointer();
	zstrd=get_stride<BLAS_VEC2D>(pz_i);

	const Attribute *px_i = num_dims==1?px:(*xit)->attribute(x->id()+i+1);
	const data_type *xval = (const data_type *)px_i->pointer();
	xstrd=get_stride<BLAS_VEC2D>(px_i);
	  
	const Attribute *py_i = num_dims==1?py:(*yit)->attribute(y->id()+i+1);
	const data_type *yval = (const data_type *)py_i->pointer();
	ystrd=get_stride<BLAS_VEC2D>(py_i);
	  
	if ( atype != BLAS_VOID && ait) {
	  const Attribute *pa_i=anum_dims==1?pa:(*ait)->attribute(a->id()+i+1);
	  aval = reinterpret_cast<const data_type *>( pa_i->pointer());
	  astrd = get_stride<atype>( pa_i);
	}

	//Loop for each element/node.
	for(int j=0; j<length; ++j, xval+=xstrd, zval+=zstrd, yval+=ystrd)
	  *zval = getref<data_type,atype,1>(aval,j,i,astrd) * *xval + *yval;
      }
    } // end if
  } // end for
}


//Operation wrapper for z = ax + y.
void Rocblas::axpy( const Attribute *a, const Attribute *x, const Attribute *y,
		   Attribute *z) {
  COM_Type att_type = z->data_type();
  const int ancomp = a->size_of_components();

  if(att_type == COM_INT || att_type == COM_INTEGER) {
    if ( a->is_windowed()) {
      if ( ancomp == 1)
	axpy_gen<int,BLAS_SCALAR>(a, x, y, z);
      else
	axpy_gen<int,BLAS_VEC>(a, x, y, z);
    }
    else {
      if ( ancomp == 1)
	axpy_gen<int,BLAS_SCNE>(a, x, y, z);
      else
	axpy_gen<int,BLAS_VEC2D>(a, x, y, z);
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		       (std::string("Unsupported data type in ")+
			z->fullname()).c_str());

    if ( a->is_windowed()) {
      if ( ancomp == 1)
	axpy_gen<double,BLAS_SCALAR>(a, x, y, z);
      else
	axpy_gen<double,BLAS_VEC>(a, x, y, z);
    }
    else {
      if ( ancomp == 1)
	axpy_gen<double,BLAS_SCNE>(a, x, y, z);
      else
	axpy_gen<double,BLAS_VEC2D>(a, x, y, z);
    }
  }
}

//Operation wrapper for z = ax + y (a is a scalar pointer).
void Rocblas::axpy_scalar( const void *a, const Attribute *x, const Attribute *y,
			   Attribute *z) {
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    axpy_gen<int,BLAS_VOID>(a, x, y, z);
  else if (att_type == COM_DOUBLE || att_type == COM_DOUBLE_PRECISION)
    axpy_gen<double,BLAS_VOID>(a, x, y, z);
}







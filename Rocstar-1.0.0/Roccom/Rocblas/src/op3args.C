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
#include <cmath>
#include <cstdlib>

//Function object that implements a limit1 operation.
template <class T>
struct Rocblas::limit1v : std::binary_function<T,T,T> {
  T operator()(T x, T y) {
    if ( x>=0&&y>=0 || x<=0&&y<=0)
      return std::abs(x)<std::abs(y)?x:y;
    else
      return 0;
  }
};

//Function object that implements a maxof operation.
template <class T>
struct Rocblas::maxof : std::binary_function<T,T,T> {
  T operator()(T x, T y) {
    if (x<y) return y;
    else return x;
  }
};

// Performs the operation:  z = x op y
template <class FuncType, int ytype>
void Rocblas::calc( Attribute *z, const Attribute *x, const void *yin,
		    FuncType opp, bool swap)
{
  typedef typename FuncType::result_type data_type;

  COM_assertion_msg( !x->is_windowed() && !z->is_windowed(),
		     (std::string("Unsupported data type in ")+
		      x->fullname()).c_str());
  COM_assertion_msg( COM_compatible_types(x->data_type(),z->data_type()),
		     (std::string("Incompatible data types between ")+
		      x->fullname()+" and "+z->fullname()).c_str());

  int num_dims = z->size_of_components();
  COM_assertion_msg( num_dims == x->size_of_components(),
		     (std::string("Numbers of components do not match between ")+
		      x->fullname()+" and "+z->fullname()).c_str());

  std::vector<Pane*> zpanes;
  std::vector<const Pane*> xpanes, ypanes;

  z->window()->panes(zpanes);
  x->window()->panes(xpanes);

  COM_assertion_msg( xpanes.size() == zpanes.size(),
		     (std::string("Numbers of panes do not match between ")+
		      x->window()->name()+" and "+z->window()->name()).c_str());

  std::vector<Pane*>::iterator zit, zend;
  std::vector<const Pane*>::const_iterator xit;
  const Pane **yit=NULL;

  const Attribute *y=NULL;
  const data_type *yval=NULL;

  // Obtain yval for BLAS_VOID or window attributes
  if ( ytype==BLAS_VOID)
    yval = reinterpret_cast<const data_type *>( yin);
  else {
    y = reinterpret_cast<const Attribute *>(yin);

    if ( !y->is_windowed()) {
      y->window()->panes(ypanes);
      COM_assertion_msg(xpanes.size() == ypanes.size(),
			(std::string("Numbers of panes do not match between ")+
			 x->window()->name()+" and "+
			 y->window()->name()).c_str());
      yit = &ypanes[0];
    }
    else {
      COM_assertion_msg( y->size_of_items()==1, 
			 (std::string("Numbers of items do not match between ")+
			  x->fullname()+" and "+y->fullname()).c_str());

      yval = reinterpret_cast<const data_type *>( y->pointer());

      COM_assertion_msg( yval, (std::string("Caught NULL pointer in ")+
				y->fullname()).c_str());
    }
  }

  const int ynum_dims = ytype!=BLAS_VOID ? y->size_of_components() : 0;
  COM_assertion_msg( ytype==BLAS_VOID || (ynum_dims==1 || ynum_dims==num_dims),
		     (std::string("Numbers of components do not match between ")+
		      z->fullname()+" and "+y->fullname()).c_str());
  
  for( zit=zpanes.begin(), zend=zpanes.end(), xit=xpanes.begin(); zit!=zend; 
       ++zit, ++xit, yit+=( ytype!=BLAS_VOID && yit!=NULL)) {
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

    const Attribute *py = (ytype!=BLAS_VOID&&yit)?(*yit)->attribute(y->id()):y;
    int ystrd = get_stride<ytype>(py);
    COM_assertion_msg( ytype!=BLAS_SCNE && ytype!=BLAS_VEC2D ||
		       (length == int(py->size_of_items()) || ystrd==0),
		       (std::string("Numbers of items do not match between ")+
			y->fullname()+" and "+z->fullname()+
			" on pane "+to_str((*zit)->id())).c_str());

    const bool ystg = py && (ynum_dims!=num_dims || ynum_dims!=ystrd);

    // Optimized version for contiguous attributes
    if ( !xstg && !zstg && !ystg && ytype != BLAS_VEC 
	 && (ytype != BLAS_SCNE || num_dims==1)) {
      const data_type *xval = (const data_type *)px->pointer();
      data_type *zval = (data_type *)pz->pointer();

      // Get address for y if y is not window attribute
      if ( ytype != BLAS_VOID && yit)
	yval = reinterpret_cast<const data_type *>(py->pointer());

      //Loop for each element/node and for each dimension
      if ( swap == false)
	for(Size i = 0, s = length*num_dims; i<s; ++i, ++zval, ++xval)
	  *zval = opp( *xval, getref<data_type,ytype,0>(yval,i,0,1));
      else
	for(Size i = 0, s = length*num_dims; i<s; ++i, ++zval, ++xval)
	  *zval = opp( getref<data_type,ytype,0>(yval,i,0,1), *xval);
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
	  
	if ( ytype != BLAS_VOID && yit) {
	  const Attribute *py_i=ynum_dims==1?py:(*yit)->attribute(y->id()+i+1);
	  yval = reinterpret_cast<const data_type *>( py_i->pointer());
	  ystrd = get_stride<ytype>( py_i);
	}

	// Loop for each element/node.
	if ( swap == false) {
	  for(int j = 0; j < length; ++j, zval+=zstrd, xval+=xstrd) 
	    *zval = opp( *xval, getref<data_type,ytype,1>(yval,j,i,ystrd));
	}
	else {
	  for(int j = 0; j < length; ++j, zval+=zstrd, xval+=xstrd) 
	    *zval = opp( getref<data_type,ytype,1>(yval,j,i,ystrd), *xval);
	}
      } // end for i
    }  // end if
  } // end for
}

 
//Chooses which calc function to call based on type of y.
template <class FuncType>
void Rocblas::calcChoose(const Attribute *x, const Attribute *y, Attribute *z,
			 FuncType opp)
{
  typedef typename FuncType::result_type data_type;

  if (x->is_windowed()) {
    if ( x->size_of_components() == 1)
      calc<FuncType,BLAS_SCALAR>(z, y, x, opp, true);
    else
      calc<FuncType,BLAS_VEC>(z, y, x, opp, true);
  }
  else if (y->is_windowed()) {
    if ( y->size_of_components() == 1)
      calc<FuncType,BLAS_SCALAR>(z, x, y, opp, false);
    else
      calc<FuncType,BLAS_VEC>(z, x, y, opp, false);
  }
  else if ( x->size_of_components()<y->size_of_components()) {
    calc<FuncType,BLAS_SCNE>(z, y, x, opp, true);
  }
  else {
    if ( y->size_of_components() == 1)
      calc<FuncType,BLAS_SCNE>(z, x, y, opp, false);
    else
      calc<FuncType,BLAS_VEC2D>(z, x, y, opp, false);
  }
}

//Operation wrapper for addition.
void Rocblas::add(const Attribute *x, const Attribute *y, Attribute *z)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calcChoose(x, y, z, std::plus<int>());
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());

    calcChoose(x, y, z, std::plus<double>());
  }
}

//Operation wrapper for subtraction.
void Rocblas::sub(const Attribute *x, const Attribute *y, Attribute *z)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calcChoose(x, y, z, std::minus<int>());
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());

    calcChoose(x, y, z, std::minus<double>());
  }
}

//Operation wrapper for multiplication.
void Rocblas::mul(const Attribute *x, const Attribute *y, Attribute *z)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calcChoose(x, y, z, std::multiplies<int>());
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());

    calcChoose(x, y, z, std::multiplies<double>());
  }
}

//Operation wrapper for division.
void Rocblas::div(const Attribute *x, const Attribute *y, Attribute *z)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calcChoose(x, y, z, std::divides<int>());
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calcChoose(x, y, z, std::divides<double>());
  }
}

//Operation wrapper for limit1.
void Rocblas::limit1(const Attribute *x, const Attribute *y, Attribute *z)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calcChoose(x, y, z, limit1v<int>());
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calcChoose(x, y, z, limit1v<double>());
  }
}

//Operation wrapper for addition with y as a scalar pointer.
void Rocblas::add_scalar(const Attribute *x, const void *y, Attribute *z,
                        int swap)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calc<std::plus<int>,BLAS_VOID>(z, x, y, std::plus<int>(), swap);
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calc<std::plus<double>,BLAS_VOID>(z, x, y, std::plus<double>(), swap);
  }
}

//Operation wrapper for max of
void Rocblas::maxof_scalar(const Attribute *x, const void *y, Attribute *z,
                        int swap)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calc<maxof<int>,BLAS_VOID>(z, x, y, maxof<int>(), swap);
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calc<maxof<double>,BLAS_VOID>(z, x, y, maxof<double>(), swap);
  }
}

//Operation wrapper for subtraction with y as a scalar pointer.
void Rocblas::sub_scalar(const Attribute *x, const void *y, Attribute *z,
                        int swap)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calc<std::minus<int>,BLAS_VOID>(z, x, y, std::minus<int>(), swap);
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calc<std::minus<double>,BLAS_VOID>(z, x, y, std::minus<double>(), swap);
  }
}

//Operation wrapper for multiplication with y as a scalar pointer.
void Rocblas::mul_scalar(const Attribute *x, const void *y, Attribute *z,
                        int swap)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calc<std::multiplies<int>,BLAS_VOID>(z, x, y, std::multiplies<int>(), swap);
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calc<std::multiplies<double>,BLAS_VOID>(z, x, y, std::multiplies<double>(), swap);
  }
}

//Operation wrapper for division with y as a scalar pointer.
void Rocblas::div_scalar(const Attribute *x, const void *y, Attribute *z,
                        int swap)
{
  COM_Type att_type = z->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calc<std::divides<int>,BLAS_VOID>(z, x, y, std::divides<int>(), swap);
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       z->fullname()).c_str());
    calc<std::divides<double>,BLAS_VOID>(z, x, y, std::divides<double>(), swap);  
  }
}







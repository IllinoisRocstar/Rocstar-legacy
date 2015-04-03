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

// Performs the operation:  z = <x, y>
template <class data_type, int ytype>
void Rocblas::calcDot( void *yout, const Attribute *x, const Attribute *z,
		       const MPI_Comm *comm, const Attribute *mults) {
  
  COM_assertion_msg( !x->is_windowed() && !z->is_windowed(),
		     "Unsupported attribute type");
  COM_assertion_msg( COM_compatible_types(x->data_type(),z->data_type()),
		     (std::string("Incompatible data types between ")+
		      x->fullname()+" and "+z->fullname()).c_str());

  int num_dims = z->size_of_components();
  COM_assertion_msg( num_dims == x->size_of_components(), 
		     (std::string("Numbers of components do not match between ")+
		      x->fullname()+" and "+z->fullname()).c_str());

  std::vector<const Pane*> xpanes, zpanes, mpanes;
  std::vector<Pane*> ypanes;

  z->window()->panes(zpanes);
  x->window()->panes(xpanes);

  COM_assertion_msg( xpanes.size() == zpanes.size(),
		     (std::string("Numbers of panes do not match between ")+
		      x->window()->name()+" and "+z->window()->name()).c_str());

  std::vector<const Pane*>::const_iterator zit, zend, xit;
  Pane **yit=NULL;

  Attribute *y=NULL;
  data_type *yval=NULL;

  if ( ytype == BLAS_VOID) {
    yval = reinterpret_cast<data_type *>( yout);
    *yval = data_type(0);
  }
  else {
    y = reinterpret_cast<Attribute *>(yout);

    if ( !y->is_windowed()) {
      y->window()->panes(ypanes);
      COM_assertion_msg(xpanes.size() == ypanes.size(),
			(std::string("Numbers of panes do not match between ")+
			 x->fullname()+" and "+y->fullname()).c_str());
      yit = &ypanes[0];
    }
    else {
      COM_assertion_msg( y->size_of_items()==1, 
			 (std::string("Numbers of items do not match between ")+
			  x->fullname()+" and "+y->fullname()).c_str());
      yval = reinterpret_cast<data_type *>( y->pointer());

      COM_assertion_msg( yval, (std::string("Caught NULL pointer in ")+
				y->fullname()).c_str());
    }
  }

  const int ynum_dims = ytype != BLAS_VOID?y->size_of_components():0;
  COM_assertion_msg( ytype==BLAS_VOID || (ynum_dims==1 || ynum_dims==num_dims),
		     (std::string("Numbers of components do not match between ")+
		      z->fullname()+" and "+y->fullname()).c_str());

  // Initialize pointer to multiplicities
  const Pane ** mit=NULL;
  const int *mval=NULL;
  if ( mults != NULL) {
    COM_assertion_msg( COM_compatible_types( COM_INT, mults->data_type()) && 
		       mults->size_of_components()==1,
		       (std::string("Multiplier ")+mults->fullname()+
			"must be integer scalars.").c_str());

    mults->window()->panes(mpanes);
    COM_assertion_msg(xpanes.size() == mpanes.size(),
		      (std::string("Numbers of panes do not match between ")+
		       x->window()->name()+" and "+
		       mults->window()->name()).c_str());
    mit = &mpanes[0];
  }

  // Initialize y to 0 for BLAS_VOID or window attribute
  if ( ytype == BLAS_VOID) {
    *yval = data_type(0);
  }
  else {
    if ( y->is_windowed()) {
      for ( int i=0; i<ynum_dims; ++i) yval[i] = data_type(0);
    }
  }
  
  for( zit=zpanes.begin(), zend=zpanes.end(), xit=xpanes.begin(); 
       zit!=zend; ++zit, ++xit, yit+=( ytype!=BLAS_VOID && yit)) {
    const Attribute *pz = (*zit)->attribute(z->id());
    const int  length = pz->size_of_items();
    int  zstrd=get_stride<BLAS_VEC2D>(pz);
    const bool zstg = num_dims!=zstrd;

    const Attribute *px = (*xit)->attribute(x->id());
    int  xstrd = get_stride<BLAS_VEC2D>(px);
    COM_assertion_msg(length == px->size_of_items(),
		      (std::string("Numbers of items do not match between ")+
		       x->fullname()+" and "+z->fullname()+
		       " on pane "+to_str((*zit)->id())).c_str());
    
    const bool xstg = num_dims!=xstrd;

    Attribute *py = y;
    if ( ytype != BLAS_VOID && !y->is_windowed()) {
      // Obtain py and initialize to 0
      py = (*yit)->attribute(y->id()); 
      const int ynum_comp = py->size_of_components();
      const int ylen = py->size_of_items();
      COM_assertion_msg( ylen==1 || ylen==length,
			 (std::string("Numbers of items do not match between ")+
			  y->fullname()+" and "+z->fullname()+
			  " on pane "+to_str((*zit)->id())).c_str());

      yval = reinterpret_cast<data_type *>( py->pointer());
      
      if ( py->stride()==ynum_comp) {
	for ( int i=0, ni=ynum_comp*ylen; i<ni; ++i) yval[i] = data_type(0);
      }
      else {
	// Loop through the number of components
	for (int i=0; i<ynum_comp; ++i) {
	  Attribute *py_i = ynum_comp>1?(*yit)->attribute( y->id()+i+1):py;
	  data_type *yv_i = reinterpret_cast<data_type*>(py_i->pointer());
	  const int strd=get_stride<BLAS_VEC2D>(py_i);
	  // loop through the stride
	  for ( int j=0, nj=ylen*strd; j<nj; j+=strd) yv_i[j] = data_type(0);
	}
      }
    }
    int ystrd = get_stride<ytype>(py);
    const bool ystg = py && (ynum_dims!=num_dims || ynum_dims!=ystrd);

    // Obtain the multiplier
    if ( mults != NULL) {
      const Attribute *pm = (*mit)->attribute(mults->id());
      COM_assertion_msg( pm->size_of_items()==length,
			 (std::string("Numbers of items do not match between ")+
			  mults->fullname()+" and "+z->fullname()+
			  " on pane "+to_str((*zit)->id())).c_str());

      mval = reinterpret_cast<const int*>( pm->pointer());
      ++mit;
    }

    // Optimized version for contiguous attributes
    if ( !xstg && !zstg && !ystg && mval == NULL) {
      const data_type *xval = (const data_type *)px->pointer();
      const data_type *zval = (const data_type *)pz->pointer();

      //Loop for each element/node and for each dimension
      for(Size i = 0, s = length*num_dims; i<s; ++i, ++xval, ++zval)
	getref<data_type,ytype,0>(yval,i,0,1) += *xval* *zval;
    }
    else { // General version
      //Loop for each dimension.
      for(int i = 0; i < num_dims; ++i) {
	const Attribute *pz_i = num_dims==1?pz:(*zit)->attribute(z->id()+i+1);
	const data_type *zval = (const data_type *)pz_i->pointer();
	zstrd=get_stride<BLAS_VEC2D>(pz_i);

	const Attribute *px_i = num_dims==1?px:(*xit)->attribute(x->id()+i+1);
	const data_type *xval = (const data_type *)px_i->pointer();
	xstrd=get_stride<BLAS_VEC2D>(px_i);

	if ( ytype != BLAS_VOID && !y->is_windowed()) {
	  Attribute *py_i=ynum_dims==1?py:(*yit)->attribute(y->id()+i+1);
	  yval = reinterpret_cast<data_type *>( py_i->pointer());
	  ystrd = get_stride<ytype>( py_i);
	}

	if ( mval != NULL) {
	  //Loop for each element/node.
	  for(int j=0; j<length; ++j, xval+=xstrd,zval+=zstrd)
	    getref<data_type,ytype,1>( yval,j,i,ystrd) += *xval* *zval / mval[j];
	}
	else
	  for(int j=0; j<length; ++j, xval+=xstrd,zval+=zstrd)
	    getref<data_type,ytype,1>( yval,j,i,ystrd) += *xval* *zval;
      }
    }
  }

  if ( ( ytype==BLAS_VOID || ytype==BLAS_SCALAR || ytype==BLAS_VEC) 
       && comm && *comm!=MPI_COMM_NULL && COMMPI_Initialized()) {
    int n = (ytype == BLAS_VEC) ? num_dims : 1;
    COM_Type att_type = x->data_type();
    
    data_type t = *yval;
    if ( att_type == COM_INT || att_type == COM_INTEGER) {
      MPI_Allreduce( &t, yval, n, MPI_INT, MPI_SUM, *comm);
    }
    else {
      MPI_Allreduce( &t, yval, n, MPI_DOUBLE, MPI_SUM, *comm);
    }
  }
}

//Wrapper for dot product that produces a single scalar answer.
void Rocblas::dot_MPI(const Attribute *x, const Attribute *y, Attribute *z,
		      const MPI_Comm *comm, const Attribute *mults) {
  COM_Type att_type = x->data_type();
  const int zncomp = z->size_of_components();

  if ( att_type == COM_INT || att_type == COM_INTEGER) {
    if ( z->is_windowed()) {
      if ( zncomp == 1)
	calcDot<int,BLAS_SCALAR>(z, x, y, comm, mults);
      else
	calcDot<int,BLAS_VEC>(z, x, y, comm, mults);
    }
    else {
      if ( zncomp == 1)
	calcDot<int,BLAS_SCNE>(z, x, y, comm, mults);
      else
	calcDot<int,BLAS_VEC2D>(z, x, y, comm, mults);
    }
  }
  else {
    COM_assertion_msg( att_type==COM_DOUBLE || att_type==COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       x->fullname()).c_str());
    if ( z->is_windowed()) {
      if ( zncomp == 1)
	calcDot<double,BLAS_SCALAR>(z, x, y, comm, mults);
      else
	calcDot<double,BLAS_VEC>(z, x, y, comm, mults);
    }
    else {
      if ( zncomp == 1)
	calcDot<double,BLAS_SCNE>(z, x, y, comm, mults);
      else
	calcDot<double,BLAS_VEC2D>(z, x, y, comm, mults);
    }
  }
}

void Rocblas::dot_scalar_MPI( const Attribute *x, const Attribute *y, void *z,
			      const MPI_Comm *comm, const Attribute *mults) {
  COM_Type att_type = x->data_type();

  if(att_type == COM_INT || att_type == COM_INTEGER)
    calcDot<int,BLAS_VOID>(z, x, y, comm, mults);
  else {
    COM_assertion_msg(att_type == COM_DOUBLE||att_type == COM_DOUBLE_PRECISION,
		      (std::string("Unsupported data type in ")+
		       x->fullname()).c_str());
    calcDot<double,BLAS_VOID>(z, x, y, comm, mults);
  }
}

/// Wrapper for 2-norm.
void Rocblas::dot(const Attribute *x, const Attribute *y, Attribute *z,
		  const Attribute*mults) 
{ dot_MPI( x, y, z, NULL, mults); }

void Rocblas::dot_scalar( const Attribute *x, const Attribute *y, void *z,
			  const Attribute *mults) 
{ dot_scalar_MPI( x, y, z, NULL, mults); }

/// Wrapper for 2-norm.
void Rocblas::nrm2(const Attribute *x, Attribute *y, 
		   const Attribute*mults) 
{ dot_MPI( x, x, y, NULL, mults); }

/// Wrapper for 2-norm with y as a scalar pointer.
void Rocblas::nrm2_scalar(const Attribute *x, void *y, 
			  const Attribute*mults) 
{ dot_scalar_MPI ( x, x, y, NULL, mults); }

/// Wrapper for 2-norm with MPI.
void Rocblas::nrm2_MPI(const Attribute *x, Attribute *y, 
		       const MPI_Comm *comm, const Attribute*mults) 
{ dot_MPI ( x, x, y, comm, mults); }

/// Wrapper for 2-norm with y as a scalar pointer with MPI.
void Rocblas::nrm2_scalar_MPI(const Attribute *x, void *y,// 
			      const MPI_Comm *comm, const Attribute *mults) 
{ dot_scalar_MPI ( x, x, y, comm, mults); }








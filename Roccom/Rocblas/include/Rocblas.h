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
// $Id: Rocblas.h,v 1.14 2009/08/07 23:24:11 gzagaris Exp $

/** \file Rocblas.h
 *  Definition for Rocblas API.
 */
/* Author: Greg Mackey and Xiangmin Jiao
 * Creation date:   3/3/2002
 */
#include <cstdio>
#include "roccom.h"
#include "roccom_devel.h"

USE_COM_NAME_SPACE

class Rocblas {
  typedef unsigned int Size;
public:
  /// Creates window for Rocblas and registers functions.
  static
  void init(const std::string &name);

  /// Delete window for Rocblas.
  static
  void finalize(const std::string &name);

  /// Operation wrapper for addition.
  static
  void add(const Attribute *x, const Attribute *y, Attribute *z);

  /// Operation wrapper for subtraction.
  static
  void sub(const Attribute *x, const Attribute *y, Attribute *z);

  /// Operation wrapper for multiplication.
  static
  void mul(const Attribute *x, const Attribute *y, Attribute *z);

  /// Operation wrapper for limit1.
  static
  void limit1(const Attribute *x, const Attribute *y, Attribute *z);

  /// Operation wrapper for division.
  static
  void div(const Attribute *x, const Attribute *y, Attribute *z);

  /// Operation wrapper for addition with y as a scalar pointer.
  static
  void add_scalar(const Attribute *x, const void *y, Attribute *z, int swap = 0);

  /// Operation wrapper for subtraction with y as a scalar pointer.
  static
  void sub_scalar(const Attribute *x, const void *y, Attribute *z, int swap = 0);

  /// Operation wrapper for multiplication with y as a scalar pointer.
  static
  void mul_scalar(const Attribute *x, const void *y, Attribute *z, int swap = 0);

  /// Operation wrapper for division with y as a scalar pointer.
  static
  void div_scalar(const Attribute *x, const void *y, Attribute *z, int swap = 0);

  /// Operation wrapper for addition with y as a scalar pointer.
  static
  void maxof_scalar(const Attribute *x, const void *y, Attribute *z, int swap = 0);

  /// Wrapper for dot product.
  static
  void dot(const Attribute *x, const Attribute *y, Attribute *z,
	   const Attribute *mults=NULL);

  /// Wrapper for 2-norm with z as a scalar pointer.
  static
  void dot_scalar(const Attribute *x, const Attribute *y, void *z,
		  const Attribute *mults=NULL);

  /// Wrapper for dot product.
  static
  void dot_MPI(const Attribute *x, const Attribute *y, Attribute *z,
	       const MPI_Comm *comm=NULL, const Attribute *mults=NULL);

  /// Wrapper for 2-norm with z as a scalar pointer.
  static
  void dot_scalar_MPI(const Attribute *x, const Attribute *y, void *z,
		      const MPI_Comm *comm=NULL, const Attribute *mults=NULL);

  /// Wrapper for 2-norm.
  static
  void nrm2(const Attribute *x, Attribute *y, const Attribute*mults=NULL);

  /// Wrapper for 2-norm with y as a scalar pointer.
  static
  void nrm2_scalar(const Attribute *x, void *y, const Attribute*mults=NULL);

  /// Wrapper for 2-norm with MPI.
  static
  void nrm2_MPI(const Attribute *x, Attribute *y,
		const MPI_Comm *comm=NULL, const Attribute*mults=NULL);

  /// Wrapper for 2-norm with y as a scalar pointer with MPI.
  static
  void nrm2_scalar_MPI(const Attribute *x, void *y,
		       const MPI_Comm *comm, const Attribute *mults=NULL);

  /// Wrapper for swap.
  static
  void swap(Attribute *x, Attribute *y);

  /// Wrapper for copy.
  static
  void copy(const Attribute *x, Attribute *y);

  /// Generate a random number between 0 and $a$ for each entry in z
  static
  void rand(const Attribute *a, Attribute *z);

  /// Operation wrapper for copy  (x is a scalar pointer).
  static
  void copy_scalar(const void *x, Attribute *y);

  /// Generate a random number between 0 and $a$ for each entry in z
  static
  void rand_scalar(const void *a, Attribute *z);

  /// Wrapper for neg (y=-x).
  static
  void neg(const Attribute *x, Attribute *y);

  /// Wrapper for sqrt (y=sqrt(x)).
  static
  void sqrt(const Attribute *x, Attribute *y);

  /// Wrapper for acos (y=acos(x)).
  static
  void acos(const Attribute *x, Attribute *y);

  /// Wrapper for max.
  static
  void max_MPI(const Attribute *x, Attribute *y, const MPI_Comm *comm=NULL);

  /// Operation wrapper for max (y is a scalar pointer).
  static
  void max_scalar_MPI(const Attribute *x, void *y, const MPI_Comm *comm=NULL);

  /// Wrapper for min.
  static
  void min_MPI(const Attribute *x, Attribute *y, const MPI_Comm *comm=NULL);

  /// Operation wrapper for min (y is a scalar pointer).
  static
  void min_scalar_MPI(const Attribute *x, void *y, const MPI_Comm *comm=NULL);

  /// Wrapper for sum.
  static
  void sum_MPI(const Attribute *x, Attribute *y, const MPI_Comm *comm=NULL);

  /// Operation wrapper for sum (y is a scalar pointer).
  static
  void sum_scalar_MPI(const Attribute *x, void *y, const MPI_Comm *comm=NULL);

  /// Operation wrapper for z = a * x + y.
  static
  void axpy(const Attribute *a, const Attribute *x, const Attribute *y,
	    Attribute *z);

  /// Operation wrapper for z = a * x + y (a is a scalar pointer).
  static
  void axpy_scalar(const void *a, const Attribute *x, const Attribute *y,
		   Attribute *z);

protected:
  ///  Performs the operation:  z = x op y
  template <class FuncType, int ytype>
  static
  void calc(Attribute *z, const Attribute *x, const void *yin,
	    FuncType opp, bool swap=false);

  ///  Performs the operation:  z = <x, y>
  template <class data_type, int ztype>
  static
  void calcDot( void *zout, const Attribute *x, const Attribute *y,
		const MPI_Comm *comm=NULL, const Attribute *mults=NULL);

  ///  Performs the operation  opp(x, y)
  template <class FuncType, int ytype>
  static
  void gen2arg( Attribute *z, void *yin, FuncType opp);

  ///  Performs the operation:  z = a*x + y
  template <class data_type, int atype>
  static
  void axpy_gen( const void *a, const Attribute *x, const Attribute *y,
		 Attribute *z);

  /// Chooses which calc function to call based on type of y.
  template <class FuncType>
  static
  void calcChoose(const Attribute *x, const Attribute *y, Attribute *z,
		  FuncType opp);


  template < class Op> static
  void copy_helper( const Attribute *x, Attribute *z);

  //Function object that implements an assignment.
  template <class T_src, class T_trg> struct assn;

  //Function object that implements a random number generator.
  template <class T> struct random;

  //Function object that implements a swap.
  template <class T> struct swapp;

  //Function object that implements a max operation.
  template <class T> struct maxv;

  //Function object that implements a min operation.
  template <class T> struct minv;

  //Function object that implements a sum operation.
  template <class T> struct sumv;

  //Function object that implements a negation.
  template <class T> struct nega;

  //Function object that implements sqrt.
  template <class T> struct sqrta;

  //Function object that implements acos.
  template <class T> struct acosa;

  //Function object that implements a limit1.
  template <class T> struct limit1v;

  //Function object that implements a maxof.
  template <class T> struct maxof;

  enum { BLAS_VOID, BLAS_SCALAR, BLAS_VEC, BLAS_SCNE, BLAS_VEC2D};

  template <int attr_type>
  inline static int
  get_stride( const Attribute *attr);

  template <class data_type, int attr_type, bool is_staggered>
  inline static data_type &
  getref( data_type *base, const int r, const int c, const int nc);

  template <class data_type, int attr_type, bool is_staggered>
  inline static const data_type &
  getref( const data_type *base, const int r, const int c, const int nc);

  template <class OPint, class OPdbl, int OPMPI>
  inline static
  void reduce_MPI(const Attribute *x, Attribute *z,
		  const MPI_Comm *comm, int, double);

  template <class OPint, class OPdbl, int OPMPI>
  inline static
  void reduce_scalar_MPI(const Attribute *x, void *y,
			 const MPI_Comm* comm, int, double);

  static std::string to_str(int i) {
    char buf[10];
    std::sprintf( buf, "%d", i);
    return buf;
  }
};

template <int attr_type>
inline int Rocblas::get_stride( const Attribute *attr) {
  if ( attr_type == BLAS_SCNE || attr_type == BLAS_VEC2D) {
    if ( attr->size_of_items()>1)
      return attr->stride();
    else
      return 0;
  }
  else if ( attr_type == BLAS_VEC)
    return 0;
  else
    return 1;
}

template <class data_type, int attr_type, bool is_staggered>
inline data_type &
Rocblas::getref( data_type *base, const int r, const int c, const int nc) {
  if ( attr_type == BLAS_SCNE || attr_type == BLAS_VEC2D && is_staggered)
    return base[r*nc];
  else if ( attr_type == BLAS_VEC2D)
    return base[r*nc+c];
  else if ( attr_type == BLAS_VEC)
    return base[c];
  else
    return *base;
}

template <class data_type, int attr_type, bool is_staggered>
inline const data_type &
Rocblas::getref( const data_type *base, const int r,
		 const int c, const int nc) {
  if ( attr_type == BLAS_SCNE || attr_type == BLAS_VEC2D && is_staggered)
    return base[r*nc];
  else if ( attr_type == BLAS_VEC2D)
    return base[r*nc+c];
  else if ( attr_type == BLAS_VEC)
    return base[c];
  else
    return *base;
}


/// Calls Rocblas initialization function.
extern "C" void Rocblas_load_module(const char *name);
extern "C" void Rocblas_unload_module(const char *name);



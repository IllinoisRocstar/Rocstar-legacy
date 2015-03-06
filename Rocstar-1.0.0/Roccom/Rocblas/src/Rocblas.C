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
// $Id: Rocblas.C,v 1.23 2008/12/06 08:43:18 mtcampbe Exp $

/** \file Rocblas.C
 *  Implementation of Rocblas.
 */

//  Name:   Rocblas.C
//  Author: Greg Mackey and Xiangmin Jiao
//  Date:   3/31/2004 (Updated for Roccom3)
//

#include "Rocblas.h"

//Creates window for Rocblas and initializes functions.
void Rocblas::init(const std::string &name) {

  const COM_Type arg4_types[]  = { COM_METADATA, COM_METADATA, COM_METADATA, COM_METADATA };
  const COM_Type arg4c_types[]  = { COM_METADATA, COM_METADATA, COM_METADATA, COM_MPI_COMM, COM_METADATA };
  const COM_Type arg4a_types[] = { COM_VOID, COM_METADATA, COM_METADATA, COM_METADATA };
  const COM_Type arg3_types[]  = { COM_METADATA, COM_METADATA, COM_METADATA };
  const COM_Type arg4cso_types[] = { COM_METADATA, COM_VOID, COM_METADATA, COM_INT };
  const COM_Type arg4mmvm_types[] = { COM_METADATA, COM_METADATA, COM_VOID, COM_METADATA };
  const COM_Type arg4mmvcm_types[] = { COM_METADATA, COM_METADATA, COM_VOID, COM_MPI_COMM, COM_METADATA };
  const COM_Type arg2_types[]  = { COM_METADATA, COM_METADATA };
  const COM_Type arg2s_types[] = { COM_VOID, COM_METADATA };

  COM_new_window(name.c_str());
  COM_set_function((name+".add").c_str(), (Func_ptr)add, "iio", arg3_types);
  COM_set_function((name+".sub").c_str(), (Func_ptr)sub, "iio", arg3_types);
  COM_set_function((name+".mul").c_str(), (Func_ptr)mul, "iio", arg3_types);
  COM_set_function((name+".div").c_str(), (Func_ptr)div, "iio", arg3_types);
  COM_set_function((name+".limit1").c_str(), 
		   (Func_ptr)limit1, "iio", arg3_types);

  COM_set_function((name+".add_scalar").c_str(), (Func_ptr)add_scalar, "iioI",
		   arg4cso_types);
  COM_set_function((name+".sub_scalar").c_str(), (Func_ptr)sub_scalar, "iioI",
		   arg4cso_types);
  COM_set_function((name+".mul_scalar").c_str(), (Func_ptr)mul_scalar, "iioI",
		   arg4cso_types);
  COM_set_function((name+".div_scalar").c_str(), (Func_ptr)div_scalar, "iioI",
		   arg4cso_types);

  COM_set_function((name+".maxof_scalar").c_str(), (Func_ptr)maxof_scalar, "iioI", arg4cso_types);

  COM_set_function((name+".neg").c_str(), (Func_ptr)neg, "io", arg2_types);
  COM_set_function((name+".sqrt").c_str(), (Func_ptr)sqrt, "io", arg2_types);
  COM_set_function((name+".acos").c_str(), (Func_ptr)acos, "io", arg2_types);
  COM_set_function((name+".dot").c_str(), (Func_ptr)dot, "iioI", arg4_types);
  COM_set_function((name+".dot_scalar").c_str(), (Func_ptr)dot_scalar, "iioI", arg4mmvm_types);
  COM_set_function((name+".dot_MPI").c_str(), (Func_ptr)dot_MPI, "iioII", arg4c_types);
  COM_set_function((name+".dot_scalar_MPI").c_str(), (Func_ptr)dot_scalar_MPI, "iioII", arg4mmvcm_types);
  COM_set_function((name+".nrm2").c_str(), (Func_ptr)nrm2, "ioI", &arg4_types[1]);
  COM_set_function((name+".nrm2_scalar").c_str(), (Func_ptr)nrm2_scalar, "ioI", &arg4mmvm_types[1]);
  
  COM_set_function((name+".nrm2_MPI").c_str(), (Func_ptr)nrm2_MPI, "ioII", &arg4c_types[1]);
  COM_set_function((name+".nrm2_scalar_MPI").c_str(), (Func_ptr)nrm2_scalar_MPI, "ioII", &arg4mmvcm_types[1]);
  
  COM_set_function((name+".swap").c_str(), (Func_ptr)swap, "bb", arg2_types);
  COM_set_function((name+".copy").c_str(), (Func_ptr)copy, "io", arg2_types);
  COM_set_function((name+".rand").c_str(), (Func_ptr)rand, "io", arg2_types);
  COM_set_function((name+".copy_scalar").c_str(), (Func_ptr)copy_scalar,
		   "io", arg2s_types);
  COM_set_function((name+".rand_scalar").c_str(), (Func_ptr)rand_scalar,
		   "io", arg2s_types);
  COM_set_function((name+".axpy").c_str(), (Func_ptr)axpy, "iiio",
		   arg4_types);
  COM_set_function((name+".axpy_scalar").c_str(), (Func_ptr)axpy_scalar,
		   "iiio", arg4a_types);
  
  COM_Type types[] = {COM_METADATA, COM_METADATA, COM_MPI_COMM};
  COM_set_function((name+".min_MPI").c_str(), 
		   (Func_ptr)min_MPI, "ioI", types);
  COM_set_function((name+".max_MPI").c_str(), 
		   (Func_ptr)max_MPI, "ioI", types);
  COM_set_function((name+".sum_MPI").c_str(),
		   (Func_ptr)sum_MPI, "ioI", types);
  
  types[1] = COM_VOID;
  COM_set_function((name+".min_scalar_MPI").c_str(), 
		   (Func_ptr)min_scalar_MPI, "ioI", types);
  COM_set_function((name+".max_scalar_MPI").c_str(), 
		   (Func_ptr)max_scalar_MPI, "ioI", types);
  COM_set_function((name+".sum_scalar_MPI").c_str(), 
		   (Func_ptr)sum_scalar_MPI, "ioI", types);

  COM_window_init_done(name.c_str());
}

void Rocblas::finalize(const std::string &name) {
  COM_delete_window( name.c_str());
}


//Calls Rocblas initialization function.
extern "C" void Rocblas_load_module(const char *name) {
  Rocblas::init(std::string(name));
}
extern "C" void Rocblas_unload_module(const char *name) {
  Rocblas::finalize(std::string(name));
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS

// All possible Fortran bindings
extern "C" void rocblas_load_module( const char *name, int length)
{ Rocblas::init( std::string(name, length)); }
extern "C" void rocblas_unload_module( const char *name, int length) 
{ Rocblas::finalize( std::string(name, length)); }

extern "C" void rocblas_load_module_( const char *name, int length)
{ Rocblas::init( std::string(name, length)); }
extern "C" void rocblas_unload_module_( const char *name, int length) 
{ Rocblas::finalize( std::string(name, length)); }

extern "C" void ROCBLAS_LOAD_MODULE( const char *name, int length)
{ Rocblas::init( std::string(name, length)); }
extern "C" void ROCBLAS_UNLOAD_MODULE( const char *name, int length) 
{ Rocblas::finalize( std::string(name, length)); }

extern "C" void ROCBLAS_LOAD_MODULE_( const char *name, int length)
{ Rocblas::init( std::string(name, length)); }
extern "C" void ROCBLAS_UNLOAD_MODULE_( const char *name, int length) 
{ Rocblas::finalize( std::string(name, length)); }

#endif // DOXYGEN_SHOULD_SKIP_THIS







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
// $Id: Function.h,v 1.10 2010/03/12 00:38:32 gzagaris Exp $

/** \file Function.h
 * Contains the prototypes and implementation for the Funcion object.
 * @see Window.h
 */
/* Author: Xiangmin Jiao */

#ifndef __ROCCOM_FUNCTION_H__
#define __ROCCOM_FUNCTION_H__

#include <string>
#include <vector>
#include "commpi.h"
#include "roccom_basic.h"
#include "roccom_exception.h"
#include "roccom_assertion.h"

COM_BEGIN_NAME_SPACE

class Attribute;

/** A Function object corresponds to a function member of a window.
 *  It can take up to MAX_NUMARG arguments of void* type (excluding the
 *  implicit arguments) and return no value.
 */
class Function {
public:
  enum { MAX_NUMARG=14};
  enum { F_FUNC, C_FUNC, CPP_MEMBER};

  /** \name Constructors
   * \{
   */
  /// Default constructor.
  Function() : _ptr(NULL), _attr(NULL), _comm(MPI_COMM_NULL),
	       _ftype(C_FUNC) {}
  /** Create a function object with physical address p.
   *  \param p physical address of the function.
   *  \param s the intentions of the arguments.
   *           Its length indicates the number of arguments.
   *           Each entry indicates the intention of its corresponding argument:
   *            'i'/'I' for input-only; 'o'/'O' for output-only;
   *            'b'/'B' for input and output.
   *            Uppercase indicates optional arguments.
   *  \param t the data types of the arguments.
   *           If it is COM_METADATA, then the argument should be a pointer to
   *           an attribute. If it is COM_RAWDATA, then argument should be
   *           the physical address of the values of an attribute. Otherwise,
   *           it is literal type and the argument should be a pointer to
   *           corresponding data type.
   *  \param a attribute with which a member functions is associated.
   *  \param b whether the function is a Fortran subroutine
   */
  Function( Func_ptr p, const std::string &s, const int *t,
	    Attribute *a, bool b=false)
    : _ptr(p), _intents(s), _types( t, t+s.size()), _attr(a),
      _comm(MPI_COMM_NULL), _ftype( b?F_FUNC:C_FUNC) {}
  Function( Member_func_ptr p, const std::string &s, const int *t,
	    Attribute *a)
    : _mem_ptr(p), _intents(s), _types( t, t+s.size()), _attr(a),
      _comm(MPI_COMM_NULL),_ftype(CPP_MEMBER) {}
  //\}

  /** \name Access methods
   * \{
   */
  /// Get physical address of the function.
  Func_ptr pointer() { return _ptr; }
  /// Get the number of arguments.
  int num_of_args() const { return _intents.size(); }
  /// Check whether the ith argument is for input.
  bool is_input( int i) const
  { return _intents[i]!='o' && _intents[i]!='O'; }
  /// Check whether the ith argument is for output.
  bool is_output( int i) const
  { return _intents[i]!='i' && _intents[i]!='I'; }
  /// Check whether the ith argument is literal type.
  bool is_literal( int i) const
  { return _types[i] != COM_RAWDATA &&  _types[i] != COM_F90POINTER &&
      _types[i] != COM_METADATA; }
  /// Check whether the ith argument is optional.
  bool is_optional( int i) const
  { return _intents[i]>='A' &&  _intents[i]<='Z'; }
  /// Check whether the ith argument is raw.
  bool is_rawdata( int i) const
  { return _types[i] == COM_RAWDATA || _types[i] == COM_F90POINTER; }
  /// Check whether the ith argument is meta.
  bool is_metadata( int i) const
  { return _types[i] == COM_METADATA; }

  bool is_fortran() const { return _ftype==F_FUNC; }
  COM_Type data_type( int i) const { return _types[i]; }
  char intent( int i) const { return _intents[i]; }

  void set_communicator( MPI_Comm c) { _comm = c; }
  MPI_Comm communicator() const { return _comm; }

  Attribute *attribute() { return _attr; }
  //\}

  /** \name Invocation
   * \{
   */
#ifndef DOXYGEN_SHOULD_SKIP_THIS
  // invoke with no argument
  void operator()() { _ptr(); }
  // invoke with 1 argument
  void operator()(void *a1) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func1)(void*);
      (*(Func1)(_ptr))(a1);
    }
    else {
      typedef void(COM_Object::*Member_Func1)();
      Member_Func1 f = (Member_Func1)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)();
    }
  }
  // invoke with 2 arguments
  void operator()(void *a1, void *a2) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func2)(void*,void*);
      (*(Func2)(_ptr))(a1,a2);
    }
    else {
      typedef void(COM_Object::*Member_Func2)(void*);
      Member_Func2 f = (Member_Func2)_mem_ptr;
      validate_object(a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2);
    }
  }
  // invoke with 3 arguments
  void operator()( void *a1, void *a2, void *a3) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func3)(void*,void*,void*);
      (*(Func3)(_ptr))(a1,a2,a3);
    }
    else {
      typedef void(COM_Object::*Member_Func3)(void*,void*);
      Member_Func3 f = (Member_Func3)_mem_ptr;
      validate_object(a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3);
    }
  }

  // invoke with 4 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func4)(void*,void*,void*,void*);
      (*(Func4)(_ptr))(a1,a2,a3,a4);
    }
    else {
      typedef void(COM_Object::*Member_Func4)(void*,void*,void*);
      Member_Func4 f = (Member_Func4)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4);
    }
  }

  // invoke with 5 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func5)(void*,void*,void*,void*,void*);
      (*(Func5)(_ptr))(a1,a2,a3,a4,a5);
    }
    else {
      typedef void(COM_Object::*Member_Func5)(void*,void*,void*,void*);
      Member_Func5 f = (Member_Func5)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5);
    }
  }

  // invoke with 6 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5,
		   void *a6) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func6)(void*,void*,void*,void*,void*,void*);
      (*(Func6)(_ptr))(a1,a2,a3,a4,a5,a6);
    }
    else {
      typedef void(COM_Object::*Member_Func6)(void*,void*,void*,void*,void*);
      Member_Func6 f = (Member_Func6)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6);
    }
  }

  // invoke with 7 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5,
		   void *a6, void *a7) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func7)(void*,void*,void*,void*,void*,void*,void*);
      (*(Func7)(_ptr))(a1,a2,a3,a4,a5,a6,a7);
    }
    else {
      typedef void(COM_Object::*Member_Func7)(void*,void*,void*,void*,
					      void*,void*);
      Member_Func7 f = (Member_Func7)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7);
    }
  }

  // invoke with 8 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5,
		   void *a6, void *a7, void *a8) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func8)(void*,void*,void*,void*,void*,void*,void*,void*);
      (*(Func8)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8);
    }
    else {
      typedef void(COM_Object::*Member_Func8)(void*,void*,void*,void*,
					      void*,void*,void*);
      Member_Func8 f = (Member_Func8)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8);
    }
  }

  // invoke with 9 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5,
		   void *a6, void *a7, void *a8, void *a9) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func9)(void*,void*,void*,void*,void*,void*,
			   void*,void*,void*);
      (*(Func9)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8,a9);
    }
    else {
      typedef void(COM_Object::*Member_Func9)(void*,void*,void*,void*,void*,
					      void*,void*,void*);
      Member_Func9 f = (Member_Func9)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8,a9);
    }
  }

  // invoke with 10 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5,
		   void *a6, void *a7, void *a8, void *a9, void *a10) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func10)(void*,void*,void*,void*,void*,void*,
			    void*,void*,void*,void*);
      (*(Func10)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10);
    }
    else {
      typedef void(COM_Object::*Member_Func10)(void*,void*,void*,void*,void*,
					       void*,void*,void*,void*);
      Member_Func10 f = (Member_Func10)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8,a9,a10);
    }
  }

  // invoke with 11 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
		   void *a7, void *a8, void *a9, void *a10, void *a11) {
    if ( _ftype!=CPP_MEMBER) {
      typedef void(*Func11)(void*,void*,void*,void*,void*,void*,
			    void*,void*,void*,void*,void*);
      (*(Func11)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);
    }
    else {
      typedef void(COM_Object::*Member_Func11)(void*,void*,void*,void*,void*,
					       void*,void*,void*,void*,void*);
      Member_Func11 f = (Member_Func11)_mem_ptr;
      validate_object( a1);
      (reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8,a9,a10,a11);
    }
  }

  // invoke with 12 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
  		   void *a7, void *a8, void *a9, void *a10, void *a11, void *a12 )
  {

		if ( _ftype!=CPP_MEMBER) {
			typedef void(*Func11)(void*,void*,void*,void*,void*,void*,
					void*,void*,void*,void*,void*,void*);
			(*(Func11)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
		}
		else {
			typedef void(COM_Object::*Member_Func11)(void*,void*,void*,void*,void*,
								 void*,void*,void*,void*,void*,void*);
			Member_Func11 f = (Member_Func11)_mem_ptr;
			validate_object( a1);
			(reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12);
		}

	}

  // invoke with 13 arguments
  void operator()( void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
    		   void *a7, void *a8, void *a9, void *a10, void *a11, void* a12, void *a13 )
  {

			if ( _ftype!=CPP_MEMBER) {
				typedef void(*Func11)(void*,void*,void*,void*,void*,void*,
						void*,void*,void*,void*,void*,void*,void*);
				(*(Func11)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13);
			}
			else {
				typedef void(COM_Object::*Member_Func11)(void*,void*,void*,void*,void*,
									 void*,void*,void*,void*,void*,void*,void*);
				Member_Func11 f = (Member_Func11)_mem_ptr;
				validate_object( a1);
				(reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13);
			}
	}

  // invoke with 14 arguments
	void operator()( void *a1, void *a2, void *a3, void *a4, void *a5, void *a6,
					 void *a7, void *a8, void *a9, void *a10, void *a11, void* a12, void *a13, void *a14 )
	{

			if ( _ftype!=CPP_MEMBER) {
				typedef void(*Func11)(void*,void*,void*,void*,void*,void*,
						void*,void*,void*,void*,void*,void*,void*,void*);
				(*(Func11)(_ptr))(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14);
			}
			else {
				typedef void(COM_Object::*Member_Func11)(void*,void*,void*,void*,void*,
									 void*,void*,void*,void*,void*,void*,void*,void*);
				Member_Func11 f = (Member_Func11)_mem_ptr;
				validate_object( a1);
				(reinterpret_cast<COM_Object*>(a1)->*f)(a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14);
			}
	}

#endif

  /// invoke with an array of arguments
  void operator()( int n, void **ps) throw (COM_exception)
  {

  	switch ( n)
  	{
			case 0: operator()(); break;
			case 1: operator()( ps[0] ); break;
			case 2: operator()( ps[0],ps[1] ); break;
			case 3: operator()( ps[0],ps[1],ps[2] ); break;
			case 4: operator()( ps[0],ps[1],ps[2],ps[3] ); break;
			case 5: operator()( ps[0],ps[1],ps[2],ps[3],ps[4] ); break;
			case 6: operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5] ); break;
			case 7: operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6] ); break;
			case 8: operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7] ); break;
			case 9: operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7],ps[8] ); break;
			case 10:operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7],ps[8],ps[9] ); break;
			case 11:operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7],ps[8],ps[9],ps[10] ); break;
			case 12:operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7],ps[8],ps[9],ps[10],ps[11] ); break;
			case 13:operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7],ps[8],ps[9],ps[10],ps[11],ps[12] ); break;
			case 14:operator()( ps[0],ps[1],ps[2],ps[3],ps[4],ps[5],ps[6],ps[7],ps[8],ps[9],ps[10],ps[11],ps[12],ps[13] ); break;
			default:  throw COM_exception(COM_ERR_TOO_MANY_ARGS);
    }

  }

  //\}
private:
  void validate_object( void *a1) {
    int ierr = reinterpret_cast<COM_Object*>(a1)->validate_object(a1);
    switch ( ierr) {
    case 1: COM_assertion_msg( ierr==0, "Invalid pointer after casting");
    case 3: COM_assertion_msg( ierr==0, "Invalid pointer and cookie after casting");
    default:;
    }
  }

#ifndef DOXYGEN_SHOULD_SKIP_THIS
  union {
    Func_ptr    _ptr;           ///< Pointer to a regular function.
    Member_func_ptr _mem_ptr;   ///< Pointer to a member function
  };
  std::string _intents;         ///< Intention of the argument. Its length
                                ///< indicates the number of arguments
  std::vector<COM_Type> _types; ///< Data type of the arguments.
  Attribute   *_attr;           ///< Member function
  MPI_Comm    _comm;
  int         _ftype;           ///< Indicate the type of the function
#endif
};

COM_END_NAME_SPACE

#endif



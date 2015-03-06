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
// ======================================================================
//
// Copyright (c) 1999 The CGAL Consortium

// This software and related documentation is part of the Computational
// Geometry Algorithms Library (CGAL).
// This software and documentation is provided "as-is" and without warranty
// of any kind. In no event shall the CGAL Consortium be liable for any
// damage of any kind. 
//
// Every use of CGAL requires a license. 
//
// Academic research and teaching license
// - For academic research and teaching purposes, permission to use and copy
//   the software and its documentation is hereby granted free of charge,
//   provided that it is not a component of a commercial product, and this
//   notice appears in all copies of the software and related documentation. 
//
// Commercial licenses
// - A commercial license is available through Algorithmic Solutions, who also
//   markets LEDA (http://www.algorithmic-solutions.de). 
// - Commercial users may apply for an evaluation license by writing to
//   Algorithmic Solutions (contact@algorithmic-solutions.com). 
//
// The CGAL Consortium consists of Utrecht University (The Netherlands),
// ETH Zurich (Switzerland), Free University of Berlin (Germany),
// INRIA Sophia-Antipolis (France), Martin-Luther-University Halle-Wittenberg
// (Germany), Max-Planck-Institute Saarbrucken (Germany), RISC Linz (Austria),
// and Tel-Aviv University (Israel).
//
// ----------------------------------------------------------------------
// 
// release       : CGAL-2.2
// release_date  : 2000, September 30
// 
// source        : basic.fw
// file          : include/CGAL/basic.h
// package       : Kernel_basic (3.14)
// revision      : 3.14
// revision_date : 15 Sep 2000 
// author(s)     : Lutz Kettner
//                 Stefan Schirra
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_BASIC_H
#define CGAL_BASIC_H

#ifndef CGAL_CONFIG_H
#  include <CGAL/config.h>
#endif // CGAL_CONFIG_H

#define CGAL_NTS CGAL::NTS::

#if ((__GNUC__ == 2) && (__GNUC_MINOR__ == 95))
#include <cmath>
#endif  // gcc 2.95

#include <iostream>
#include <cstdlib>


// Big endian or little endian machine.
// ====================================
#ifdef CGAL_CFG_NO_BIG_ENDIAN
#define CGAL_LITTLE_ENDIAN 1
#else
#define CGAL_BIG_ENDIAN 1
#endif


#ifdef CGAL_USE_LEDA
#  ifndef CGAL_PROTECT_LEDA_BASIC_H
#    if ( __LEDA__ < 380 )
#      define Max leda_Max
#      define Min leda_Min
#    endif // __LEDA__ < 380
#    include <LEDA/basic.h>
#    if ( __LEDA__ < 380 )
#      undef Max
#      undef Min
#    endif // __LEDA__ < 380
#    define CGAL_PROTECT_LEDA_BASIC_H
#  endif // CGAL_PROTECT_LEDA_BASIC_H
#endif  // CGAL_USE_LEDA

// CGAL uses std::min and std::max
// (see ISO C++ 25.3.7, page 562),
// if feasible
#include <algorithm>

namespace CGAL {

#if !defined(CGAL_CFG_USING_USING_BUG) && !defined(CGAL_CFG_BROKEN_USING)

 using std::min;
 using std::max;

#else

 template <class NT>
 inline
 NT
 // const NT&
 min(const NT& x, const NT& y)
 { return (y < x) ? y : x; }

 template <class NT>
 inline
 NT
 // const NT&
 max(const NT& x, const NT& y)
 { return (x < y) ? y : x; }

#endif // CGAL_CFG_BROKEN_USING
} // namespace CGAL


#ifndef CGAL_ASSERTIONS_H
#include <CGAL/assertions.h>
#endif // CGAL_ASSERTIONS_H
#ifndef CGAL_KERNEL_ASSERTIONS_H
#include <CGAL/kernel_assertions.h>
#endif // CGAL_KERNEL_ASSERTIONS_H
#ifndef CGAL_HANDLE_H
#include <CGAL/Handle.h>
#endif // CGAL_HANDLE_H
#ifndef CGAL_OBJECT_H
#include <CGAL/Object.h>
#endif // CGAL_OBJECT_H
#ifndef CGAL_ENUM_H
#include <CGAL/enum.h>
#endif // CGAL_ENUM_H
#ifndef CGAL_TAGS_H
#include <CGAL/tags.h>
#endif // CGAL_TAGS_H
#ifndef CGAL_MISC_H
#include <CGAL/misc.h>
#endif // CGAL_MISC_H
#ifndef CGAL_NUMBER_TYPE_BASIC_H
#include <CGAL/number_type_basic.h>
#endif // CGAL_NUMBER_TYPE_BASIC_H
#include <CGAL/number_utils.h>
#ifndef CGAL_IO_IO_H
#include <CGAL/IO/io.h>
#endif // CGAL_IO_IO_H
#ifndef CGAL_KERNEL_BASIC_H
#include <CGAL/kernel_basic.h>
#endif // CGAL_KERNEL_BASIC_H

// #ifndef CGAL_KNOWN_BIT_SIZE_INTEGERS_H
// #include <CGAL/known_bit_size_integers.h>
// #endif // CGAL_KNOWN_BIT_SIZE_INTEGERS_H

CGAL_BEGIN_NAMESPACE


// Two struct's to denote boolean compile time decisions.
// ======================================================
struct Tag_true  {};
struct Tag_false {};

inline bool check_tag( Tag_true)  {return true;}
inline bool check_tag( Tag_false) {return false;}

// A function that asserts a specific compile time tag
// forcing its two arguments to have equal type.
// It is encapsulated with #ifdef since it will be defined also elsewhere.
// ======================================================
#ifndef CGAL_ASSERT_COMPILE_TIME_TAG
#define CGAL_ASSERT_COMPILE_TIME_TAG 1
template <class Base>
struct Assert_tag_class
{
    void match_compile_time_tag( const Base&) const {}
};

template <class Tag, class Derived>
inline
void
Assert_compile_time_tag( const Tag&, const Derived& b)
{
  Assert_tag_class<Tag> x;
  x.match_compile_time_tag(b);
}
#endif // CGAL_ASSERT_COMPILE_TIME_TAG

template < class T>
inline
void
assert_equal_types( const T&, const T&) {}



// Symbolic constants to tailor inlining. Inlining Policy.
// =======================================================
#ifndef CGAL_MEDIUM_INLINE
#define CGAL_MEDIUM_INLINE inline
#endif
#ifndef CGAL_LARGE_INLINE
#define CGAL_LARGE_INLINE
#endif
#ifndef CGAL_HUGE_INLINE
#define CGAL_HUGE_INLINE
#endif

CGAL_END_NAMESPACE



#endif // CGAL_BASIC_H







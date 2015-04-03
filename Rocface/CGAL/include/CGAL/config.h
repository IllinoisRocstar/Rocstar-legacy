// ======================================================================
//
// Copyright (c) 1997 The CGAL Consortium

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
// file          : include/CGAL/config.h
// package       : Configuration (2.4)
// source        :
// revision      : 1.11
// revision_date : 30 Mar 1998
// author(s)     : Wieger Wesselink
//                 Michael Hoffmann
//
// coordinator   : Utrecht University
//
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_CONFIG_H
#define CGAL_CONFIG_H

#define CGAL_VERSION 2.2
#define CGAL_VERSION_NR 1002002100

#define CGAL_CFG_NO_ADVANCED_KERNEL 1

//----------------------------------------------------------------------//
//             STLport fix for MSVC
//----------------------------------------------------------------------//


#ifdef _MSC_VER
#   define CGAL_SCOPE
#   define CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT 1
#   include <stl_config.h>
#   include <stl_iterator_base.h>
#else  // not _MSC_VER
#   define CGAL_SCOPE CGAL::
#   define CGAL_DEFINE_ITERATOR_TRAITS_POINTER_SPEC(a)
#endif // _MSC_VER


//----------------------------------------------------------------------//
//             include platform specific workaround flags (CGAL_CFG_...)
//----------------------------------------------------------------------//

#include <CGAL/compiler_config.h>

//----------------------------------------------------------------------//
//             do some post processing for the flags
//----------------------------------------------------------------------//


#ifdef CGAL_CFG_TYPENAME_BUG
#   define CGAL_TYPENAME_MSVC_NULL
#else
#   define CGAL_TYPENAME_MSVC_NULL typename
#endif


#ifdef CGAL_CFG_NO_NAMESPACE
#  define CGAL_USING_NAMESPACE_STD
#  define CGAL_STD
#  define CGAL std
#else
#  define CGAL_USING_NAMESPACE_STD using namespace std;
#  define CGAL_STD std
#  ifndef CGAL_USE_NAMESPACE
#    define CGAL_USE_NAMESPACE 1
#  endif
#endif

#if CGAL_USE_NAMESPACE
#  define CGAL_BEGIN_NAMESPACE namespace CGAL {
#  define CGAL_END_NAMESPACE }
#else
#  define CGAL_BEGIN_NAMESPACE
#  define CGAL_END_NAMESPACE
#endif

#ifdef CGAL_CFG_NO_MUTABLE
#  define mutable
#endif

#ifdef CGAL_CFG_NO_TEMPLATE_FRIEND_DISTINCTION
#  define CGAL_NULL_TMPL_ARGS
#else
#  define CGAL_NULL_TMPL_ARGS <>
#endif

#ifdef CGAL_CFG_NO_EXPLICIT_CLASS_TEMPLATE_SPECIALISATION
#  define CGAL_TEMPLATE_NULL
#else
#  define CGAL_TEMPLATE_NULL template <>
#endif


#ifdef CGAL_CFG_NO_STDC_NAMESPACE
#define CGAL_CLIB_STD
#else
#define CGAL_CLIB_STD std
#endif

//----------------------------------------------------------------------//
//             include separate workaround files
//----------------------------------------------------------------------//

#ifdef _MSC_VER
#  include <CGAL/MSVC_standard_header_fixes.h>
#endif
#if defined(__BORLANDC__) && __BORLANDC__ > 0x520
#include <CGAL/Borland_fixes.h>
#endif
#include <CGAL/workaround_casts.h>

//----------------------------------------------------------------------//
//             select old or new style headers
//----------------------------------------------------------------------//


#ifndef CGAL_USE_NEWSTYLE_HEADERS
#  ifndef CGAL_CFG_NO_STANDARD_HEADERS
#    ifndef CGAL_NO_NEWSTYLE_HEADERS
#      define CGAL_USE_NEWSTYLE_HEADERS
#    endif // ! CGAL_NO_NEWSTYLE_HEADERS
#  endif // ! CGAL_CFG_NO_STANDARD_HEADERS
#endif // ! CGAL_USE_NEWSTYLE_HEADERS

#endif // CGAL_CONFIG_H


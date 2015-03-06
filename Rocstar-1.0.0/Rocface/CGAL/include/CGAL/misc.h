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
// source        : misc.fw
// file          : include/CGAL/misc.h
// package       : Kernel_basic (3.14)
// revision      : 3.14
// revision_date : 15 Sep 2000 
// author(s)     : Andreas Fabri
//                 Stefan Schirra
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_MISC_H
#define CGAL_MISC_H

#include <algorithm>

CGAL_BEGIN_NAMESPACE


/*
template < class T >
inline
void
swap(T& a, T& b)
{
  T c;
  c = a;
  a = b;
  b = c;
}
*/

#ifndef CGAL_CFG_NO_EXPLICIT_TEMPLATE_FUNCTION_ARGUMENT_SPECIFICATION
// A helper class:
// ---------------------
template <class Target, class Source>
struct converter
{
    static inline Target do_it(const Source& s)
    { return static_cast<Target>(s); }
};

template <class Target, class Source>
inline
Target
convert_to (const Source& s)
{ return converter<Target, Source>::do_it(s); }

/*
template <class Target, class Source>
inline
Target
convert_to( const Source& s)
{ return Target(s); }
*/
#endif // CGAL_CFG_NO_EXPLICIT_TEMPLATE_FUNCTION_ARGUMENT_SPECIFICATION

template <class Target, class Source>
inline
Target
convert_from_to( const Target& t, const Source& s)
{ return Target(s); }

CGAL_END_NAMESPACE

#endif // CGAL_MISC_H

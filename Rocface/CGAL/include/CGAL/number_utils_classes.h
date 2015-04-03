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
// file          : include/CGAL/number_utils_classes.h
// package       : Number_types (3.4)
// source        : 
// revision      : 2.0.5
// revision_date : 14 Mar 99
// author(s)     : Michael Hoffmann
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
//           
//                 slight modification of Michael's 1.1 version
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

// to be included by number_utils.h

#ifndef CGAL_NUMBER_UTILS_CLASSES_H
#define CGAL_NUMBER_UTILS_CLASSES_H 1

#ifndef CGAL_CONFIG_H
#include <CGAL/config.h>
#endif // CGAL_CONFIG_H

#include <algorithm>
#include <functional>

CGAL_BEGIN_NAMESPACE

template < class NT >
struct Is_zero :public CGAL_STD::unary_function< NT, bool > {
  bool operator()( const NT& x) const
  { return is_zero( x); }
};

template < class NT >
struct Is_one :public CGAL_STD::unary_function< NT, bool > {
  bool operator()( const NT& x) const
  { return is_one( x); }
};

template < class NT >
struct Is_negative :public CGAL_STD::unary_function< NT, bool > {
  bool operator()( const NT& x) const
  { return is_negative( x); }
};

template < class NT >
struct Is_positive :public CGAL_STD::unary_function< NT, bool > {
  bool operator()( const NT& x) const
  { return is_positive( x); }
};

// Sign would result in a name clash with enum.h
template < class NT >
struct Sgn :public CGAL_STD::unary_function< NT, int > {
  Sign operator()( const NT& x) const
  { return sign( x); }
};

template < class NT >
struct Lexicographical_sign
  :public CGAL_STD::binary_function< NT, NT, int > {

  Sign operator()( const NT& x, const NT& y) const
  { return lexicographical_sign( x, y); }
};

template < class NT >
struct Abs :public CGAL_STD::unary_function< NT, NT > {
  NT operator()( const NT& x) const
  { return abs( x); }
};

template < class NT >
struct Min :public CGAL_STD::binary_function< NT, NT, NT > {
  NT operator()( const NT& x, const NT& y) const
  { return std::min( x, y); }
};

template < class NT >
struct Max :public CGAL_STD::binary_function< NT, NT, NT > {
  NT operator()( const NT& x, const NT& y) const
  { return std::max( x, y); }
};

template < class NT >
struct Compare
  :public CGAL_STD::binary_function< NT, NT, Comparison_result > {

  Comparison_result
  operator()( const NT& x, const NT& y) const
  { return compare( x, y); }
};

template < class NT >
struct Square :public CGAL_STD::unary_function< NT, NT > {

  NT
  operator()( const NT& x) const
  { return square( x ); }
};

CGAL_END_NAMESPACE

#endif // CGAL_NUMBER_UTILS_CLASSES_H

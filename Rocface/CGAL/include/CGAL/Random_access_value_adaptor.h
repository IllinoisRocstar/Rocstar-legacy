// ======================================================================
//
// Copyright (c) 1997, 1998, 1999, 2000 The CGAL Consortium

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
// file          : include/CGAL/Random_access_value_adaptor.h
// package       : STL_Extension (2.21)
// chapter       : $CGAL_Chapter: STL Extensions for CGAL $
// source        : stl_extension.fw
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:48 $
// author(s)     : Michael Hoffmann
//                 Lutz Kettner
//
//
// Random Access Value Adaptor provides random access for sequences.
// coordinator   : ?
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_RANDOM_ACCESS_VALUE_ADAPTOR_H
#define CGAL_RANDOM_ACCESS_VALUE_ADAPTOR_H 1
#include <CGAL/Random_access_adaptor.h>

CGAL_BEGIN_NAMESPACE

template < class IC, class T >
class Random_access_value_adaptor : public Random_access_adaptor<IC> {
public:
  typedef typename Random_access_adaptor<IC>::size_type size_type;

  Random_access_value_adaptor() {}
  // invalid index.

  Random_access_value_adaptor( const IC& i)
  : Random_access_adaptor<IC>(i) {}
  // empty random access index initialized to start at i.

  Random_access_value_adaptor( const IC& i, const IC& j)
  : Random_access_adaptor<IC>(i,j) {}
  // random access index initialized with range [i,j).

  // OPERATIONS

  T& operator[]( size_type n) const {
    // returns inverse index of k.
    return *(Random_access_adaptor<IC>::operator[](n));
  }
};

CGAL_END_NAMESPACE
#endif // CGAL_RANDOM_ACCESS_VALUE_ADAPTOR_H //
// EOF //

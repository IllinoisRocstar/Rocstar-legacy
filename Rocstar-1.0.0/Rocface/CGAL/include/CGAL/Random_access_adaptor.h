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
// file          : include/CGAL/Random_access_adaptor.h
// package       : STL_Extension (2.21)
// chapter       : $CGAL_Chapter: STL Extensions for CGAL $
// source        : stl_extension.fw
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:48 $
// author(s)     : Michael Hoffmann
//                 Lutz Kettner
//
//
// Random Access Adaptor provides random access for sequences.
// coordinator   : ?
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_RANDOM_ACCESS_ADAPTOR_H
#define CGAL_RANDOM_ACCESS_ADAPTOR_H 1
#include <vector>
#include <CGAL/circulator.h>

CGAL_BEGIN_NAMESPACE

template < class IC>
class Random_access_adaptor {

  // DEFINITION
  //
  // The class Random_access_adaptor<IC> provides a random access
  // for data structures. Either the data structure supports random access
  // iterators or circulators where this class maps function calls to the
  // iterator or circulator, or a STL `vector' is used to provide the random
  // access. The iterator or circulator of the data structure are of type
  // `IC'.
  //
  // CREATION

protected:
  typedef std::vector< IC> Index;
  Index   index;
  IC      start;

public:
  typedef typename Index::size_type  size_type;

  void init_index( IC i, const IC& j, std::forward_iterator_tag);
  void init_index( const IC& i, const IC& j,
                   std::bidirectional_iterator_tag){
    init_index( i, j, std::forward_iterator_tag());
  }
  void init_index( const IC& i, const IC&,
                   std::random_access_iterator_tag){
    start = i;
  }
  void init_index( const IC& i, const IC& j) {
#if !defined(CGAL_CFG_NO_ITERATOR_TRAITS) || \
    defined(CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT)
    typedef typename std::iterator_traits<IC>::iterator_category ICC;
    init_index( i, j, ICC());
#else
    init_index( i, j, std::iterator_category( i));
#endif
  }


  void reserve( size_type r, std::forward_iterator_tag) {
    index.reserve( r);
  }
  void reserve( size_type r, std::bidirectional_iterator_tag){
    reserve( r, std::forward_iterator_tag());
  }
  void reserve( size_type, std::random_access_iterator_tag){}


  void push_back( const IC& k, std::forward_iterator_tag) {
    index.push_back(k);
  }
  void push_back( const IC& k, std::bidirectional_iterator_tag){
    push_back( k, std::forward_iterator_tag());
  }
  void push_back( const IC&, std::random_access_iterator_tag){}


  const IC& find( size_type n, std::forward_iterator_tag) const {
    // returns inverse index of k.
    CGAL_assertion( n < index.size());
    return index[n];
  }
  const IC& find( size_type n, std::bidirectional_iterator_tag) const {
    return find( n, std::forward_iterator_tag());
  }
  IC  find( size_type n, std::random_access_iterator_tag) const {
    return start + n;
  }

  typedef IC   iterator;
  typedef IC   Circulator;

  Random_access_adaptor() : start(IC()) {}
  // invalid index.

  Random_access_adaptor( const IC& i) : start(i) {}
  // empty random access index initialized to start at i.

  Random_access_adaptor( const IC& i, const IC& j) : start(i) {
    // random access index initialized with range [i,j).
    init_index( i, j);
  }

  void reserve( size_type r) {
    // reserve r entries, if a `vector' is used internally.
#if !defined(CGAL_CFG_NO_ITERATOR_TRAITS) || \
    defined(CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT)
    typedef typename std::iterator_traits<IC>::iterator_category ICC;
    reserve( r, ICC());
#else
    reserve( r, std::iterator_category( IC()));
#endif
  }

  // OPERATIONS

  IC  find( size_type n) const {
    // returns inverse index of k.
#if !defined(CGAL_CFG_NO_ITERATOR_TRAITS) || \
    defined(CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT)
    typedef typename std::iterator_traits<IC>::iterator_category ICC;
    return find( n, ICC());
#else
    return find( n, std::iterator_category( IC()));
#endif
  }

  IC  operator[]( size_type n) const { return find(n); }

  void push_back( const IC& k) {
    // adds k at the end of the indices.
#if !defined(CGAL_CFG_NO_ITERATOR_TRAITS) || \
    defined(CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT)
    typedef typename std::iterator_traits<IC>::iterator_category ICC;
    push_back( k, ICC());
#else
    push_back( k, std::iterator_category( k));
#endif
  }
};

template < class IC>
void
Random_access_adaptor< IC>::init_index( IC i, const IC& j,
                                        std::forward_iterator_tag) {
  if ( ! is_empty_range( i, j)) {
    do {
      index.push_back( i);
    } while ((++i) != (j));
  }
}

CGAL_END_NAMESPACE
#endif // CGAL_RANDOM_ACCESS_ADAPTOR_H //
// EOF //

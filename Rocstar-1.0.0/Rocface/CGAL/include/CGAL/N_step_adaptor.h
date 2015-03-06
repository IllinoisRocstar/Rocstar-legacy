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
// file          : include/CGAL/N_step_adaptor.h
// package       : STL_Extension (2.21)
// chapter       : $CGAL_Chapter: STL Extensions for CGAL $
// source        : stl_extension.fw
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:47 $
// author(s)     : Michael Hoffmann
//                 Lutz Kettner
//
//
// An iterator/circulator adaptor doing n-steps per increment.
// coordinator   : ?
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_N_STEP_ADAPTOR_H
#define CGAL_N_STEP_ADAPTOR_H 1
#include <CGAL/circulator.h>

CGAL_BEGIN_NAMESPACE

#if defined(CGAL_CFG_NO_ITERATOR_TRAITS) && \
!defined(CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT)
template < class I, int N, class Ref, class Ptr,
           class Val, class Dist, class Ctg>
#else
template < class I,
           int N,
           class Ref  = typename std::iterator_traits<I>::reference,
           class Ptr  = typename std::iterator_traits<I>::pointer,
           class Val  = typename std::iterator_traits<I>::value_type,
           class Dist = typename std::iterator_traits<I>::difference_type,
           class Ctg = typename std::iterator_traits<I>::iterator_category>
#endif
class N_step_adaptor {
protected:
  I        nt;    // The internal iterator.
public:
  typedef I                                        Iterator;
  typedef N_step_adaptor<I,N,Ref,Ptr,Val,Dist,Ctg> Self;
  typedef Ctg                                      iterator_category;
  typedef Val                                      value_type;
  typedef Dist                                     difference_type;
#ifdef CGAL_CFG_NO_ITERATOR_TRAITS
  typedef Ref                                      reference;
  typedef Ptr                                      pointer;
#else
  typedef typename std::iterator_traits<I>::reference reference;
  typedef typename std::iterator_traits<I>::pointer   pointer;
#endif
  // Special for circulators.
  typedef I_Circulator_size_traits<iterator_category,I> C_S_Traits;
  typedef typename  C_S_Traits::size_type               size_type;

  // CREATION
  // --------

  N_step_adaptor() {}
  N_step_adaptor( Iterator j) : nt(j) {}

  template <class II>
  N_step_adaptor( const N_step_adaptor<II,N>& j)
    : nt( j.current_iterator()) {}

  // OPERATIONS Forward Category
  // ---------------------------

  // Circulator stuff.
  typedef  I  Circulator;
  Circulator  current_circulator() const { return nt;}

  Iterator  current_iterator() const { return nt;}
  bool operator==( CGAL_NULL_TYPE p) const {
    CGAL_assertion( p == 0);
    return ( nt == 0);
  }
  bool  operator!=( CGAL_NULL_TYPE p) const { return !(*this == p); }
  bool  operator==( const Self& i) const { return ( nt == i.nt); }
  bool  operator!=( const Self& i) const { return !(*this == i); }
  Ref   operator*()  const { return *nt; }
  Ptr   operator->() const { return nt.operator->(); }
  Self& operator++() {
    std::advance( nt, N);
    return *this;
  }
  Self  operator++(int) {
    Self tmp = *this;
    ++*this;
    return tmp;
  }

  // OPERATIONS Bidirectional Category
  // ---------------------------------

  Self& operator--() {
    std::advance( nt, -N);
    return *this;
  }
  Self  operator--(int) {
    Self tmp = *this;
    --*this;
    return tmp;
  }

  // OPERATIONS Random Access Category
  // ---------------------------------

  Self  min_circulator() const { return Self( nt.min_circulator()); }
  Self& operator+=( difference_type n) {
    nt += difference_type(N * n);
    return *this;
  }
  Self  operator+( difference_type n) const {
    Self tmp = *this;
    tmp.nt += difference_type(N * n);
    return tmp;
  }
#ifdef CGAL_CFG_NO_CONSTANTS_IN_FUNCTION_TEMPLATES
  friend inline
  Self
  operator+( difference_type n, Self i) {
    i = i + n;
    return i;
  }
#endif // CGAL_CFG_NO_CONSTANTS_IN_FUNCTION_TEMPLATES //
  Self& operator-=( difference_type n) {
    return operator+=( -n);
  }
  Self  operator-( difference_type n) const {
    Self tmp = *this;
    return tmp += -n;
  }
  difference_type  operator-( const Self& i) const { return (nt-i.nt)/N;}
  Ref  operator[]( difference_type n) const {
    Self tmp = *this;
    tmp += n;
    return tmp.operator*();
  }
  bool operator<( const Self& i) const { return ( nt < i.nt); }
  bool operator>( const Self& i) const { return i < *this; }
  bool operator<=( const Self& i) const { return !(i < *this); }
  bool operator>=( const Self& i) const { return !(*this < i); }
#ifdef CGAL_CFG_NO_ITERATOR_TRAITS
#ifndef CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT
  friend inline  iterator_category
  iterator_category( const Self&) { return iterator_category(); }
  friend inline  value_type*
  value_type( const Self&) { return (value_type*)(0); }
  friend inline  difference_type*
  distance_type( const Self&) { return (difference_type*)(0); }
  typedef _Circulator_traits<iterator_category> C_Traits;
  typedef typename  C_Traits::category  category;
  friend inline  category
  query_circulator_or_iterator( const Self&) { return category(); }
#endif // CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT
#endif // CGAL_CFG_NO_ITERATOR_TRAITS //
};
#ifndef CGAL_CFG_NO_CONSTANTS_IN_FUNCTION_TEMPLATES
template < class I, int N, class Ref, class Ptr,
           class Val, class Dist, class Ctg>
inline
N_step_adaptor<I,N,Ref,Ptr,Val,Dist,Ctg>
operator+( Dist n, N_step_adaptor<I,N,Ref,Ptr,Val,Dist,Ctg> i)
{ return i += n; }
#endif // CGAL_CFG_NO_CONSTANTS_IN_FUNCTION_TEMPLATES //

CGAL_END_NAMESPACE
#endif // CGAL_N_STEP_ADAPTOR_H //
// EOF //

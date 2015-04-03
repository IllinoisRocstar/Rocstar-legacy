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
// source        : Vector_2.fw
// file          : include/CGAL/Vector_2.h
// package       : _2 (3.6)
// revision      : 3.6
// revision_date : 30 Jul 2000 
// author(s)     : Andreas Fabri
//                 Stefan Schirra
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_VECTOR_2_H
#define CGAL_VECTOR_2_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifndef CGAL_POINT_2_H
#include <CGAL/Point_2.h>
#endif // CGAL_POINT_2_H


#ifndef CGAL_DIRECTION_2_H
#include <CGAL/Direction_2.h>
#endif // CGAL_DIRECTION_2_H



#ifdef VECTOR_WRAPPER
#ifndef VECTOR_2_RFT_WRAPPER_H
#include <CGAL/Vector_2_rft_wrapper.h>
#endif // VECTOR_2_RFT_WRAPPER_H
#endif // VECTOR_WRAPPER

CGAL_BEGIN_NAMESPACE

template <class T> class Quotient;
template <class R_>
class Vector_2 : public R_::Vector_2_base
{
public:
  typedef  R_                        R;
  typedef typename R::RT             RT;
  typedef typename R::FT             FT;
  typedef typename R::Vector_2_base  RVector_2;

friend CGAL_FRIEND_INLINE
       CGAL::Vector_2<R>
       CGAL_SCOPE point_to_vector_conversion CGAL_NULL_TMPL_ARGS
                                        (const CGAL::Point_2<R> &p);

  Vector_2() {}

  Vector_2(const CGAL::Vector_2<R> &v) : RVector_2((const RVector_2&)v) {}

  Vector_2(const RVector_2& v) : RVector_2(v) {}

  Vector_2(const Null_vector &v) : RVector_2(v) {}

  Vector_2(const RT &x, const RT &y) : RVector_2(x,y) {}

  Vector_2(const RT &x, const RT &y, const RT &w) : RVector_2(x,y,w) {}


  bool
  operator==(const CGAL::Vector_2<R> &v) const
  { return RVector_2::operator==(v); }

  bool
  operator!=(const CGAL::Vector_2<R> &v) const
  { return !(*this == v); }

  bool
  operator==(const Null_vector &v) const
  { return RVector_2::operator==(v); }

  bool
  operator!=(const Null_vector &v) const
  { return !(*this == v); }

  RT
  hx() const
  { return RVector_2::hx(); }

  RT
  hy() const
  { return RVector_2::hy(); }

  RT
  hw() const
  { return RVector_2::hw(); }

  FT x() const
  { return RVector_2::x(); }

  FT y() const
  { return RVector_2::y(); }

  RT homogeneous(int i) const
  { return RVector_2::homogeneous(i); }

  FT
  cartesian(int i) const
  { return RVector_2::cartesian(i); }

  FT
  operator[](int i) const
  { return cartesian(i); }

  int
  dimension() const
  { return 2; }

  CGAL::Vector_2<R>
  operator+(const CGAL::Vector_2<R> &w) const
  { return (const RVector_2&)(*this) + (const RVector_2&)(w); }

  CGAL::Vector_2<R>
  operator-(const CGAL::Vector_2<R> &w) const
  { return (const RVector_2&)(*this) - (const RVector_2&)(w); }

  CGAL::Vector_2<R>
  operator-() const
  { return RVector_2::operator-(); }

  FT
  operator*(const CGAL::Vector_2<R> &w) const
  { return (const RVector_2&)(*this) * (const RVector_2&)(w); }

#ifndef VECTOR_WRAPPER
  CGAL::Vector_2<R>
  operator*(const RT &c) const
  { return c * (const RVector_2&)(*this); }

  CGAL::Vector_2<R>
  operator*(const Quotient<RT> &q) const
  { return (q.numerator() * (const RVector_2&)(*this)) / q.denominator(); }

  CGAL::Vector_2<R>
  operator/(const Quotient<RT> &q) const
  { return (q.denominator() * (const RVector_2&)(*this)) / q.numerator(); }
#endif // VECTOR_WRAPPER

  CGAL::Vector_2<R>
  operator/(const RT &c) const
  { return (const RVector_2&)(*this) / c; }

  CGAL::Direction_2<R>
  direction() const
  { return RVector_2::direction(); }

  CGAL::Vector_2<R>
  perpendicular(const Orientation &o) const
  { return RVector_2::perpendicular(o); }

  CGAL::Vector_2<R>
  transform(const CGAL::Aff_transformation_2<R> &t) const
  { return RVector_2::transform(t); }

private:
  Vector_2(const CGAL::Point_2<R> &p) : RVector_2(p) {}

  Vector_2(const CGAL::Direction_2<R> &d) : RVector_2(d) {}
};

template < class R >
No_number_tag
number_type_tag(const Vector_2<R> &)
{ return No_number_tag(); }
#ifndef NO_OSTREAM_INSERT_VECTOR_2
template < class R >
std::ostream &
operator<<(std::ostream &os, const Vector_2<R> &v)
{
  typedef typename  R::Vector_2_base  RVector_2;
  return os << (const RVector_2&)v;
}
#endif // NO_OSTREAM_INSERT_VECTOR_2

#ifndef NO_ISTREAM_EXTRACT_VECTOR_2
template < class R >
std::istream &
operator>>(std::istream &is, Vector_2<R> &p)
{
  typedef typename  R::Vector_2_base  RVector_2;
  return is >> (RVector_2&)p;
}
#endif // NO_ISTREAM_EXTRACT_VECTOR_2

CGAL_END_NAMESPACE


#endif // CGAL_VECTOR_2_H

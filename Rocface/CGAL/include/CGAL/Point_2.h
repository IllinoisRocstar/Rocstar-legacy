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
// source        : Point_2.fw
// file          : include/CGAL/Point_2.h
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
 

#ifndef CGAL_POINT_2_H
#define CGAL_POINT_2_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifdef CGAL_HOMOGENEOUS_H
#include <CGAL/PointH2.h>
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#include <CGAL/Cartesian/Point_2.h>
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/PointS2.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


#include <CGAL/point_vector_declarations_2.h>

CGAL_BEGIN_NAMESPACE

template <class R_>
class Point_2 : public R_::Point_2_base
{
public:
  typedef  R_   R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Point_2_base  RPoint_2;
  typedef typename R::Vector_2_base  RVector_2;


friend  CGAL_FRIEND_INLINE
        CGAL::Point_2<R>
        CGAL_SCOPE vector_to_point_conversion CGAL_NULL_TMPL_ARGS
                                         (const CGAL::Vector_2<R>& v);

  Point_2()
  {}

  Point_2(const Origin& o)
    : RPoint_2(o)
  {}

  Point_2(const CGAL::Point_2<R>& p)
    : RPoint_2((RPoint_2&)p)
  {}

  Point_2(const RPoint_2& p)
    : RPoint_2(p)
  {}

  Point_2(const RT& hx, const RT& hy)
    : RPoint_2(hx, hy)
  {}

  Point_2(const RT& hx, const RT& hy, const RT& hw)
    : RPoint_2(hx, hy, hw)
  {}


  bool operator==(const CGAL::Point_2<R>& p) const
  {
    return RPoint_2::operator==(p);
  }

  bool operator!=(const CGAL::Point_2<R>& p) const
  {
    return !(*this == p);
  }

  RT hx() const
  {
    return RPoint_2::hx();
  }

  RT hy() const
  {
    return RPoint_2::hy();
  }

  RT hw() const
  {
    return RPoint_2::hw();
  }
  FT x() const
  {
    return RPoint_2::x();
  }

  FT y() const
  {
    return RPoint_2::y();
  }

  RT homogeneous(int i) const
  {
    return RPoint_2::homogeneous(i);
  }

  FT cartesian(int i) const
  {
    return RPoint_2::cartesian(i);
  }

  FT operator[](int i) const
  {
    return cartesian(i);
  }

  int dimension() const
  {
    return 2;
  }

  Bbox_2       bbox() const
  {
    return RPoint_2::bbox();
  }

  CGAL::Point_2<R> transform(const CGAL::Aff_transformation_2<R>& t) const
  {
    return RPoint_2::transform(t);
  }

private:

  Point_2(const RVector_2& v)
    : RPoint_2(v)
  {}
};

#ifndef NO_OSTREAM_INSERT_POINT_2
template < class R >
std::ostream&
operator<<(std::ostream& os, const Point_2<R>& p)
{
  typedef typename  R::Point_2_base  RPoint_2;
  return os << (const RPoint_2&)p;
}
#endif // NO_OSTREAM_INSERT_POINT_2

#ifndef NO_ISTREAM_EXTRACT_POINT_2
template < class R >
std::istream&
operator>>(std::istream& is, Point_2<R>& p)
{
  typedef typename  R::Point_2_base  RPoint_2;
  return is >> (RPoint_2&)p;
}
#endif // NO_ISTREAM_EXTRACT_POINT_2

template <class R>
inline
Point_2<R>
operator+(const Origin& o, const Vector_2<R>& v);

template <class R>
inline
Point_2<R>
operator-(const Origin& o, const Vector_2<R>& v);

template <class R>
inline
Vector_2<R>
operator-(const Point_2<R>& p, const Origin& );

CGAL_END_NAMESPACE


#ifndef CGAL_VECTOR_2_H
#include <CGAL/Vector_2.h>
#endif // CGAL_VECTOR_2_H

#include <CGAL/point_vector_definitions_2.C>

#ifndef CGAL_AFF_TRANSFORMATION_2_H
#include <CGAL/Aff_transformation_2.h>
#endif // CGAL_AFF_TRANSFORMATION_2_H

CGAL_BEGIN_NAMESPACE

template <class R>
inline
bool
operator==(const Origin& o, const Point_2<R>& p)
{ return p == o; }

template <class R>
inline
bool
operator!=(const Origin& o, const Point_2<R>& p)
{ return p != o; }

CGAL_END_NAMESPACE


#endif // CGAL_POINT_2_H

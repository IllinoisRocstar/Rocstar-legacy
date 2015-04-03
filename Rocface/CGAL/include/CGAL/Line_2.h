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
// source        : Line_2.fw
// file          : include/CGAL/Line_2.h
// package       : _2 (3.6)
// revision      : 3.6
// revision_date : 30 Jul 2000 
// author(s)     : Andreas Fabri
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_LINE_2_H
#define CGAL_LINE_2_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifdef CGAL_HOMOGENEOUS_H
#ifndef CGAL_LINEH2_H
#include <CGAL/LineH2.h>
#endif // CGAL_LINEH2_H
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#ifndef CGAL_LINEC2_H
#include <CGAL/Cartesian/Line_2.h>
#endif // CGAL_LINEC2_H
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/LineS2.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


#ifndef CGAL_PREDICATES_ON_POINTS_2_H
#include <CGAL/predicates_on_points_2.h>
#endif // CGAL_PREDICATES_ON_POINTS_2_H
#ifndef CGAL_VECTOR_2_H
#include <CGAL/Vector_2.h>
#endif // CGAL_VECTOR_2_H

CGAL_BEGIN_NAMESPACE

template <class R_>
class Line_2 : public R_::Line_2_base
{
public:
  typedef  R_   R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Line_2_base  RLine_2;

  Line_2()
    : RLine_2()
  {}

  ~Line_2()
  {}

  Line_2(const CGAL::Line_2<R>  &l)
    : RLine_2((RLine_2&)l)
  {}

  Line_2(const CGAL::Point_2<R> &p, const CGAL::Point_2<R> &q)
    : RLine_2(p,q)
  {}

  Line_2(const RT &a, const RT &b, const RT &c)
    : RLine_2(a,b,c)
  {}


  Line_2(const RLine_2& l)  // conversion impl -> interface class
    : RLine_2(l)
  {}


  Line_2(const CGAL::Segment_2<R>& s)
    : RLine_2(s)
  {}

  Line_2(const CGAL::Ray_2<R>& r)
    : RLine_2(r)
  {}

  Line_2(const CGAL::Point_2<R> &p, const CGAL::Direction_2<R> &d)
    : RLine_2(p,d)
  {}


  bool operator==(const CGAL::Line_2<R> &l) const
  {
    return RLine_2::operator==(l);
  }

  bool operator!=(const CGAL::Line_2<R> &l) const
  {
    return !(*this == l);
  }

  RT a() const
  {
    return RLine_2::a();
  }

  RT b() const
  {
    return RLine_2::b();
  }

  RT c() const
  {
    return RLine_2::c();
  }


  FT x_at_y(const FT &y) const
  {
    return RLine_2::x_at_y(y);
  }

  FT y_at_x(const FT &x) const
  {
    return RLine_2::y_at_x(x);
  }

  CGAL::Line_2<R> perpendicular(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::perpendicular(p);
  }

  CGAL::Line_2<R> opposite() const
  {
    return RLine_2::opposite();
  }

  CGAL::Point_2<R> point(int i) const
  {
    return RLine_2::point(i);
  }

  CGAL::Point_2<R> projection(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::projection(p);
  }

  CGAL::Point_2<R> point() const
  {
    return RLine_2::point();
  }

  CGAL::Direction_2<R> direction() const
  {

    return RLine_2::direction();
  }

  Oriented_side oriented_side(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::oriented_side(p);
  }

  bool has_on(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::has_on_boundary(p);
  }

  bool has_on_boundary(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::has_on_boundary(p);
  }

  bool has_on_positive_side(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::has_on_positive_side(p);
  }

  bool has_on_negative_side(const CGAL::Point_2<R> &p) const
  {
    return RLine_2::has_on_negative_side(p);
  }

  bool is_horizontal() const
  {

    return RLine_2::is_horizontal();
  }

  bool is_vertical() const
  {

    return RLine_2::is_vertical();
  }

  bool is_degenerate() const
  {

    return RLine_2::is_degenerate();
  }

  CGAL::Line_2<R> transform(const CGAL::Aff_transformation_2<R> &t) const
  {
    return  RLine_2::transform(t);
  }
};



#ifndef NO_OSTREAM_INSERT_LINE_2
template < class R >
std::ostream &
operator<<(std::ostream &os, const Line_2<R> &l)
{
  typedef typename  R::Line_2_base  RLine_2;
  return os << (const RLine_2&)l;
}
#endif // NO_OSTREAM_INSERT_LINE_2

#ifndef NO_ISTREAM_EXTRACT_LINE_2
template < class R >
std::istream &
operator>>(std::istream &is, Line_2<R> &p)
{
  typedef typename  R::Line_2_base  RLine_2;
  return is >> (RLine_2&)p;
}
#endif // NO_ISTREAM_EXTRACT_LINE_2



CGAL_END_NAMESPACE


#ifndef CGAL_SEGMENT_2_H
#include <CGAL/Segment_2.h>
#endif // CGAL_SEGMENT_2_H
#ifndef CGAL_RAY_2_H
#include <CGAL/Ray_2.h>
#endif // CGAL_RAY_2_H

#endif  // CGAL_LINE_2_H

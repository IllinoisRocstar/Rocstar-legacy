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
// source        : Ray_2.fw
// file          : include/CGAL/Ray_2.h
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
 

#ifndef CGAL_RAY_2_H
#define CGAL_RAY_2_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifndef CGAL_SEGMENT_2_H
#include <CGAL/Segment_2.h>
#endif // CGAL_SEGMENT_2_H

#ifdef CGAL_HOMOGENEOUS_H
#ifndef CGAL_RAYH2_H
#include <CGAL/RayH2.h>
#endif // CGAL_RAYH2_H
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#ifndef CGAL_RAYC2_H
#include <CGAL/Cartesian/Ray_2.h>
#endif // CGAL_RAYC2_H
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/RayS2.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


CGAL_BEGIN_NAMESPACE

template <class R_>
class Ray_2 : public R_::Ray_2_base
{
public:
  typedef  R_   R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Ray_2_base  RRay_2;

    Ray_2()
      : RRay_2()
  {}

  ~Ray_2()
  {}

  Ray_2(const CGAL::Ray_2<R> &r)
    : RRay_2((const RRay_2&)r)
  {

  }

  Ray_2(const RRay_2& r)
    : RRay_2(r)
  {

  }

  Ray_2(const CGAL::Point_2<R> &sp,
             const CGAL::Point_2<R> &secondp)
    : RRay_2(sp, secondp)
  {}

  Ray_2(const CGAL::Point_2<R> &sp,
             const CGAL::Direction_2<R> &d)
    : RRay_2(sp, d)
  {}


  bool operator==(const CGAL::Ray_2<R> &r) const
  { return RRay_2::operator==(r); }

  bool operator!=(const CGAL::Ray_2<R> &r) const
  { return !(*this == r); }

  CGAL::Point_2<R> start() const
  { return RRay_2::start(); }

  CGAL::Point_2<R> source() const
  { return RRay_2::source(); }

  CGAL::Point_2<R> second_point() const
  { return RRay_2::second_point(); }

  CGAL::Point_2<R> point(int i) const
  { return RRay_2::point(i); }

  CGAL::Direction_2<R> direction() const
  { return RRay_2::direction(); }

  CGAL::Line_2<R> supporting_line() const
  { return RRay_2::supporting_line(); }

  CGAL::Ray_2<R> opposite() const
  { return RRay_2::opposite(); }

  CGAL::Ray_2<R> transform(const CGAL::Aff_transformation_2<R> &t) const
  { return RRay_2::transform(t); }

  bool is_horizontal() const
  { return RRay_2::is_horizontal(); }

  bool is_vertical() const
  { return RRay_2::is_vertical(); }

  bool is_degenerate() const
  { return RRay_2::is_degenerate(); }

  bool has_on(const CGAL::Point_2<R> &p) const
  { return RRay_2::has_on(p); }

  bool collinear_has_on(const CGAL::Point_2<R> &p) const
  { return RRay_2::collinear_has_on(p); }

};

#ifndef NO_OSTREAM_INSERT_RAY_2
template < class R >
std::ostream &
operator<<(std::ostream &os, const Ray_2<R> &r)
{
  typedef typename  R::Ray_2_base  RRay_2;
  return os << (const RRay_2&)r;
}
#endif // NO_OSTREAM_INSERT_RAY_2

#ifndef NO_ISTREAM_EXTRACT_RAY_2
template < class R >
std::istream &
operator>>(std::istream &is, Ray_2<R> &r)
{
  typedef typename  R::Ray_2_base  RRay_2;
  return is >> (RRay_2&)r;
}
#endif // NO_ISTREAM_EXTRACT_RAY_2

CGAL_END_NAMESPACE


#endif  // CGAL_RAY_2_H

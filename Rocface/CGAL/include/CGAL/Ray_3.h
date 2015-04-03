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
// source        : Ray_3.fw
// file          : include/CGAL/Ray_3.h
// package       : _3 (3.7)
// revision      : 3.7
// revision_date : 16 Aug 2000 
// author(s)     : Andreas Fabri
//                 Stefan Schirra
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_RAY_3_H
#define CGAL_RAY_3_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifdef CGAL_HOMOGENEOUS_H
#ifndef CGAL_RAYH3_H
#include <CGAL/RayH3.h>
#endif // CGAL_RAYH3_H
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#ifndef CGAL_RAYC3_H
#include <CGAL/Cartesian/Ray_3.h>
#endif // CGAL_RAYC3_H
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/RayS3.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


CGAL_BEGIN_NAMESPACE

template <class R_>
class Ray_3 : public R_::Ray_3_base
{
public:
  typedef          R_                       R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Ray_3_base  RRay_3;

  Ray_3() : RRay_3()
  {}
  Ray_3(const CGAL::Ray_3<R>& r) : RRay_3(r)
  {}
  Ray_3(const RRay_3&  r) : RRay_3(r)
  {}
  Ray_3(const CGAL::Point_3<R>& sp,
            const CGAL::Point_3<R>& secondp)
    : RRay_3(sp, secondp)
  {}
  Ray_3(const CGAL::Point_3<R>& sp,
            const CGAL::Direction_3<R>& d)
    : RRay_3(sp, d)
  {}

  bool                operator==(const CGAL::Ray_3<R>& r) const
  { return RRay_3::operator==(r); }
  bool                operator!=(const CGAL::Ray_3<R>& r) const
  { return !(*this == r); }

  CGAL::Point_3<R>     start() const
  { return RRay_3::start(); }
  CGAL::Point_3<R>     source() const
  { return RRay_3::source(); }
  CGAL::Point_3<R>     second_point() const
  { return RRay_3::second_point(); }
  CGAL::Point_3<R>     point(int i) const
  { return RRay_3::point(i); }
  CGAL::Direction_3<R> direction() const
  { return RRay_3::direction(); }
  CGAL::Line_3<R>      supporting_line() const
  { return RRay_3::supporting_line(); }
  CGAL::Ray_3<R>       opposite() const
  { return RRay_3::opposite(); }
  CGAL::Ray_3<R>       transform(const CGAL::Aff_transformation_3<R>& t) const
  { return RRay_3::transform(t); }
  bool                is_degenerate() const
  { return RRay_3::is_degenerate(); }
  bool                has_on(const CGAL::Point_3<R>& p) const
  { return RRay_3::has_on(p); }
};

#ifndef NO_OSTREAM_INSERT_RAY_3
template < class R >
std::ostream&
operator<<(std::ostream& os, const Ray_3<R>& r)
{
  typedef typename  R::Ray_3_base  RRay_3;
  return os << (const RRay_3& )r;
}
#endif // NO_OSTREAM_INSERT_RAY_3

#ifndef NO_ISTREAM_EXTRACT_RAY_3
template < class R >
std::istream&
operator>>(std::istream& is, Ray_3<R>& r)
{
  typedef typename  R::Ray_3_base  RRay_3;
  return is >> (RRay_3& )r;
}
#endif // NO_ISTREAM_EXTRACT_RAY_3


CGAL_END_NAMESPACE


#ifndef CGAL_LINE_3_H
#include <CGAL/Line_3.h>
#endif // CGAL_LINE_3_H

#endif // CGAL_RAY_3_H

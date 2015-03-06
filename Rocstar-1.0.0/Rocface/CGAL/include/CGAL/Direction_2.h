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
// source        : Direction_2.fw
// file          : include/CGAL/Direction_2.h
// package       : _2 (3.6)
// revision      : 3.6
// revision_date : 30 Jul 2000 
// author(s)     : Stefan Schirra
//
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_DIRECTION_2_H
#define CGAL_DIRECTION_2_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifdef CGAL_HOMOGENEOUS_H
#ifndef CGAL_DIRECTIONH2_H
#include <CGAL/DirectionH2.h>
#endif // CGAL_DIRECTIONH2_H
#endif  // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#ifndef CGAL_DIRECTIONC2_H
#include <CGAL/Cartesian/Direction_2.h>
#endif // CGAL_DIRECTIONC2_H
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/DirectionS2.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


#ifndef CGAL_VECTOR_2_H
#include <CGAL/Vector_2.h>
#endif // CGAL_VECTOR_2_H

CGAL_BEGIN_NAMESPACE

template <class R_>
class Direction_2 : public R_::Direction_2_base
{
public:
  typedef  R_   R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Vector_2_base  RVector_2;
  typedef typename R::Direction_2_base  RDirection_2;
  // typedef typename R::Aff_transformation_2  Aff_transformation_2;

  Direction_2()
  {}

  Direction_2(const CGAL::Direction_2<R> &d)
    : RDirection_2((const RDirection_2&)d)
  {}


  Direction_2(const RDirection_2& d)
    : RDirection_2(d)
  {}


  Direction_2(const RVector_2& v)
    : RDirection_2(v)
  {}

  Direction_2(const RT &x, const RT &y)
    :  RDirection_2(x,y)
  {}

  bool
  operator==(const CGAL::Direction_2<R> &d) const
  { return RDirection_2::operator==(d); }

  bool
  operator!=(const CGAL::Direction_2<R> &d) const
  { return !(*this == d); }

  bool
  operator>=(const CGAL::Direction_2<R> &d) const
  { return RDirection_2::operator>=(d); }

  bool
  operator<=(const CGAL::Direction_2<R> &d) const
  { return RDirection_2::operator<=(d); }

  bool
  operator>(const CGAL::Direction_2<R> &d) const
  { return RDirection_2::operator>(d); }

  bool
  operator<(const CGAL::Direction_2<R> &d) const
  { return RDirection_2::operator<(d); }

  bool
  counterclockwise_in_between(const CGAL::Direction_2<R> &d1,
                                   const CGAL::Direction_2<R> &d2) const
  { return RDirection_2::counterclockwise_in_between(d1,d2); }

  CGAL::Vector_2<R>
  vector() const
  { return (CGAL::Vector_2<R>)RDirection_2::to_vector(); }

  CGAL::Vector_2<R>
  to_vector() const
  { return (CGAL::Vector_2<R>)RDirection_2::to_vector(); }

  CGAL::Direction_2<R>
  transform(const CGAL::Aff_transformation_2<R> &t) const
  { return RDirection_2::transform(t); }

  CGAL::Direction_2<R>
  operator-() const
  { return RDirection_2::operator-(); }

  RT
  delta(int i) const
  { return RDirection_2::delta(i); }

  RT
  dx() const
  { return RDirection_2::dx(); }

  RT
  dy() const
  { return RDirection_2::dy(); }
};


#ifndef NO_OSTREAM_INSERT_DIRECTION_2
template < class R >
std::ostream &
operator<<(std::ostream &os, const Direction_2<R> &d)
{
  typedef typename  R::Direction_2_base  RDirection_2;
  return os << (const RDirection_2&)d;
}

#endif // NO_OSTREAM_INSERT_DIRECTION_2

#ifndef NO_ISTREAM_EXTRACT_DIRECTION_2
template < class R >
std::istream &
operator>>(std::istream &is, Direction_2<R> &p)
{
  typedef typename  R::Direction_2_base  RDirection_2;
  return is >> (RDirection_2&)p;
}
#endif // NO_ISTREAM_EXTRACT_DIRECTION_2


CGAL_END_NAMESPACE


#endif // CGAL_DIRECTION_2_H

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
// source        : Direction_3.fw
// file          : include/CGAL/Direction_3.h
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
 

#ifndef CGAL_DIRECTION_3_H
#define CGAL_DIRECTION_3_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifdef CGAL_HOMOGENEOUS_H
#ifndef CGAL_DIRECTIONH3_H
#include <CGAL/DirectionH3.h>
#endif // CGAL_DIRECTIONH3_H
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#ifndef CGAL_DIRECTIONC3_H
#include <CGAL/Cartesian/Direction_3.h>
#endif // CGAL_DIRECTIONC3_H
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/DirectionS3.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


#ifndef CGAL_VECTOR_3_H
#include <CGAL/Vector_3.h>
#endif // CGAL_VECTOR_3_H

CGAL_BEGIN_NAMESPACE

template <class R_>
class Direction_3 : public R_::Direction_3_base
{
public:
  typedef          R_                       R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Direction_3_base  RDirection_3;
  typedef typename R::Vector_3_base  RVector_3;

  Direction_3()
  {}
  Direction_3(const CGAL::Direction_3<R>& d)
    : RDirection_3( (const RDirection_3& )d )
  {}
  Direction_3(const RDirection_3&  d)
    : RDirection_3(d)
  {}
  Direction_3(const RVector_3&  v)
    : RDirection_3(v)
  {}
  Direction_3(const RT& hx, const RT& hy, const RT& hz)
    : RDirection_3(hx, hy, hz)
  {}

  bool operator==(const CGAL::Direction_3<R> & d) const
  { return RDirection_3::operator==(d); }

  bool operator!=(const CGAL::Direction_3<R> & d) const
  { return !(*this == d); }

  CGAL::Vector_3<R> vector() const
  { return (CGAL::Vector_3<R>)RDirection_3::to_vector(); }

  CGAL::Vector_3<R> to_vector() const
  { return (CGAL::Vector_3<R>)RDirection_3::to_vector(); }

  CGAL::Direction_3<R> transform(const CGAL::Aff_transformation_3<R> & t) const
  { return RDirection_3::transform(t); }

  CGAL::Direction_3<R> operator-() const
  { return RDirection_3::operator-(); }

  RT delta(int i) const
  { return RDirection_3::delta(i); }

  RT dx() const
  { return RDirection_3::dx(); }

  RT dy() const
  { return RDirection_3::dy(); }

  RT dz() const
  { return RDirection_3::dz(); }
};


#ifndef NO_OSTREAM_INSERT_DIRECTION_3
template < class R >
std::ostream& operator<<(std::ostream& os, const Direction_3<R>& d)
{
  typedef typename  R::Direction_3_base  RDirection_3;
  return os << (const RDirection_3& )d; }
#endif // NO_OSTREAM_INSERT_DIRECTION_3


#ifndef NO_ISTREAM_EXTRACT_DIRECTION_3
template < class R >
std::istream& operator>>(std::istream& is, Direction_3<R>& p)
{
  typedef typename  R::Direction_3_base  RDirection_3;
  return is >> (RDirection_3& )p; }
#endif // NO_ISTREAM_EXTRACT_DIRECTION_3

CGAL_END_NAMESPACE


#endif // CGAL_DIRECTION_3_H

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
// source        : Aff_transformation_3.fw
// file          : include/CGAL/Aff_transformation_3.h
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
 

#ifndef CGAL_AFF_TRANSFORMATION_3_H
#define CGAL_AFF_TRANSFORMATION_3_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#if defined(CGAL_CFG_INCOMPLETE_TYPE_BUG_1) && \
   !defined(CGAL_NO_PLANE_TRANSFORM_IN_AT)
#define CGAL_NO_PLANE_TRANSFORM_IN_AT
#endif // CGAL_CFG_INCOMPLETE_TYPE_BUG_1

#ifdef CGAL_HOMOGENEOUS_H
#include <CGAL/Aff_transformationH3.h>
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#include <CGAL/Cartesian/Aff_transformation_3.h>
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/Aff_transformationS3.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


#include <CGAL/Point_3.h>
#include <CGAL/Vector_3.h>
#include <CGAL/Direction_3.h>
#include <CGAL/Plane_3.h>

CGAL_BEGIN_NAMESPACE

template <class R_>
class Aff_transformation_3 : public R_::Aff_transformation_3_base
{
public:
  typedef R_                                R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Plane_3_base  RPlane_3;
  typedef typename R::Aff_transformation_3_base  RAff_transformation_3;

  // default constructor
  Aff_transformation_3() : RAff_transformation_3()
  {}

  // copy constructor
  Aff_transformation_3(const CGAL::Aff_transformation_3<R>& t)
    : RAff_transformation_3(t)
  {}

  // up cast constructor
  Aff_transformation_3(const RAff_transformation_3&  t)
    : RAff_transformation_3(t)
  {}

  // identity:
  Aff_transformation_3(const Identity_transformation& tag)
    : RAff_transformation_3(tag)
  {}

  // translation:
  Aff_transformation_3(const Translation tag,
                       const CGAL::Vector_3<R>& v)
    : RAff_transformation_3(tag, v)
  {}

  // scaling:
  Aff_transformation_3(const Scaling tag,
                       const RT& s,
                       const RT& w= RT(1) )
    : RAff_transformation_3(tag, s, w)
  {}

  // the general case:
  Aff_transformation_3(
      const RT& m11, const RT& m12, const RT& m13, const RT& m14,
      const RT& m21, const RT& m22, const RT& m23, const RT& m24,
      const RT& m31, const RT& m32, const RT& m33, const RT& m34,
                                                   const RT& w= RT(1) )
    : RAff_transformation_3(m11, m12, m13, m14,
                            m21, m22, m23, m24,
                            m31, m32, m33, m34,
                                           w)
  {}
  Aff_transformation_3(
      const RT& m11, const RT& m12, const RT& m13,
      const RT& m21, const RT& m22, const RT& m23,
      const RT& m31, const RT& m32, const RT& m33,
                                                   const RT& w = RT(1) )
    : RAff_transformation_3(m11, m12, m13,
                           m21, m22, m23,
                           m31, m32, m33,
                                          w)
  {}
  // dtor
  ~Aff_transformation_3()
  {}
  // transformations
  CGAL::Point_3<R>       transform(const CGAL::Point_3<R>& p) const
                        { return RAff_transformation_3::transform(p); }
  CGAL::Point_3<R>       operator()(const CGAL::Point_3<R>& p) const
                        { return RAff_transformation_3::transform(p); }
  CGAL::Vector_3<R>      transform(const CGAL::Vector_3<R>& v) const
                        { return RAff_transformation_3::transform(v); }
  CGAL::Vector_3<R>      operator()(const CGAL::Vector_3<R>& v) const
                        { return RAff_transformation_3::transform(v); }
  CGAL::Direction_3<R>   transform(const CGAL::Direction_3<R>& d) const
                        { return RAff_transformation_3::transform(d); }
  CGAL::Direction_3<R>   operator()(const CGAL::Direction_3<R>& d) const
                        { return RAff_transformation_3::transform(d); }
#ifndef CGAL_NO_PLANE_TRANSFORM_IN_AT
  CGAL::Plane_3<R>       transform(const CGAL::Plane_3<R>& pl) const
                        { return RAff_transformation_3::transform(pl); }
#else
  CGAL::Plane_3<R>
    transform(const CGAL::Plane_3<R>& pl) const
    {
      return
      (( const RPlane_3&  )pl).transform( (const RAff_transformation_3& )(*this) );
    }
#endif // CGAL_NO_PLANE_TRANSFORM_IN_AT
  CGAL::Plane_3<R>       operator()(const CGAL::Plane_3<R>& pl) const
                        { return transform(pl); }
  // further members
  CGAL::Aff_transformation_3<R>
                        inverse() const
                        { return RAff_transformation_3::inverse(); }
  bool                  is_even() const
                        { return RAff_transformation_3::is_even(); }
  bool                  is_odd() const
                        { return !is_even(); }
  // access
  FT                    cartesian(int i, int j) const
                        { return RAff_transformation_3::cartesian(i,j); }
  RT                    homogeneous(int i, int j) const
                        { return RAff_transformation_3::homogeneous(i,j); }
  FT                    m(int i, int j) const
                        { return RAff_transformation_3::m(i,j); }
  RT                    hm(int i, int j) const
                        { return RAff_transformation_3::hm(i,j); }
  // composition
  CGAL::Aff_transformation_3<R>
                        operator*(const CGAL::Aff_transformation_3<R>& t) const
                        {
                          return
                          static_cast<const RAff_transformation_3&>(*this) *
                          static_cast<const RAff_transformation_3&>(t) ;
                        }
};

// I/O operators
#ifndef NO_OSTREAM_INSERT_AFF_TRANSFORMATION_3
template < class R >
std::ostream&
operator<<(std::ostream& os, const CGAL::Aff_transformation_3<R>& t)
{
  typedef typename   R::Aff_transformation_3_base  RAff_transformation_3;
  return os << static_cast<const RAff_transformation_3&>(t);
}
#endif // NO_OSTREAM_INSERT_AFF_TRANSFORMATION_3

#ifndef NO_ISTREAM_EXTRACT_AFF_TRANSFORMATION_3
template < class R >
std::istream&
operator>>(std::istream& is, CGAL::Aff_transformation_3<R>& t)
{
  typedef typename   R::Aff_transformation_3_base  RAff_transformation_3;
  return is >> static_cast<const RAff_transformation_3&>(t);
}
#endif // NO_ISTREAM_EXTRACT_AFF_TRANSFORMATION_3

CGAL_END_NAMESPACE


#endif // CGAL_AFF_TRANSFORMATION_3_H

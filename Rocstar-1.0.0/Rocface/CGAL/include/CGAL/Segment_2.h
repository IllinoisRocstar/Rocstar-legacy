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
// source        : Segment_2.fw
// file          : include/CGAL/Segment_2.h
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
 

#ifndef CGAL_SEGMENT_2_H
#define CGAL_SEGMENT_2_H

#ifndef CGAL_REP_CLASS_DEFINED
#error  no representation class defined
#endif  // CGAL_REP_CLASS_DEFINED

#ifdef CGAL_HOMOGENEOUS_H
#ifndef CGAL_SEGMENTH2_H
#include <CGAL/SegmentH2.h>
#endif // CGAL_SEGMENTH2_H
#endif // CGAL_HOMOGENEOUS_H

#ifdef CGAL_CARTESIAN_H
#ifndef CGAL_SEGMENTC2_H
#include <CGAL/Cartesian/Segment_2.h>
#endif // CGAL_SEGMENTC2_H
#endif // CGAL_CARTESIAN_H

#ifdef CGAL_SIMPLE_CARTESIAN_H
#include <CGAL/SimpleCartesian/SegmentS2.h>
#endif // CGAL_SIMPLE_CARTESIAN_H


#ifndef CGAL_LINE_2_H
#include <CGAL/Line_2.h>
#endif // CGAL_LINE_2_H

CGAL_BEGIN_NAMESPACE

template <class R_>
class Segment_2 : public R_::Segment_2_base
{
public:
  typedef  R_                               R;
  typedef typename R::RT                    RT;
  typedef typename R::FT                    FT;
  typedef typename R::Segment_2_base  RSegment_2;

  Segment_2()     // doesn't the default constructor do the same ???
    : RSegment_2()  // does the handle stuff
  {}

  ~Segment_2()
  {}

  Segment_2(const CGAL::Segment_2<R>& s)
    : RSegment_2((RSegment_2&)s)  // does the handle stuff
  {}

  Segment_2(const CGAL::Point_2<R> &sp, const CGAL::Point_2<R> &ep)
    :  RSegment_2(sp,ep)
  {}


  // conversion from implementation class object to interface class object
  Segment_2(const RSegment_2& s)
    : RSegment_2(s)  // does the handle stuff
  {}


  bool  is_horizontal() const
  { return RSegment_2::is_horizontal(); }

  bool  is_vertical() const
  { return RSegment_2::is_vertical(); }

  bool  has_on(const CGAL::Point_2<R> &p) const
  { return RSegment_2::has_on(p); }

  bool  collinear_has_on(const CGAL::Point_2<R> &p) const
  { return RSegment_2::collinear_has_on(p); }


  bool  operator==(const CGAL::Segment_2<R> &s) const
  { return RSegment_2::operator==(s); }

  bool  operator!=(const CGAL::Segment_2<R> &s) const
  { return !(*this == s); }

  CGAL::Point_2<R>     start() const
  { return RSegment_2::start(); }

  CGAL::Point_2<R>     end() const
  { return RSegment_2::end(); }

  CGAL::Point_2<R>     source() const
  { return RSegment_2::source(); }

  CGAL::Point_2<R>     target() const
  { return RSegment_2::target(); }

  CGAL::Point_2<R>     min() const
  { return RSegment_2::min(); }

  CGAL::Point_2<R>     max() const
  { return RSegment_2::max(); }

  CGAL::Point_2<R>     vertex(int i) const
  { return RSegment_2::vertex(i); }

  CGAL::Point_2<R>     point(int i) const
  { return RSegment_2::vertex(i); }

  CGAL::Point_2<R>     operator[](int i) const
  { return vertex(i); }

  FT                 squared_length() const
  { return RSegment_2::squared_length(); }

  CGAL::Direction_2<R> direction() const
  { return RSegment_2::direction(); }

  CGAL::Segment_2<R>  opposite() const
  { return CGAL::Segment_2<R>(target(),source()); }

  // this makes use of the constructor of the interface class
  // taking an object of the implemetation class as argument.

  CGAL::Segment_2<R>   transform(const CGAL::Aff_transformation_2<R> &t) const
  { return  RSegment_2::transform(t); }


  CGAL::Line_2<R>      supporting_line() const
  { return RSegment_2::supporting_line(); }

  bool                is_degenerate() const
  { return RSegment_2::is_degenerate(); }

  Bbox_2         bbox() const
  { return source().bbox() + target().bbox(); }

};

#ifndef NO_OSTREAM_INSERT_SEGMENT_2
template < class R>
std::ostream &
operator<<(std::ostream &os, const Segment_2<R> &s)
{
  typedef typename  R::Segment_2_base  RSegment_2;
  return os << (const RSegment_2&)s;
}
#endif // NO_OSTREAM_INSERT_SEGMENT_2

#ifndef NO_ISTREAM_EXTRACT_SEGMENT_2
template < class R>
std::istream &
operator>>(std::istream &is, Segment_2<R> &s)
{
  typedef typename  R::Segment_2_base  RSegment_2;
  return is >> (RSegment_2&)s;
}
#endif // NO_ISTREAM_EXTRACT_SEGMENT_2

CGAL_END_NAMESPACE


#endif //  CGAL_SEGMENT_2_H

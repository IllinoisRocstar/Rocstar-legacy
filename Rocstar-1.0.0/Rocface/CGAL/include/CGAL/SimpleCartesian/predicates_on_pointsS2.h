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
// release       : CGAL-2.2
// release_date  : 2000, September 30
//
// source        : webS2/S2.lw
// file          : include/CGAL/SimpleCartesian/predicates_on_pointsS2.h
// package       : S2 (1.7)
// revision      : 1.6
// revision_date : 27 Jun 2000
// author(s)     : Stefan Schirra
//                 based on code by
//                 Andreas Fabri and
//                 Herve Brönnimann
//
// coordinator   : MPI, Saarbrücken
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================


#ifndef CGAL_PREDICATES_ON_POINTSS2_H
#define CGAL_PREDICATES_ON_POINTSS2_H

#include <CGAL/SimpleCartesian/simple_cartesian_classes.h>
#include <CGAL/SimpleCartesian/PointS2.h>
#include <CGAL/predicates/kernel_ftC2.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
inline
bool
x_equal(const PointS2<FT> &p, const PointS2<FT> &q)
{ return p.x() == q.x(); }

template < class FT >
inline
bool
y_equal(const PointS2<FT> &p, const PointS2<FT> &q)
{ return p.y() == q.y(); }

template < class FT >
inline
bool
equal_xy(const PointS2<FT> &p, const PointS2<FT> &q)
{ return (p.x() == q.x()) && (p.y() == q.y()); }

template < class FT >
inline
bool
less_x(const PointS2<FT> &p, const PointS2<FT> &q)
{ return (p.x() < q.x()); }

template < class FT >
inline
bool
less_y(const PointS2<FT> &p, const PointS2<FT> &q)
{ return (p.y() < q.y()); }

template < class FT >
inline
Comparison_result
compare_x(const PointS2<FT> &p, const PointS2<FT> &q)
{ return CGAL_NTS compare(p.x(), q.x()); }

template < class FT >
inline
Comparison_result
compare_y(const PointS2<FT> &p, const PointS2<FT> &q)
{ return CGAL_NTS compare(p.y(), q.y()); }

template < class FT >
inline
Comparison_result
compare_deltax_deltay(const PointS2<FT>& p,
                      const PointS2<FT>& q,
                      const PointS2<FT>& r,
                      const PointS2<FT>& s)
{
  return compare_deltax_deltayC2(p.x(), q.x(), r.y(), s.y());
}

template < class FT >
inline
Comparison_result
compare_lexicographically_xy(const PointS2<FT> &p,
                             const PointS2<FT> &q)
{
  return compare_lexicographically_xyC2(p.x(),p.y(),q.x(),q.y());
}

template < class FT >
inline
bool
lexicographically_xy_smaller_or_equal(const PointS2<FT> &p,
                                      const PointS2<FT> &q)
{
  return ( !( compare_lexicographically_xy(p,q) == LARGER ) );
}

template < class FT >
inline
bool
lexicographically_xy_smaller(const PointS2<FT> &p,
                             const PointS2<FT> &q)
{
  return compare_lexicographically_xy(p,q) == SMALLER ;
}

template < class FT >
inline
Comparison_result
compare_lexicographically_yx(const PointS2<FT> &p,
                             const PointS2<FT> &q)
{
  return compare_lexicographically_xyC2(p.y(),p.x(),q.y(),q.x());
}


template < class FT >
inline
bool
lexicographically_yx_smaller_or_equal(const PointS2<FT> &p,
                                      const PointS2<FT> &q)
{
    return  !( compare_lexicographically_yx(p,q) == LARGER ) ;
}

template < class FT >
inline
bool
lexicographically_yx_smaller(const PointS2<FT> &p,
                             const PointS2<FT> &q)
{
    return compare_lexicographically_yx(p,q) == SMALLER ;
}

template < class FT >
inline
Orientation
orientation(const PointS2<FT> &p,
            const PointS2<FT> &q,
            const PointS2<FT> &r)
{
    return orientationC2(p.x(), p.y(), q.x(), q.y(), r.x(), r.y());
}

template < class FT >
inline
bool
collinear(const PointS2<FT> &p,
               const PointS2<FT> &q,
               const PointS2<FT> &r)
{
  return (orientation(p,q,r) == COLLINEAR);
}



template < class FT >
inline
bool
collinear_are_ordered_along_line(const PointS2<FT> &p,
                                 const PointS2<FT> &q,
                                 const PointS2<FT> &r)
{
  return collinear_are_ordered_along_lineC2
             (p.x(),p.y(),q.x(),q.y(),r.x(),r.y());
}


template < class FT >
inline
bool
are_ordered_along_line(const PointS2<FT> &p,
                       const PointS2<FT> &q,
                       const PointS2<FT> &r)
{
  if (!collinear(p, q, r)) { return false; }
  return collinear_are_ordered_along_line(p, q, r);
}

template < class FT >
inline
bool
collinear_are_strictly_ordered_along_line(const PointS2<FT> &p,
                                          const PointS2<FT> &q,
                                          const PointS2<FT> &r)
{
  return collinear_are_strictly_ordered_along_lineC2
               (p.x(),p.y(),q.x(),q.y(),r.x(),r.y());
}


template < class FT >
inline
bool
are_strictly_ordered_along_line(const PointS2<FT> &p,
                                const PointS2<FT> &q,
                                const PointS2<FT> &r)
{
  if (!collinear(p, q, r)) { return false; }
  return collinear_are_strictly_ordered_along_line(p, q, r);
}

template < class FT >
inline
bool
leftturn(const PointS2<FT> &p,
         const PointS2<FT> &q,
         const PointS2<FT> &r)
{
  return (orientation(p,q,r) == LEFTTURN );
}

template < class FT >
inline
bool
leftturn(const Origin &o,
         const PointS2<FT> &q,
         const PointS2<FT> &r)
{
   return (orientationC2(FT(0), FT(0), q.x(), q.y(), r.x(), r.y())
           == LEFTTURN );
}

template < class FT >
inline
bool
rightturn(const PointS2<FT> &p,
          const PointS2<FT> &q,
          const PointS2<FT> &r)
{
   return (orientationC2(p.x(), p.y(), q.x(), q.y(), r.x(), r.y())
           == RIGHTTURN);
}

template < class FT >
inline
bool
rightturn(const Origin &o,
          const PointS2<FT> &q,
          const PointS2<FT> &r)
{
   return (orientationC2(FT(0), FT(0), q.x(), q.y(), r.x(), r.y())
           == RIGHTTURN);
}

template <class FT >
inline
Oriented_side
side_of_oriented_circle(const PointS2<FT> &p,
                        const PointS2<FT> &q,
                        const PointS2<FT> &r,
                        const PointS2<FT> &test)
{
  return side_of_oriented_circleC2
             (p.x(),p.y(),q.x(),q.y(),r.x(),r.y(),test.x(),test.y());
}


template <class FT >
inline
Bounded_side
side_of_bounded_circle(const PointS2<FT> &p,
                       const PointS2<FT> &q,
                       const PointS2<FT> &r,
                       const PointS2<FT> &test)
{
  return side_of_bounded_circleC2
             (p.x(),p.y(),q.x(),q.y(),r.x(),r.y(),test.x(),test.y());
}


CGAL_END_NAMESPACE

#endif  // CGAL_PREDICATES_ON_POINTSS2_H

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
// source        : webS3/S3.lw
// file          : include/CGAL/SimpleCartesian/LineS3.h
// package       : S3 (1.6)
// revision      : 1.6
// revision_date : 28 Jun 2000
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

#ifndef CGAL_LINES3_H
#define CGAL_LINES3_H

#include <CGAL/SimpleCartesian/predicates_on_pointsS3.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
class LineS3
{
public:
                  LineS3() {}
                  LineS3(const PointS3<FT>& p,
                         const PointS3<FT>& q);
                  LineS3(const SegmentS3<FT>& s);
                  LineS3(const RayS3<FT>& r);
                  LineS3(const PointS3<FT>& p,
                         const DirectionS3<FT>& d);

  bool            operator==(const LineS3<FT>& l) const;
  bool            operator!=(const LineS3<FT>& l) const;

  PlaneS3<FT>     perpendicular_plane(const PointS3<FT>& p) const;
  LineS3<FT>      opposite() const;

  PointS3<FT>     point() const;
  PointS3<FT>     point(int i) const;

  PointS3<FT>     projection(const PointS3<FT>& p) const;

  DirectionS3<FT> direction() const;

  bool            has_on(const PointS3<FT>& p) const;
  bool            is_degenerate() const;

  LineS3<FT>      transform(const Aff_transformationS3<FT>& t) const;


// private:
  void            new_rep(const PointS3<FT>& p,
                          const VectorS3<FT>& v);

  PointS3<FT>     e0;
  PointS3<FT>     e1;

};


template < class FT >
inline 
void 
LineS3<FT>::new_rep(const PointS3<FT>& p, const VectorS3<FT>& v)
{
  e0 = p;
  e1 = ORIGIN + v;
}


CGAL_END_NAMESPACE

#include <CGAL/SimpleCartesian/SegmentS3.h>
#include <CGAL/SimpleCartesian/RayS3.h>
#include <CGAL/SimpleCartesian/PlaneS3.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
LineS3<FT>::LineS3(const PointS3<FT>& p, const PointS3<FT>& q)
{ new_rep(p, q - p); }

template < class FT >
LineS3<FT>::LineS3(const SegmentS3<FT>& s)
{ new_rep(s.start(), s.end() - s.start()); }

template < class FT >
LineS3<FT>::LineS3(const RayS3<FT>& r)
{ new_rep(r.start(), r.second_point() - r.start()); }

template < class FT >
LineS3<FT>::LineS3(const PointS3<FT>& p, const DirectionS3<FT>& d)
{ new_rep(p, d.vector()); }

template < class FT >
bool LineS3<FT>::operator==(const LineS3<FT>& l) const
{ return has_on(l.point()) && (direction() == l.direction()); }

template < class FT >
inline 
bool 
LineS3<FT>::operator!=(const LineS3<FT>& l) const
{ return !(*this == l); }

template < class FT >
PointS3<FT> 
LineS3<FT>::point() const
{ return e0; }

template < class FT >
DirectionS3<FT> 
LineS3<FT>::direction() const
{ return (e1 - ORIGIN).direction(); }


template < class FT >
PointS3<FT> 
LineS3<FT>::point(int i) const
{ return PointS3<FT>(point() + FT(i) * (e1 - ORIGIN)); }

template < class FT >
PlaneS3<FT> 
LineS3<FT>::perpendicular_plane(const PointS3<FT>& p) const
{ return PlaneS3<FT>(p, direction().vector()); }

template < class FT >
LineS3<FT> 
LineS3<FT>::opposite() const
{ return LineS3<FT>(point(), -direction()); }

template < class FT >
PointS3<FT> 
LineS3<FT>::projection(const PointS3<FT>& p) const
{
  return point() + ( ((direction().vector() * (p - point())) /
                      (direction().vector() * direction().vector()))
                     * direction().vector() );
}

template < class FT >
bool 
LineS3<FT>::has_on(const PointS3<FT>& p) const
{ return collinear(point(), point()+direction().vector(), p); }


template < class FT >
bool 
LineS3<FT>::is_degenerate() const
{ return direction() == DirectionS3<FT>(0,0,0); }


template < class FT >
inline
LineS3<FT> 
LineS3<FT>::transform(const Aff_transformationS3<FT>& t) const
{ return LineS3<FT>( t.transform(point()), t.transform(direction())); }


#ifndef CGAL_NO_OSTREAM_INSERT_LINES3
template < class FT >
std::ostream& 
operator<<(std::ostream& os, const LineS3<FT>& l)
{
    switch(os.iword(IO::mode)) 
    {
      case IO::ASCII :
        return os << l.point(0) << ' ' << l.point(1);
      case IO::BINARY :
        return os << l.point(0) <<  l.point(1);
      default:
        return  os << "LineS3(" << l.point(0) << ", " << l.point(1) << ")";
    }
}
#endif // CGAL_NO_OSTREAM_INSERT_LINES3

#ifndef CGAL_NO_ISTREAM_EXTRACT_LINES3
template < class FT >
std::istream& 
operator>>(std::istream& is, LineS3<FT>& l)
{
    PointS3<FT> p, q;
    is >> p >> q;
    l = LineS3<FT>(p, q);
    return is;
}
#endif // CGAL_NO_ISTREAM_EXTRACT_LINES3


CGAL_END_NAMESPACE

#endif // CGAL_LINES3_H

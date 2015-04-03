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
// file          : include/CGAL/SimpleCartesian/SegmentS3.h
// package       : S3 (1.6)
// revision      : 1.6
// revision_date : 28 Jun 2000
// author(s)     : Stefan Schirra
//                 based on code by
//                 Andreas Fabri and
//                 Herve Br�nnimann
//
// coordinator   : MPI, Saarbr�cken
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_SEGMENTS3_H
#define CGAL_SEGMENTS3_H

#include <CGAL/SimpleCartesian/LineS3.h>
#include <CGAL/SimpleCartesian/predicates_on_pointsS3.h>
#include <CGAL/SimpleCartesian/basic_constructionsS3.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
class SegmentS3{
public:
                  SegmentS3() {}
                  SegmentS3(const PointS3<FT>& sp, const PointS3<FT>& ep)
                   : e0(sp), e1(ep) {}

  bool            has_on(const PointS3<FT>& p) const;
  bool            collinear_has_on(const PointS3<FT>& p) const;

  bool            operator==(const SegmentS3<FT>& s) const;
  bool            operator!=(const SegmentS3<FT>& s) const;

  PointS3<FT>     start() const;
  PointS3<FT>     end() const;

  PointS3<FT>     source() const;
  PointS3<FT>     target() const;

  PointS3<FT>     min() const;
  PointS3<FT>     max() const;
  PointS3<FT>     vertex(int i) const;
  PointS3<FT>     point(int i) const;
  PointS3<FT>     operator[](int i) const;

  FT              squared_length() const;

  DirectionS3<FT> direction() const;
  LineS3<FT>      supporting_line() const;
  SegmentS3       opposite() const;
  SegmentS3       transform(const Aff_transformationS3<FT>& t) const;

  bool            is_degenerate() const;
  Bbox_3          bbox() const;

// private:
  PointS3<FT>     e0;
  PointS3<FT>     e1;
};

template < class FT >
inline
bool
SegmentS3<FT>::operator==(const SegmentS3<FT>& s) const
{ return (source() == s.source())  && (target() == s.target()); }


template < class FT >
inline
bool
SegmentS3<FT>::operator!=(const SegmentS3<FT>& s) const
{ return !(*this == s); }


template < class FT >
PointS3<FT>  SegmentS3<FT>::start() const
{ return e0; }


template < class FT >
PointS3<FT>  SegmentS3<FT>::end() const
{ return e1; }


template < class FT >
PointS3<FT>  SegmentS3<FT>::source() const
{ return e0; }


template < class FT >
PointS3<FT>  SegmentS3<FT>::target() const
{ return e1; }


template < class FT >
inline
PointS3<FT>
SegmentS3<FT>::min() const
{
  return (lexicographically_xyz_smaller(source(),target())) ? source()
                                                            : target();
}


template < class FT >
inline
PointS3<FT>
SegmentS3<FT>::max() const
{
  return (lexicographically_xyz_smaller(source(),target())) ? target()
                                                            : source();
}


template < class FT >
inline
PointS3<FT>
SegmentS3<FT>::vertex(int i) const
{ return (i%2 == 0) ? source() : target(); }


template < class FT >
inline
PointS3<FT>
SegmentS3<FT>::point(int i) const
{ return (i%2 == 0) ? source() : target(); }


template < class FT >
inline
PointS3<FT>
SegmentS3<FT>::operator[](int i) const
{ return vertex(i); }

template < class FT >
inline
FT
SegmentS3<FT>::squared_length() const
{ return squared_distance(target(), source()); }


template < class FT >
inline
DirectionS3<FT>
SegmentS3<FT>::direction() const
{ return DirectionS3<FT>( target() - source() ); }


template < class FT >
inline
LineS3<FT>
SegmentS3<FT>::supporting_line() const
{ return LineS3<FT>(*this); }


template < class FT >
inline
SegmentS3<FT>
SegmentS3<FT>::opposite() const
{ return SegmentS3<FT>(target(), source()); }


template < class FT >
inline
SegmentS3<FT>
SegmentS3<FT>::transform(const Aff_transformationS3<FT>& t) const
{ return SegmentS3<FT>(t.transform(source()), t.transform(target())); }


template < class FT >
inline
bool
SegmentS3<FT>::is_degenerate() const
{ return source() == target(); }


template < class FT >
inline
Bbox_3
SegmentS3<FT>::bbox() const
{ return source().bbox() + target().bbox(); }


#ifndef CGAL_NO_OSTREAM_INSERT_SEGMENTS3
template < class FT >
std::ostream& operator<<(std::ostream& os, const SegmentS3<FT>& s)
{
    switch(os.iword(IO::mode)) {
    case IO::ASCII :
        return os << s.source() << ' ' << s.target();
    case IO::BINARY :
        return os << s.source() << s.target();
    default:
        return os << "SegmentS3(" << s.source() <<  ", " << s.target() << ")";
    }
}
#endif // CGAL_NO_OSTREAM_INSERT_SEGMENTS3

#ifndef CGAL_NO_ISTREAM_EXTRACT_SEGMENTS3
template < class FT >
std::istream& operator>>(std::istream& is, SegmentS3<FT>& s)
{
    PointS3<FT> p, q;

    is >> p >> q;

    s = SegmentS3<FT>(p, q);
    return is;
}
#endif // CGAL_NO_ISTREAM_EXTRACT_SEGMENTS3

template < class FT >
bool SegmentS3<FT>::has_on(const PointS3<FT>& p) const
{ return are_ordered_along_line(source(), p, target()); }

template < class FT >
inline
bool
SegmentS3<FT>::collinear_has_on(const PointS3<FT>& p) const
{ return collinear_are_ordered_along_line(source(), p, target()); }


CGAL_END_NAMESPACE

#endif

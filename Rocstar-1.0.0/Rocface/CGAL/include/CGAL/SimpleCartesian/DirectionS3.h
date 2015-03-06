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
// file          : include/CGAL/SimpleCartesian/DirectionS3.h
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

#ifndef CGAL_DIRECTIONS3_H
#define CGAL_DIRECTIONS3_H

#include <CGAL/SimpleCartesian/VectorS3.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
class DirectionS3
{
public:
                 DirectionS3() {}
                 DirectionS3(const VectorS3<FT>& v);
                 DirectionS3(const FT& x, const FT& y, const FT& z)
                  : e0(x), e1(y), e2(z) {}

  bool           operator==(const DirectionS3<FT>& d) const;
  bool           operator!=(const DirectionS3<FT>& d) const;

  VectorS3<FT>   to_vector() const;
  VectorS3<FT>   vector() const { return to_vector(); } 


  DirectionS3    transform(const Aff_transformationS3<FT>& t) const;

  DirectionS3    operator-() const;

  const FT&      delta(int i) const;
  const FT&      dx() const;
  const FT&      dy() const;
  const FT&      dz() const;

  const FT&      hdx() const;
  const FT&      hdy() const;
  const FT&      hdz() const;
  FT             hw() const;


// private:
  FT   e0;
  FT   e1;
  FT   e2;
};

template < class FT >
DirectionS3<FT>::DirectionS3(const VectorS3<FT>& v)
{
  e0 = v.e0; 
  e1 = v.e1; 
  e2 = v.e2;
}

template < class FT >
bool 
DirectionS3<FT>::operator==(const DirectionS3<FT>& d) const
{
  return  ( dx()*d.dy() == dy()*d.dx() )
        &&( dx()*d.dz() == dz()*d.dx() )
        &&( dy()*d.dz() == dz()*d.dy() )
        &&( CGAL_NTS sign( dx() ) == CGAL_NTS sign( d.dx() ) )
        &&( CGAL_NTS sign( dy() ) == CGAL_NTS sign( d.dy() ) )
        &&( CGAL_NTS sign( dz() ) == CGAL_NTS sign( d.dz() ) );
}

template < class FT >
inline 
bool  
DirectionS3<FT>::operator!=(const DirectionS3<FT>& d) const
{ return !(*this == d); }

template < class FT >
inline 
VectorS3<FT> 
DirectionS3<FT>::to_vector() const
{ return VectorS3<FT>(*this); }


template < class FT >
inline
DirectionS3<FT>
DirectionS3<FT>::transform(const Aff_transformationS3<FT>& t) const
{ return t.transform(*this); }


template < class FT >
inline 
DirectionS3<FT> 
DirectionS3<FT>::operator-() const
{ return DirectionS3<FT>(-dx(), -dy(), -dz()); }


template < class FT >
const FT&
DirectionS3<FT>::delta(int i) const
{
  CGAL_kernel_precondition( i >= 0 && i <= 2 );
  return (i==0) ? dx() :
         (i==1) ? dy() : dz() ;
}


template < class FT >
inline 
const FT&
DirectionS3<FT>::dx() const
{ return e0; }


template < class FT >
inline 
const FT&
DirectionS3<FT>::dy() const
{ return e1; }


template < class FT >
inline 
const FT&
DirectionS3<FT>::dz() const
{ return e2; } 

template < class FT >
inline 
const FT&
DirectionS3<FT>::hdx() const
{ return e0; }


template < class FT >
inline 
const FT&
DirectionS3<FT>::hdy() const
{ return e1; }


template < class FT >
inline 
const FT&
DirectionS3<FT>::hdz() const
{ return e2; }

template < class FT >
inline 
FT 
DirectionS3<FT>::hw() const
{ return FT(1); }



#ifndef CGAL_NO_OSTREAM_INSERT_DIRECTIONS3
template < class FT >
std::ostream& operator<<(std::ostream& os, const DirectionS3<FT>& d)
{
  VectorS3<FT> v = d.vector();
  switch(os.iword(IO::mode)) {
    case IO::ASCII :
        return os << v.x() << ' ' << v.y()  << ' ' << v.z();
    case IO::BINARY :
        write(os, v.x());
        write(os, v.y());
        write(os, v.z());
        return os;
    default:
        os << "DirectionS3(" << v.x() << ", " << v.y() << ", " << v.z() << ")";
        return os;
    }
}
#endif // CGAL_NO_OSTREAM_INSERT_DIRECTIONS3

#ifndef CGAL_NO_ISTREAM_EXTRACT_DIRECTIONS3
template < class FT >
std::istream& operator>>(std::istream& is, DirectionS3<FT>& p)
{
    FT x, y, z;
    switch(is.iword(IO::mode)) {
    case IO::ASCII :
        is >> x >> y >> z;
        break;
    case IO::BINARY :
        read(is, x);
        read(is, y);
        read(is, z);
        break;
    default:
        CGAL_kernel_assertion_msg(false,"Stream must be in ascii or binary mode"); 
        // throw ios_base::failure("Stream must be in ascii or binary mode");
        break;
    }
    p = DirectionS3<FT>(x, y, z);
    return is;
}
#endif // CGAL_NO_ISTREAM_EXTRACT_DIRECTIONS3



CGAL_END_NAMESPACE

#endif

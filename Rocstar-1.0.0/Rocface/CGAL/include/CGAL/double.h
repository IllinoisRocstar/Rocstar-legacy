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
// source        : Double.fw
// file          : include/CGAL/double.h
// package       : Number_types (3.4)
// revision      : 3.4
// revision_date : 13 Jul 2000 
// author(s)     : Geert-Jan Giezeman
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_DOUBLE_H
#define CGAL_DOUBLE_H 1

#include <CGAL/basic.h>
#ifndef CGAL_TAGS_H
#include <CGAL/tags.h>
#endif // CGAL_TAGS_H
#include <cmath>
#include <CGAL/IEEE_754_unions.h>
#ifdef __sgi
#include <fp_class.h>
#endif // __sgi

CGAL_BEGIN_NAMESPACE


inline
double
to_double(double d)
{ return d; }

inline
double
sqrt(double d)
{ return std::sqrt(d); }

inline
bool
is_integral (const double d)
{ return ceil(d) == d; }

inline
Number_tag
number_type_tag(double)
{ return Number_tag(); }

#ifdef OLD_FINITE_VALID
extern
bool
is_finite(double d);

extern
bool
is_valid(double d);

#else

#ifdef __sgi

// double
inline
bool is_finite(double d)
{
    switch (fp_class_d(d)) {
    case FP_POS_NORM:
    case FP_NEG_NORM:
    case FP_POS_ZERO:
    case FP_NEG_ZERO:
    case FP_POS_DENORM:
    case FP_NEG_DENORM:
        return true;
    case FP_SNAN:
    case FP_QNAN:
    case FP_POS_INF:
    case FP_NEG_INF:
        return false;
    }
    return false; // NOT REACHED
}

inline
bool is_valid(double d)
{
    switch (fp_class_d(d)) {
    case FP_POS_NORM:
    case FP_NEG_NORM:
    case FP_POS_ZERO:
    case FP_NEG_ZERO:
    case FP_POS_INF:
    case FP_NEG_INF:
    case FP_POS_DENORM:
    case FP_NEG_DENORM:
        return true;
    case FP_SNAN:
    case FP_QNAN:
        return false;
    }
    return false; // NOT REACHED
}

#else
#if defined(_MSC_VER) || defined(CGAL_MASK_FINITE_VALID) || defined(__BORLANDC__)

#define CGAL_EXPONENT_DOUBLE_MASK   0x7ff00000
#define CGAL_MANTISSA_DOUBLE_MASK   0x000fffff

inline
bool
is_finite_by_mask_double(unsigned int h)
{
  unsigned int e = h & CGAL_EXPONENT_DOUBLE_MASK;
  return ( ( e ^ CGAL_EXPONENT_DOUBLE_MASK ) != 0 );
}

inline
bool
is_nan_by_mask_double(unsigned int h, unsigned int l)
{
  if ( is_finite_by_mask_double(h) ) return false;
  return ( (( h & CGAL_MANTISSA_DOUBLE_MASK ) != 0) || (( l & 0xffffffff ) != 0));
}

inline
bool
is_finite( const double& dble)
{
  double d = dble;
  IEEE_754_double* p = reinterpret_cast<IEEE_754_double*>(&d);
  return is_finite_by_mask_double( p->c.H );
}

inline
bool
is_valid( const double& dble)
{
  double d = dble;
  IEEE_754_double* p = reinterpret_cast<IEEE_754_double*>(&d);
  return ! ( is_nan_by_mask_double( p->c.H, p->c.L ));
}


#else

inline
bool
is_valid(double d)
{ return (d == d); }

inline
bool
is_finite(double d)
{ return (d == d) && (is_valid(d-d)); }

#endif // MSC_VER || ...
#endif // __sgi

#endif // OLD_FINITE_VALID

inline
io_Operator
io_tag(double)
{ return io_Operator(); }

#ifndef CGAL_NO_NTS_NAMESPACE
namespace NTS {
#ifndef CGAL_NUMBER_UTILS_H
template <class NT> NT abs(const NT &x);
#endif // CGAL_NUMBER_UTILS_H

CGAL_TEMPLATE_NULL
inline
double
abs(const double& d)
{ return CGAL_CLIB_STD::fabs(d); }


} // namespace NTS
#else
#ifndef CGAL_NUMBER_UTILS_H
template <class NT> NT abs(const NT &x);
#endif // CGAL_NUMBER_UTILS_H

CGAL_TEMPLATE_NULL
inline
double
abs(const double& d)
{ return CGAL_CLIB_STD::fabs(d); }


#endif // CGAL_NO_NTS_NAMESPACE

CGAL_END_NAMESPACE

#endif // CGAL_DOUBLE_H

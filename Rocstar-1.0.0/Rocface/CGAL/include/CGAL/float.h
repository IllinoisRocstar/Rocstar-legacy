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
// source        : Float.fw
// file          : include/CGAL/float.h
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
 

#ifndef CGAL_FLOAT_H
#define CGAL_FLOAT_H 1

#include <CGAL/basic.h>
#ifndef CGAL_TAGS_H
#include <CGAL/tags.h>
#endif // CGAL_TAGS_H
#include <cmath>
#include <CGAL/IEEE_754_unions.h>
#ifdef __sgi
#include <fp_class.h>
#endif

CGAL_BEGIN_NAMESPACE

inline
double
to_double(float f)
{ return (double)f; }

inline
Number_tag
number_type_tag(float)
{ return Number_tag(); }

#ifdef OLD_FINITE_VALID
extern
bool
is_finite(float d);

extern
bool
is_valid(float d);

#else
#ifdef __sgi

inline
bool is_finite(float f)
{
    switch (fp_class_f(f)) {
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
bool is_valid(float d)
{
    switch (fp_class_f(d)) {
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

#define CGAL_EXPONENT_FLOAT_MASK   0x7f800000
#define CGAL_MANTISSA_FLOAT_MASK   0x007fffff

inline
bool
is_finite_by_mask_float(unsigned int u)
{
  unsigned int e = u & CGAL_EXPONENT_FLOAT_MASK;
  return ( (e ^ CGAL_EXPONENT_FLOAT_MASK) != 0);
}

inline
bool
is_nan_by_mask_float(unsigned int u)
{
  if ( is_finite_by_mask_float(u) ) return false;
  // unsigned int m = u & CGAL_MANTISSA_FLOAT_MASK;
  return ( (u & CGAL_MANTISSA_FLOAT_MASK) != 0);
}

inline
bool
is_finite( const float& flt)
{
  float f = flt;
  IEEE_754_float* p = reinterpret_cast<IEEE_754_float*>(&f);
  return is_finite_by_mask_float( p->c );
}

inline
bool
is_valid( const float& flt)
{
  float f = flt;
  IEEE_754_float* p = reinterpret_cast<IEEE_754_float*>(&f);
  return !is_nan_by_mask_float( p->c );
}



#else

inline
bool
is_valid(float d)
{ return (d == d); }

inline
bool
is_finite(float d)
{ return (d == d) && (is_valid(d-d)); }

#endif // MSC_VER || ...
#endif // __sgi

#endif // OLD_FINITE_VALID

inline
io_Operator
io_tag(float)
{ return io_Operator(); }

CGAL_END_NAMESPACE

#endif // CGAL_FLOAT_H

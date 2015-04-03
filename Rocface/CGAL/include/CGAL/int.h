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
// source        : Int.fw
// file          : include/CGAL/int.h
// package       : Number_types (3.4)
// revision      : 3.4
// revision_date : 13 Jul 2000 
// author(s)     : Stefan Schirra
//
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_INT_H
#define CGAL_INT_H

#ifndef CGAL_NUMBER_TYPE_TAGS_H
#include <CGAL/number_type_tags.h>
#endif // CGAL_NUMBER_TYPE_TAGS_H

// int

CGAL_BEGIN_NAMESPACE


inline
double
to_double(int i)
{ return (double)i; }

inline
Number_tag
number_type_tag(int)
{ return Number_tag(); }

inline
bool
is_finite(int)
{ return true; }

inline
bool
is_valid(int)
{ return true; }

inline
io_Operator
io_tag(int)
{ return io_Operator(); }

// long

inline
double
to_double(long int i)
{ return (double)i; }

inline
Number_tag
number_type_tag(long int)
{ return Number_tag(); }

inline
bool
is_finite(long int)
{ return true; }

inline
bool
is_valid(long int)
{ return true; }

inline
io_Operator
io_tag(long int)
{ return io_Operator(); }

// short

inline
double
to_double(short int i)
{ return (double)i; }

inline
Number_tag
number_type_tag(short int)
{ return Number_tag(); }

inline
bool
is_finite(short int)
{ return true; }

inline
bool
is_valid(short int)
{ return true; }

inline
io_Operator
io_tag(short int)
{ return io_Operator(); }

// long long

#ifdef LONG_LONG

inline
double
to_double(long long i)
{ return (double)i; }

inline
Number_tag
number_type_tag(long long)
{ return Number_tag(); }

inline
bool
is_finite(long long)
{ return true; }

inline
bool
is_valid(long long)
{ return true; }
#endif // LONG_LONG

// io_tags for unsigned types
inline
io_Operator
io_tag(unsigned char)
{ return io_Operator(); }

inline
io_Operator
io_tag(unsigned short)
{ return io_Operator(); }

inline
io_Operator
io_tag(unsigned int)
{ return io_Operator(); }

inline
io_Operator
io_tag(unsigned long)
{ return io_Operator(); }

CGAL_END_NAMESPACE


#endif // CGAL_INT_H

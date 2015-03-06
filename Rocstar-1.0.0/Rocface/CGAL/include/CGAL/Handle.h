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
// source        : Handle.fw
// file          : include/CGAL/Handle.h
// package       : Kernel_basic (3.14)
// revision      : 3.14
// revision_date : 15 Sep 2000 
// author(s)     :
//
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_HANDLE_H
#define CGAL_HANDLE_H
#include <CGAL/Handle_for.h>

CGAL_BEGIN_NAMESPACE

class Leda_like_rep
{
    friend class Leda_like_handle;
  protected:
             Leda_like_rep() { count = 1; }
    virtual ~Leda_like_rep() {}

    int      count;
};

class Leda_like_handle
{
  public:
             Leda_like_handle()
             { PTR = (Leda_like_rep*)NULL; }
             // { PTR = (Leda_like_rep*)0xefefefef; }

             Leda_like_handle(const Leda_like_handle& x)
             {
               CGAL_kernel_precondition( x.PTR != (Leda_like_rep*)NULL );
               PTR = x.PTR;
               PTR->count++;
             }


             ~Leda_like_handle()
             { if ( PTR && (--PTR->count == 0)) { delete PTR; } }

    Leda_like_handle&
    operator=(const Leda_like_handle& x)
    {
      CGAL_kernel_precondition( x.PTR != (Leda_like_rep*)NULL );
      x.PTR->count++;
      if ( PTR && (--PTR->count == 0)) { delete PTR; }
      PTR = x.PTR;
      return *this;
    }


    int
    refs()  const { return PTR->count; }

    friend unsigned long id(const Leda_like_handle& x);

  protected:
    Leda_like_rep* PTR;
};

inline
unsigned long
id(const Leda_like_handle& x)
{ return (unsigned long)x.PTR; }

template < class T >
inline
bool
identical(const T &t1, const T &t2)
{ return id(t1) == id(t2); }
CGAL_END_NAMESPACE


#if defined(CGAL_USE_LEDA) && !defined(CGAL_NO_LEDA_HANDLE)
#  include <LEDA/basic.h>

CGAL_BEGIN_NAMESPACE

typedef handle_base      Handle;
typedef handle_rep       Rep;

inline
unsigned long
id(const Handle& x)
{ return ID_Number(x); }
CGAL_END_NAMESPACE


#  else

CGAL_BEGIN_NAMESPACE

typedef Leda_like_handle Handle;
typedef Leda_like_rep    Rep;
CGAL_END_NAMESPACE


#endif // defined(CGAL_USE_LEDA) && !defined(CGAL_NO_LEDA_HANDLE)
#endif // CGAL_HANDLE_H

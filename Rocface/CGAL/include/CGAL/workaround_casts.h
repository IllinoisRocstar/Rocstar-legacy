// ======================================================================
//
// Copyright (c) 1997,1998 The CGAL Consortium

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
// file          : include/CGAL/workaround_casts.h
// package       : Configuration (2.4)
// chapter       : $CGAL_Chapter: Configuration $
//
// source        : web/workarounds.aw
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:48 $
// author(s)     : Sven Schönherr
//
// coordinator   : Utrecht University (Wieger Wesselink)
//
// implementation: Workarounds for C++-style casts
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_WORKAROUND_CASTS_H
#define CGAL_WORKAROUND_CASTS_H 1


// workaround for C++-style casts

#if defined( CGAL_CFG_NO_DYNAMIC_CAST)
#  define  CGAL_dynamic_cast(type,expr)  (type)( expr)
#else
#  define  CGAL_dynamic_cast(type,expr)  dynamic_cast< type >(expr)
#endif // CGAL_CFG_NO_DYNAMIC_CAST

#endif // CGAL_WORKAROUND_CASTS_H

// ===== EOF ==================================================================

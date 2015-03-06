// ======================================================================
//
// Copyright (c) 1997 The CGAL Consortium

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
//
// release       : CGAL-2.2
// release_date  : 2000, September 30
//
// file          : src/io.C
// package       : iostream (2.8)
// source        : $RCSfile: io.C,v $
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:49 $
// author(s)     : Andreas Fabri
//
// coordinator   : Mariette.Yvinec
//
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================


#ifndef CGAL_IO_C
#define CGAL_IO_C

#include <CGAL/basic.h>
#include <CGAL/IO/io.h>

CGAL_BEGIN_NAMESPACE

int IO::mode = std::ios::xalloc();


IO::Mode
get_mode(std::ios& i)
{
    return static_cast<IO::Mode>(i.iword(IO::mode));
}

IO::Mode
set_ascii_mode(std::ios& i)
{
    IO::Mode m = get_mode(i);
    i.iword(IO::mode) = IO::ASCII;
    return m;
}


IO::Mode
set_binary_mode(std::ios& i)
{
    IO::Mode m = get_mode(i);
    i.iword(IO::mode) = IO::BINARY;
    return m;
}


IO::Mode
set_pretty_mode(std::ios& i)
{
    IO::Mode m = get_mode(i);
    i.iword(IO::mode) = IO::PRETTY;
    return m;
}


IO::Mode
set_mode(std::ios& i, IO::Mode m)
{
    IO::Mode old = get_mode(i);
    i.iword(IO::mode) = m;
    return old;
}

bool
is_pretty(std::ios& i)
{
    return i.iword(IO::mode) == IO::PRETTY;
}

bool
is_ascii(std::ios& i)
{
    return i.iword(IO::mode) == IO::ASCII;
}


bool
is_binary(std::ios& i)
{
    return i.iword(IO::mode) == IO::BINARY;
}

CGAL_END_NAMESPACE

#endif // CGAL_IO_C

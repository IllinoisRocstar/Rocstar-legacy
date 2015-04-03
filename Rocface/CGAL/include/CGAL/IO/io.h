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
// release       : CGAL-2.2
// release_date  : 2000, September 30
//
// file          : include/CGAL/IO/io.h
// package       : iostream (2.8)
// source        : $RCSfile: io.h,v $
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:48 $
// author(s)     : Andreas Fabri
//
// coordinator   : Mariette Yvinec
//
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================


#ifndef CGAL_IO_H
#define CGAL_IO_H

#include <iostream>
#include <CGAL/IO/io_tags.h>
#include <CGAL/IO/Color.h>
#include <CGAL/Object.h>

CGAL_BEGIN_NAMESPACE

class IO {
public:
    static int mode;
    enum Mode {ASCII = 0, PRETTY, BINARY};
};

IO::Mode
get_mode(std::ios& i);

IO::Mode
set_ascii_mode(std::ios& i);

IO::Mode
set_binary_mode(std::ios& i);

IO::Mode
set_pretty_mode(std::ios& i);

IO::Mode
set_mode(std::ios& i, IO::Mode m);
bool
is_pretty(std::ios& i);

bool
is_ascii(std::ios& i);

bool
is_binary(std::ios& i);

inline io_Read_write io_tag(char){ return io_Read_write(); }


template < class T >
inline
void
write(std::ostream& os, const T& t, const io_Read_write&)
{
    os.write((char*)&t, sizeof(t));
}


template < class T >
inline
void
write(std::ostream& os, const T& t, const io_Operator&)
{
    os << t;
}


template < class T >
inline
void
write(std::ostream& os, const T& t, const io_Extract_insert&)
{
    insert(os, t);
}


template < class T >
inline
void
write(std::ostream& os, const T& t)
{
    write(os, t, io_tag(t));
}


template < class T >
inline
void
read(std::istream& is, T& t, const io_Read_write&)
{
    is.read((char*)&t, sizeof(t));
}


template < class T >
inline
void
read(std::istream& is, T& t, const io_Operator&)
{
    is >> t;
}


template < class T >
inline
void
read(std::istream& is, T& t, const io_Extract_insert&)
{
    extract(is, t);
}


template < class T >
inline
void
read(std::istream& is, T& t)
{
    read(is, t, io_tag(t));
}


inline
std::ostream& operator<<( std::ostream& out, const Color& col)
{
    switch(out.iword(IO::mode)) {
    case IO::ASCII :
        return out << static_cast<int>(col.red())   << ' ' 
		   << static_cast<int>(col.green()) << ' ' 
		   << static_cast<int>(col.blue());
    case IO::BINARY :
        write(out, static_cast<int>(col.red()));
        write(out, static_cast<int>(col.green()));
        write(out, static_cast<int>(col.blue()));
        return out;
    default:
        return out << "Color(" << static_cast<int>(col.red()) << ", " 
		   << static_cast<int>(col.green()) << ", "
                   << static_cast<int>(col.blue()) << ')';
    }
}

inline
std::istream &operator>>(std::istream &is, Color& col)
{
    int r, g, b;
    switch(is.iword(IO::mode)) {
    case IO::ASCII :
        is >> r >> g >> b;
        break;
    case IO::BINARY :
        read(is, r);
        read(is, g);
        read(is, b);
        break;
    default:
        std::cerr << "" << std::endl;
        std::cerr << "Stream must be in ascii or binary mode" << std::endl;
        break;
    }
    col = Color(r,g,b);
    return is;
}

CGAL_END_NAMESPACE

#endif // CGAL_IO_H

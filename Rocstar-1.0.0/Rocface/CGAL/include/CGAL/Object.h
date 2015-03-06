// ======================================================================
//
// Copyright (c) 2000 The CGAL Consortium

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
// file          : include/CGAL/Object.h
// package       : Kernel_basic (3.14)
// source        : Object.lw 
// revision      : 3.0
// revision_date : 02 Feb 2000
// author(s)     : Stefan.Schirra
//                 Andreas Fabri
//                 Geert-Jan Giezeman
//                 Michael Seel
//
// coordinator   : MPI Saarbruecken, Germany 
//
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_OBJECT_H
#define CGAL_OBJECT_H

#ifdef CGAL_CFG_NO_DYNAMIC_CAST
#error fatal error: dynamic cast not supported
#endif // CGAL_CFG_NO_DYNAMIC_CAST

#include <CGAL/Handle_for.h>

namespace CGAL {

class Object;
class Object_base;
template <class T> class Wrapper;


class Object_base : public Ref_counted  
{
  public:
    virtual   ~Object_base() {}
};


template <class T>
class Wrapper : public Object_base 
{
  public:
    Wrapper(const T& object) : _object(object) {}

    Wrapper() {}

    operator T() { return _object; }

    virtual   ~Wrapper() {}

  private:
    T         _object;
};


class Object
{
  public:
    Object() : ptr( static_cast<Object_base*>(0) ) {}

    Object(Object_base *base) 
    { 
      ptr = base; 
      CGAL_kernel_assertion( !ptr || (ptr->count == 1));
    }

    Object(const Object& o) : ptr(o.ptr)
    { if (ptr) ptr->count++; }

    ~Object()
    { if (ptr && (--ptr->count == 0)) { delete ptr; } }

    Object&       
    operator=(const Object& o)
    {
      if (o.ptr) o.ptr->count++;
      if (ptr && (--ptr->count == 0)) { delete ptr; }
      ptr = o.ptr;
      return *this;
    }

    bool          
    is_empty() const { return ptr == static_cast<Object_base*>(0); }

    template <class T>
    friend bool assign(T& t, const Object& o);

  protected:
    Object_base*  ptr;
};


template <class T>
Object
make_object(const T& t)
{ return Object(new Wrapper< T >(t)); }


template <class T>
bool
assign(T& t, const Object& o)
{
# ifdef CGAL_CFG_DYNAMIC_CAST_BUG
  Wrapper<T>   instantiate_it;
# endif // CGAL_CFG_DYNAMIC_CAST_BUG
  Wrapper<T>*  wp = dynamic_cast<Wrapper<T>*>(o.ptr);
  if ( wp == static_cast<Wrapper<T>*>(0) ) { return false; }
  t = *(wp);
  return true;
}

} // namespace CGAL

#endif // CGAL_OBJECT_H


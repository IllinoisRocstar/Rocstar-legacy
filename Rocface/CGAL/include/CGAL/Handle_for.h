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
// source        : Handle_for.fw
// file          : include/CGAL/Handle_for.h
// package       : Kernel_basic (3.14)
// revision      : 3.14
// revision_date : 15 Sep 2000 
// author(s)     : Stefan Schirra
//
//
// coordinator   : MPI, Saarbruecken  (<Stefan.Schirra>)
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================
 

#ifndef CGAL_HANDLE_FOR_H
#define CGAL_HANDLE_FOR_H
#include <CGAL/memory.h>

namespace CGAL {


template <class RefCounted, class Allocator> class Handle_for;
class Object;


class Ref_counted
{
  public:
    Ref_counted() : count(1) {}
    Ref_counted(const Ref_counted&) : count(1) {}

    void  add_reference() { ++count; }
    void  remove_reference() { --count; }
    bool  is_referenced() { return (count != 0); }
    bool  is_shared() { return (count > 1); }

  friend class Object;

  protected:
    unsigned int count;
};


template <class RefCounted,
          class Allocator = CGAL_ALLOCATOR(RefCounted) >
// RefCounted must provide
// add_reference()
// remove_reference()
// bool is_referenced()
// bool is_shared()
// and initialize count to 1 in default and copy constructor
class Handle_for
{
  public:
    Handle_for(const RefCounted& rc)
    {
      ptr = allocator.allocate(1);
      allocator.construct(ptr, rc);
    }

    Handle_for()
    {
      ptr = allocator.allocate(1);
    }

    Handle_for( const Handle_for& h)
    {
      ptr = h.ptr;
      ptr->add_reference();
    }

    ~Handle_for()
    {
      ptr->remove_reference();
      if ( !ptr->is_referenced() )
      {
        allocator.destroy( ptr);
        allocator.deallocate( ptr, 1);
      }
    }

    Handle_for&
    operator=( const Handle_for& h)
    {
      h.ptr->add_reference();
      ptr->remove_reference();
      if ( !ptr->is_referenced() )
      {
        allocator.destroy( ptr);
        allocator.deallocate( ptr, 1);
      }
      ptr = h.ptr;
      return *this;
    }

    void
    initialize_with( const RefCounted& rc)
    {
      allocator.construct(ptr, rc);
    }

    void
    copy_on_write()
    {
      if ( ptr->is_shared() )
      {
        RefCounted* tmp_ptr = allocator.allocate(1);
        allocator.construct( tmp_ptr, *ptr);
        ptr->remove_reference();
        ptr = tmp_ptr;
      }
    }

    bool
    identical( const Handle_for& h) const
    { return ptr == h.ptr; }

    long int
    id() const
    { return reinterpret_cast<long int>( &(*ptr)); }

  protected:
    static Allocator allocator;
    typename Allocator::pointer      ptr;
};


template <class RefCounted, class Allocator>
Allocator  Handle_for<RefCounted,Allocator>::allocator;

} // namespace CGAL
#endif // CGAL_HANDLE_FOR_H

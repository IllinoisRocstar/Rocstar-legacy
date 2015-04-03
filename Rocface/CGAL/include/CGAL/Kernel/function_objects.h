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
// source        : for_function_objects.lw
// file          : include/CGAL/Kernel/function_objects.h
// package       : Kernel_basic (3.14)
// revision      : 3.13
// revision_date : 10 Aug 2000
// author(s)     : Stefan Schirra
//
// coordinator   : MPI, Saarbruecken
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_KERNEL_FUNCTION_OBJECTS_H
#define CGAL_KERNEL_FUNCTION_OBJECTS_H

CGAL_BEGIN_NAMESPACE
namespace CGALi {

template <class ToBeConstructed>
class Construct
{
  public:
    typedef ToBeConstructed  result_type;

    ToBeConstructed
    operator()() const
    { return ToBeConstructed(); }

    template <class A1> 
    ToBeConstructed
    operator()( const A1& a1) const
    { return ToBeConstructed(a1); }

    template <class A1, class A2> 
    ToBeConstructed
    operator()( const A1& a1, const A2& a2) const
    { return ToBeConstructed(a1,a2); }

    template <class A1, class A2, class A3> 
    ToBeConstructed
    operator()( const A1& a1, const A2& a2, const A3& a3) const
    { return ToBeConstructed(a1,a2,a3); }

    template <class A1, class A2, class A3, class A4> 
    ToBeConstructed
    operator()( const A1& a1, const A2& a2, const A3& a3, const A4& a4) const
    { return ToBeConstructed(a1,a2,a3,a4); }

    template <class A1, class A2, class A3, class A4, class A5> 
    ToBeConstructed
    operator()( const A1& a1, const A2& a2, const A3& a3, const A4& a4, const A5& a5) const
    { return ToBeConstructed(a1,a2,a3,a4,a5); }

    template <class A> 
    ToBeConstructed
    operator()( const A& a1, const A& a2, const A& a3,
                const A& a4, const A& a5, const A& a6 )
    { return ToBeConstructed(a1,a2,a3,a4,a5,a6); }

    template <class A> 
    ToBeConstructed
    operator()( const A& a1, const A& a2, const A& a3,
                const A& a4, const A& a5, const A& a6,
                const A& a7 )
    { return ToBeConstructed(a1,a2,a3,a4,a5,a6,a7); }

    template <class A> 
    ToBeConstructed
    operator()( const A& a1, const A& a2, const A& a3,
                const A& a4, const A& a5, const A& a6,
                const A& a7, const A& a8, const A& a9)
    { return ToBeConstructed(a1,a2,a3,a4,a5,a6,a7,a8,a9); }

    template <class A> 
    ToBeConstructed
    operator()( const A& a1, const A& a2, const A& a3,
                const A& a4, const A& a5, const A& a6,
                const A& a7, const A& a8, const A& a9,
                const A& a10)
    { return ToBeConstructed(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10); }

    template <class A> 
    ToBeConstructed
    operator()( const A& a1, const A& a2, const A& a3,
                const A& a4, const A& a5, const A& a6,
                const A& a7, const A& a8, const A& a9,
                const A& a10,const A& a11,const A& a12)
    { return ToBeConstructed(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12); }

    template <class A> 
    ToBeConstructed
    operator()( const A& a1, const A& a2, const A& a3,
                const A& a4, const A& a5, const A& a6,
                const A& a7, const A& a8, const A& a9,
                const A& a10,const A& a11,const A& a12,
                const A& a13)
    { return ToBeConstructed(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13); }

};


template <class ReturnType>
class Call_point_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.point(); }

    template <class Cls>
    ReturnType
    operator()( const Cls& c, int i) const
    { return c.point(i); }
};
template <class ReturnType>
class Call_second_point_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.second_point(); }
};
template <class ReturnType>
class Call_perpendicular_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.perpendicular(); }

    template <class Cls, class A1>
    ReturnType
    operator()( const Cls& c, const A1& a1) const
    { return c.perpendicular(a1); }
};

template <class ReturnType>
class Call_perpendicular_plane_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls, class A1>
    ReturnType
    operator()( const Cls& c, const A1& a1) const
    { return c.perpendicular_plane(a1); }
};

template <class Point>
class p_Midpoint
{
  public:
    typedef Point          result_type;

    Point
    operator()(const Point& p, const Point& q) const { return midpoint(p,q); }
};
template <class Point>
class p_Circumcenter
{
  public:
    typedef Point          result_type;

    Point
    operator()(const Point& p, const Point& q, const Point& r) const
    { return circumcenter(p,q,r); }

    Point
    operator()(const Point& p, const Point& q, 
               const Point& r, const Point& s) const
    { return circumcenter(p,q,r,s); }
};
template <class Point, class Line>
class pl_Bisector
{
  public:
    typedef Line           result_type;

    Line
    operator()(const Point& p, const Point& q) const { return bisector(p,q); }
};
class Intersect
{
  public:
    typedef CGAL::Object   result_type;

    template <class T1, class T2>
    CGAL::Object
    operator()(const T1& t1, const T2& t2) const
    { return intersection( t1, t2); }
};
class Assign
{
  public:
    typedef bool           result_type;

    template <class T1>
    bool
    operator()(T1& t1, const CGAL::Object& o) const
    { return assign( t1, o); }
};
template <class ReturnType>
class Call_y_at_x_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c, const ReturnType& x) const
    { return c.y_at_x(x); }
};
template <class ReturnType>
class Call_x_at_y_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c, const ReturnType& x) const
    { return c.x_at_y(x); }
};
template <class ReturnType>
class Call_squared_length_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.squared_length(); }
};
class Counterclockwise_in_between
{
  public:
    typedef bool           result_type;

    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r) const
    { return p.counterclockwise_in_between(q,r); }
};


class Collinear
{
  public:
    typedef bool           result_type;

    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r) const
    { return collinear(p,q,r); }
};

class Coplanar
{
  public:
    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r, const T& s) const
    { return coplanar(p,q,r,s); }
};

class Side_of_oriented_circle
{
  public:
    typedef Oriented_side  result_type;

    template <class T>
    Oriented_side
    operator()(const T& p, const T& q, const T& r, const T& t) const
    { return side_of_oriented_circle(p,q,r,t); }
};

class Side_of_bounded_circle
{
  public:
    typedef Bounded_side   result_type;

    template <class T>
    Bounded_side
    operator()(const T& p, const T& q, const T& r, const T& t) const
    { return side_of_bounded_circle(p,q,r,t); }
};
class Side_of_oriented_sphere
{
  public:
    typedef Oriented_side  result_type;

    template <class T>
    Oriented_side
    operator()(const T& p, const T& q, const T& r, const T& s, const T& t) const
    { return side_of_oriented_sphere(p,q,r,s,t); }
};

class Side_of_bounded_sphere
{
  public:
    typedef Bounded_side   result_type;

    template <class T>
    Bounded_side
    operator()(const T& p, const T& q, const T& r, const T& s, const T& t) const
    { return side_of_bounded_sphere(p,q,r,s,t); }
};
class Call_is_horizontal
{
  public:
    typedef bool           result_type;

    template <class Cls>
    bool
    operator()( const Cls& c) const
    { return c.is_horizontal(); }
};

class Call_is_vertical
{
  public:
    typedef bool           result_type;

    template <class Cls>
    bool
    operator()( const Cls& c) const
    { return c.is_vertical(); }
};
class Call_is_degenerate
{
  public:
    typedef bool           result_type;

    template <class Cls>
    bool
    operator()( const Cls& c) const
    { return c.is_degenerate(); }
};
class Call_has_on_bounded_side
{
  public:
    typedef bool           result_type;

    template <class Cls, class Arg>
    bool
    operator()( const Cls& c, const Arg& a) const
    { return c.has_on_bounded_side(a); }
};

class Call_has_on_unbounded_side
{
  public:
    typedef bool           result_type;

    template <class Cls, class Arg>
    bool
    operator()( const Cls& c, const Arg& a) const
    { return c.has_on_unbounded_side(a); }
};

class Call_has_on_boundary
{
  public:
    typedef bool           result_type;

    template <class Cls, class Arg>
    bool
    operator()( const Cls& c, const Arg& a) const
    { return c.has_on_boundary(a); }
};

class Call_has_on_positive_side
{
  public:
    typedef bool           result_type;

    template <class Cls, class Arg>
    bool
    operator()( const Cls& c, const Arg& a) const
    { return c.has_on_positive_side(a); }
};

class Call_has_on_negative_side
{
  public:
    typedef bool           result_type;

    template <class Cls, class Arg>
    bool
    operator()( const Cls& c, const Arg& a) const
    { return c.has_on_negative_side(a); }
};

class Call_oriented_side
{
  public:
    typedef bool           result_type;

    template <class Cls, class Arg>
    Oriented_side
    operator()( const Cls& c, const Arg& a) const
    { return c.oriented_side(a); }
};
class Less_x
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return less_x(a1,a2); }
};

class Less_y
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return less_y(a1,a2); }
};

class Less_z
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return less_y(a1,a2); }
};

class Less_xy
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return lexicographically_xy_smaller(a1,a2); }
};

class Less_yx
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return lexicographically_yx_smaller(a1,a2); }
};

class Less_xyz
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return lexicographically_xyz_smaller(a1,a2); }
};

class Equal
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()(const T1& p, const T2& q) const
    { return p == q; }
};

class Equal_x
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return x_equal(a1,a2); }
};

class Equal_y
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return y_equal(a1,a2); }
};

class Equal_z
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return z_equal(a1,a2); }
};

class Equal_xy
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return equal_xy(a1,a2); }
};

class Equal_xyz
{
  public:
    typedef bool           result_type;

    template <class T1, class T2>
    bool
    operator()( const T1& a1, const T2& a2) const
    { return equal_xyz(a1,a2); }
};
class Compare_x
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_x(a1,a2); }

    template <class T1, class T2, class T3>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3) const
    { return compare_x(a1,a2,a3); }

    template <class T1, class T2, class T3, class T4>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3, const T4& a4) const
    { return compare_x(a1,a2,a3,a4); }

};

class Compare_y
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_y(a1,a2); }

    template <class T1, class T2, class T3>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3) const
    { return compare_y(a1,a2,a3); }

    template <class T1, class T2, class T3, class T4>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3, const T4& a4) const
    { return compare_y(a1,a2,a3,a4); }


};

class Compare_z
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_z(a1,a2); }
};

class Compare_xy
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_lexicographically_xy(a1,a2); }
};

class Compare_xyz
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_lexicographically_xyz(a1,a2); }
};

class Compare_y_at_x
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_y_at_x(a1,a2); }

    template <class T1, class T2, class T3>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3) const
    { return compare_y_at_x(a1,a2,a3); }
    
    template <class T1, class T2, class T3, class T4>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3, const T4& a4) const
    { return compare_y_at_x(a1,a2,a3,a4); }
};

class Compare_x_at_y
{
  public:
    typedef Comparison_result result_type;

    template <class T1, class T2>
    Comparison_result
    operator()( const T1& a1, const T2& a2) const
    { return compare_x_at_y(a1,a2); }

    template <class T1, class T2, class T3>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3) const
    { return compare_x_at_y(a1,a2,a3); }
    
    template <class T1, class T2, class T3, class T4>
    Comparison_result
    operator()( const T1& a1, const T2& a2, const T3& a3, const T4& a4) const
    { return compare_x_at_y(a1,a2,a3,a4); }
};

class Are_ordered_along_line
{
  public:
    typedef bool           result_type;

    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r) const
    { return are_ordered_along_line(p,q,r); }
};

class Are_strictly_ordered_along_line
{
  public:
    typedef bool           result_type;

    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r) const
    { return are_strictly_ordered_along_line(p,q,r); }
};

class Collinear_are_ordered_along_line
{
  public:
    typedef bool           result_type;

    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r) const
    { return collinear_are_ordered_along_line(p,q,r); }
};

class Collinear_are_strictly_ordered_along_line
{
  public:
    typedef bool           result_type;

    template <class T>
    bool
    operator()(const T& p, const T& q, const T& r) const
    { return collinear_are_strictly_ordered_along_line(p,q,r); }
};
class Call_transform
{
 public:
    template <class Transformation, class ArgumentType>
    ArgumentType
    operator()( const ArgumentType& a, const Transformation& t)
    { return a.transform(t); }
};
template <class ReturnType>
class Call_source_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.source(); }
};

template <class ReturnType>
class Call_target_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.target(); }
};

template <class ReturnType>
class Call_min_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.min(); }
};

template <class ReturnType>
class Call_max_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.max(); }
};

template <class ReturnType>
class Call_direction_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.direction(); }
};

template <class ReturnType>
class Call_supporting_line_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.supporting_line(); }
};

template <class ReturnType>
class Call_supporting_plane_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.supporting_plane(); }
};

template <class ReturnType>
class Call_opposite_to_get
{
  public:
    typedef ReturnType     result_type;

    template <class Cls>
    ReturnType
    operator()( const Cls& c) const
    { return c.opposite(); }
};

class Call_has_on
{
  public:
    typedef bool           result_type;

    template <class Cls, class A1>
    bool
    operator()( const Cls& c, const A1& a1) const
    { return c.has_on(a1); }
};

class Call_collinear_has_on
{
  public:
    typedef bool           result_type;

    template <class Cls, class A1>
    bool
    operator()( const Cls& c, const A1& a1) const
    { return c.collinear_has_on(a1); }
};



} // end namespace CGALi
CGAL_END_NAMESPACE

#endif // CGAL_KERNEL_FUNCTION_OBJECTS_H

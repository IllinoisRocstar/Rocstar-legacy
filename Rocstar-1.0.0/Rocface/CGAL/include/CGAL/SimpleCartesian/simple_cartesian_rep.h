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
// source        : Simple_cartesian.lw
// file          : include/CGAL/SimpleCartesian/simple_cartesian_rep.h
// package       : S2 (1.7)
// revision      : 1.7
// revision_date : 11 Aug 2000
// author(s)     : Stefan Schirra
//
// coordinator   : MPI, Saarbruecken
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_SIMPLE_CARTESIAN_REP_H
#define CGAL_SIMPLE_CARTESIAN_REP_H

#define CGAL_REP_CLASS_DEFINED

#include <CGAL/SimpleCartesian/simple_cartesian_classes.h>
#include <CGAL/predicate_objects_on_points_2.h>
#include <CGAL/Kernel/function_objects.h>

CGAL_BEGIN_NAMESPACE
template < class FT_ >
class Data_accessorS2
{
public:
    typedef  FT_          FT;
    typedef  PointS2<FT>  Point;

    FT  get_x( Point const& p) const { return( p.x()); }
    FT  get_y( Point const& p) const { return( p.y()); }

    void
    get( Point const& p, FT& x, FT& y) const
    {
        x = get_x( p);
        y = get_y( p);
    }

    void
    set( Point& p, FT const& x, FT const& y) const
    {
        p = Point( x, y);
    }
};
template <class R_, class FT_>
class Simple_cartesian_base
{
  public:
    typedef FT_                RT;
    typedef FT_                FT;
    typedef CGAL::Object                            Object_2;
    // we have: template <class R> CGAL::Point_2 : public R::Point_2_base
    typedef CGAL::Point_2< R_ >                     Point_2;
    typedef CGAL::Vector_2< R_ >                    Vector_2;
    typedef CGAL::Direction_2< R_ >                 Direction_2;
    typedef CGAL::Segment_2< R_ >                   Segment_2;
    typedef CGAL::Line_2< R_ >                      Line_2;
    typedef CGAL::Ray_2< R_ >                       Ray_2;
    typedef CGAL::Circle_2< R_ >                    Circle_2;
    typedef CGAL::Triangle_2< R_ >                  Triangle_2;
    typedef CGAL::Iso_rectangle_2< R_ >             Iso_rectangle_2;
    typedef CGAL::Aff_transformation_2< R_ >        Aff_transformation_2;
    typedef CGAL::Object                               Object_3;
    typedef CGAL::Point_3< R_ >                        Point_3;
    typedef CGAL::Vector_3< R_ >                       Vector_3;
    typedef CGAL::Direction_3< R_ >                    Direction_3;
    typedef CGAL::Segment_3< R_ >                      Segment_3;
    typedef CGAL::Plane_3< R_ >                        Plane_3;
    typedef CGAL::Line_3< R_ >                         Line_3;
    typedef CGAL::Ray_3< R_ >                          Ray_3;
    typedef CGAL::Triangle_3< R_ >                     Triangle_3;
    typedef CGAL::Tetrahedron_3< R_ >                  Tetrahedron_3;
    typedef CGAL::Iso_cuboid_3< R_ >                   Iso_cuboid_3;
    typedef CGAL::Sphere_3< R_ >                       Sphere_3;
    typedef CGAL::Aff_transformation_3< R_ >           Aff_transformation_3;
    // we have: template <class R> CGAL::Point_d : public R::Point_d_base
    typedef CGAL::Point_d< R_ >                    Point_d;

};
template <class FT_>
class Simple_cartesian 
 : public Simple_cartesian_base< Simple_cartesian<FT_>, FT_ >
{
  public:
    typedef FT_                RT;
    typedef FT_                FT;
    typedef PointS2< FT>                        Point_2_base;        
    typedef VectorS2< FT>                       Vector_2_base;        
    typedef DirectionS2< FT>                    Direction_2_base;        
    typedef SegmentS2< FT>                      Segment_2_base;        
    typedef LineS2< FT>                         Line_2_base;        
    typedef RayS2< FT>                          Ray_2_base;        
    typedef CircleS2< FT>                       Circle_2_base;        
    typedef TriangleS2< FT>                     Triangle_2_base;        
    typedef Iso_rectangleS2< FT>                Iso_rectangle_2_base;        
    typedef Aff_transformationS2< FT>           Aff_transformation_2_base;        
    
    typedef Simple_cartesian_base< Simple_cartesian<FT_>, FT_ >   KernelBase;

    typedef typename KernelBase::Point_2               Point_2;
    typedef typename KernelBase::Vector_2              Vector_2;
    typedef typename KernelBase::Direction_2           Direction_2;
    typedef typename KernelBase::Line_2                Line_2;
    typedef typename KernelBase::Segment_2             Segment_2;
    typedef typename KernelBase::Ray_2                 Ray_2;
    typedef typename KernelBase::Circle_2              Circle_2;
    typedef typename KernelBase::Triangle_2            Triangle_2;
    typedef typename KernelBase::Iso_rectangle_2       Iso_rectangle_2;
    typedef typename KernelBase::Aff_transformation_2  Aff_transformation_2;

    typedef CGALi::Construct<Point_2>                  Construct_point_2;
    typedef CGALi::Construct<Vector_2>                 Construct_vector_2;
    typedef CGALi::Construct<Direction_2>              Construct_direction_2;
    typedef CGALi::Construct<Segment_2>                Construct_segment_2;
    typedef CGALi::Construct<Line_2>                   Construct_line_2;
    typedef CGALi::Construct<Ray_2>                    Construct_ray_2;
    typedef CGALi::Construct<Circle_2>                 Construct_circle_2;
    typedef CGALi::Construct<Triangle_2>               Construct_triangle_2;
    typedef CGALi::Construct<Aff_transformation_2>     Construct_aff_transformation_2;

    Construct_point_2 
    construct_point_2_object() const 
    { return Construct_point_2(); }

    Construct_vector_2
    construct_vector_2_object() const 
    { return Construct_vector_2(); }

    Construct_direction_2
    construct_direction_2_object() const 
    { return Construct_direction_2(); }

    Construct_segment_2
    construct_segment_2_object() const 
    { return Construct_segment_2(); }

    Construct_line_2
    construct_line_2_object() const 
    { return Construct_line_2(); }

    Construct_ray_2
    construct_ray_2_object() const 
    { return Construct_ray_2(); }

    Construct_circle_2
    construct_circle_2_object() const 
    { return Construct_circle_2(); }

    Construct_triangle_2
    construct_triangle_2_object() const 
    { return Construct_triangle_2(); }

    Construct_aff_transformation_2
    construct_aff_transformation_2_object() const 
    { return Construct_aff_transformation_2(); }

    
    typedef CGALi::Call_point_to_get<Point_2>              Construct_point_on_2;
    Construct_point_on_2
    construct_point_on_2_object() const 
    { return Construct_point_on_2(); }

    typedef CGALi::Call_second_point_to_get<Point_2>       Construct_second_point_on_2;
    Construct_second_point_on_2
    construct_second_point_on_2_object() const 
    { return Construct_second_point_on_2(); }

    typedef CGALi::Call_source_to_get<Point_2>             Construct_source_point_2;
    Construct_source_point_2
    construct_source_point_2_object() const 
    { return Construct_source_point_2(); }

    typedef CGALi::Call_target_to_get<Point_2>             Construct_target_point_2;
    Construct_target_point_2
    construct_target_point_2_object() const 
    { return Construct_target_point_2(); }

    typedef CGALi::Call_min_to_get<Point_2>                Construct_min_point_2;
    Construct_min_point_2
    construct_min_point_2_object() const 
    { return Construct_min_point_2(); }

    typedef CGALi::Call_max_to_get<Point_2>                Construct_max_point_2;
    Construct_max_point_2
    construct_max_point_2_object() const 
    { return Construct_max_point_2(); }

    typedef CGALi::Call_direction_to_get<Direction_2>      Construct_direction_of_line_2;
    Construct_direction_of_line_2
    construct_direction_of_line_2_object() const 
    { return Construct_direction_of_line_2(); }

    typedef CGALi::Call_direction_to_get<Direction_2>      Construct_direction_of_ray_2;
    Construct_direction_of_ray_2
    construct_direction_of_ray_2_object() const 
    { return Construct_direction_of_ray_2(); }

    typedef CGALi::Call_supporting_line_to_get<Line_2>     Construct_supporting_line_2;
    Construct_supporting_line_2
    construct_supporting_line_2_object() const 
    { return Construct_supporting_line_2(); }

    typedef CGALi::Call_perpendicular_to_get<Vector_2>     Construct_perpendicular_vector_2;
    Construct_perpendicular_vector_2
    construct_perpendicular_vector_2_object() const 
    { return Construct_perpendicular_vector_2(); }

    typedef CGALi::Call_perpendicular_to_get<Direction_2>  Construct_perpendicular_direction_2;
    Construct_perpendicular_direction_2
    construct_perpendicular_direction_2_object() const 
    { return Construct_perpendicular_direction_2(); }

    typedef CGALi::Call_perpendicular_to_get<Line_2>       Construct_perpendicular_line_2;
    Construct_perpendicular_line_2
    construct_perpendicular_line_2_object() const 
    { return Construct_perpendicular_line_2(); }

    typedef CGALi::p_Midpoint<Point_2>                     Construct_midpoint;
    Construct_midpoint
    construct_midpoint_object() const 
    { return Construct_midpoint(); }

    typedef CGALi::p_Circumcenter<Point_2>                 Construct_circumcenter_2;
    Construct_circumcenter_2
    construct_circumcenter_2_object() const 
    { return Construct_circumcenter_2(); }

    typedef CGALi::pl_Bisector<Point_2, Line_2>            Construct_bisector_2;
    Construct_bisector_2
    construct_bisector_2_object() const 
    { return Construct_bisector_2(); }

    typedef CGALi::Call_opposite_to_get<Segment_2>         Construct_opposite_segment_2;
    Construct_opposite_segment_2
    construct_opposite_segment_2_object() const 
    { return Construct_opposite_segment_2(); }

    typedef CGALi::Call_opposite_to_get<Ray_2>             Construct_opposite_ray_2;
    Construct_opposite_ray_2
    construct_opposite_ray_2_object() const 
    { return Construct_opposite_ray_2(); }

    typedef CGALi::Call_opposite_to_get<Line_2>            Construct_opposite_line_2;
    Construct_opposite_line_2
    construct_opposite_line_2_object() const 
    { return Construct_opposite_line_2(); }

    typedef CGALi::Call_opposite_to_get<Triangle_2>        Construct_opposite_triangle_2;
    Construct_opposite_triangle_2
    construct_opposite_triangle_2_object() const 
    { return Construct_opposite_triangle_2(); }

    typedef CGALi::Call_opposite_to_get<Circle_2>          Construct_opposite_circle_2;
    Construct_opposite_circle_2
    construct_opposite_circle_2_object() const 
    { return Construct_opposite_circle_2(); }

    typedef CGALi::Call_transform                          Transform_2;
    Transform_2
    transform_2_object() const 
    { return Transform_2(); }

    
    typedef CGALi::Assign                                  Assign_2;
    Assign_2
    assign_2_object() const 
    { return Assign_2(); }

    typedef CGALi::Intersect                               Intersect_2;
    Intersect_2
    intersect_2_object() const 
    { return Intersect_2(); }

    typedef CGALi::Call_y_at_x_to_get<FT>                  Compute_y_at_x_2;
    Compute_y_at_x_2
    compute_y_at_x_2_object() const 
    { return Compute_y_at_x_2(); }

    typedef CGALi::Call_squared_length_to_get<FT>          Compute_squared_length_2;
    Compute_squared_length_2
    Compute_squared_length_2_object() const 
    { return Compute_squared_length_2(); }
    typedef CGALi::Equal                                   Equal_2;
    Equal_2
    equal_2_object() const 
    { return Equal_2(); }

    typedef CGALi::Equal_x                                 Equal_x_2;
    Equal_x_2
    equal_x_2_object() const 
    { return Equal_x_2(); }

    typedef CGALi::Equal_y                                 Equal_y_2;
    Equal_y_2
    equal_y_2_object() const 
    { return Equal_y_2(); }

    typedef CGALi::Equal_xy                                Equal_xy_2;
    Equal_xy_2
    equal_xy_2_object() const 
    { return Equal_xy_2(); }

    typedef CGALi::Less_x                                  Less_x_2;
    Less_x_2
    less_x_2_object() const 
    { return Less_x_2(); }

    typedef CGALi::Less_y                                  Less_y_2;
    Less_y_2
    less_y_2_object() const 
    { return Less_y_2(); }

    typedef CGAL::p_Less_xy<Point_2>                       Less_xy_2;
    Less_xy_2
    less_xy_2_object() const 
    { return Less_xy_2(); }

    typedef CGAL::p_Less_yx<Point_2>                       Less_yx_2;
    Less_yx_2
    less_yx_2_object() const 
    { return Less_yx_2(); }

    typedef CGALi::Compare_x                               Compare_x_2;
    Compare_x_2
    compare_x_2_object() const 
    { return Compare_x_2(); }

    typedef CGALi::Compare_y                               Compare_y_2;
    Compare_y_2
    compare_y_2_object() const 
    { return Compare_y_2(); }

    typedef CGALi::Compare_xy                              Compare_xy_2;
    Compare_xy_2
    compare_xy_2_object() const 
    { return Compare_xy_2(); }

    typedef CGALi::Compare_y_at_x                          Compare_y_at_x_2;
    Compare_y_at_x_2
    compare_y_at_x_2_object() const 
    { return Compare_y_at_x_2(); }

    typedef CGAL ::p_Less_dist_to_point<Point_2>           Less_distance_to_point_2;
    Less_distance_to_point_2
    less_distance_to_point_2_object(const Point_2& p) const 
    { return Less_distance_to_point_2(p); }

    typedef CGAL ::p_Less_dist_to_line_2p<Point_2>         Less_signed_distance_to_line_2;
    Less_signed_distance_to_line_2
    less_signed_distance_to_line_2_object(const Point_2& p, const Point_2& q) const 
    { return Less_signed_distance_to_line_2(p,q); }

    typedef CGAL ::p_Less_rotate_ccw<Point_2>              Less_rotate_ccw_2;
    Less_rotate_ccw_2
    less_rotate_ccw_2_object(const Point_2& p) const 
    { return Less_rotate_ccw_2(p); }

    typedef CGALi::Counterclockwise_in_between             Counterclockwise_in_between_2;
    Counterclockwise_in_between_2
    counterclockwise_in_between_2_object() const 
    { return Counterclockwise_in_between_2(); }

    typedef CGAL ::p_Leftturn<Point_2>                     Leftturn_2;
    Leftturn_2
    leftturn_2_object() const 
    { return Leftturn_2(); }

    typedef CGAL ::p_Left_of_line_2p<Point_2>              Left_of_line_2;
    Left_of_line_2
    left_of_line_2_object(const Point_2& p, const Point_2& q) const 
    { return Left_of_line_2(p,q); }

    typedef CGALi::Collinear                               Collinear_2;
    Collinear_2
    collinear_2_object() const 
    { return Collinear_2(); }

    typedef CGAL ::p_Orientation<Point_2>                  Orientation_2;
    Orientation_2
    orientation_2_object() const 
    { return Orientation_2(); }

    typedef CGALi::Side_of_oriented_circle                 Side_of_oriented_circle_2;
    Side_of_oriented_circle_2
    side_of_oriented_circle_2_object() const 
    { return Side_of_oriented_circle_2(); }

    typedef CGALi::Side_of_bounded_circle                  Side_of_bounded_circle_2;
    Side_of_bounded_circle_2
    side_of_bounded_circle_2_object() const
    { return Side_of_bounded_circle_2(); }

    typedef CGALi::Call_is_horizontal                      Is_horizontal_2;
    Is_horizontal_2
    is_horizontal_2_object() const 
    { return Is_horizontal_2(); }

    typedef CGALi::Call_is_vertical                        Is_vertical_2;
    Is_vertical_2
    is_vertical_2_object() const 
    { return Is_vertical_2(); }

    typedef CGALi::Call_is_degenerate                      Is_degenerate_2;
    Is_degenerate_2
    is_degenerate_2_object() const 
    { return Is_degenerate_2(); }

    typedef CGALi::Call_has_on                             Has_on_2;
    Has_on_2
    has_on_2_object() const 
    { return Has_on_2(); }

    typedef CGALi::Call_collinear_has_on                   Collinear_has_on_2;
    Collinear_has_on_2
    collinear_has_on_2_object() const 
    { return Collinear_has_on_2(); }

    typedef CGALi::Call_has_on_bounded_side                Has_on_bounded_side_2;
    Has_on_bounded_side_2
    has_on_bounded_side_2_object() const 
    { return Has_on_bounded_side_2(); }

    typedef CGALi::Call_has_on_unbounded_side              Has_on_unbounded_side_2;
    Has_on_unbounded_side_2
    has_on_unbounded_side_2_object() const 
    { return Has_on_unbounded_side_2(); }

    typedef CGALi::Call_has_on_boundary                    Has_on_boundary_2;
    Has_on_boundary_2
    has_on_boundary_2_object() const 
    { return Has_on_boundary_2(); }

    typedef CGALi::Call_has_on_positive_side               Has_on_positive_side_2;
    Has_on_positive_side_2
    has_on_positive_side_2_object() const 
    { return Has_on_positive_side_2(); }

    typedef CGALi::Call_has_on_negative_side               Has_on_negative_side_2;
    Has_on_negative_side_2
    has_on_negative_side_2_object() const 
    { return Has_on_negative_side_2(); }

    typedef CGALi::Call_oriented_side                      Oriented_side_2;
    Oriented_side_2
    oriented_side_2_object() const 
    { return Oriented_side_2(); }

    typedef CGALi::Are_ordered_along_line                  Are_ordered_along_line_2 ;
    Are_ordered_along_line_2
    are_ordered_along_line_2_object() const 
    { return Are_ordered_along_line_2(); }

    typedef CGALi::Are_strictly_ordered_along_line         Are_strictly_ordered_along_line_2;
    Are_strictly_ordered_along_line_2
    are_strictly_ordered_along_line_2_object() const 
    { return Are_strictly_ordered_along_line_2(); }

    typedef CGALi::Collinear_are_ordered_along_line        Collinear_are_ordered_along_line_2;
    Collinear_are_ordered_along_line_2
    collinear_are_ordered_along_line_2_object() const 
    { return Collinear_are_ordered_along_line_2(); }

    typedef CGALi::Collinear_are_strictly_ordered_along_line Collinear_are_strictly_ordered_along_line_2;
    Collinear_are_strictly_ordered_along_line_2
    collinear_are_strictly_ordered_along_line_2_object() const 
    { return Collinear_are_strictly_ordered_along_line_2(); }

    typedef PointS3< FT>                           Point_3_base;        
    typedef VectorS3< FT>                          Vector_3_base;        
    typedef DirectionS3< FT>                       Direction_3_base;        
    typedef SegmentS3< FT>                         Segment_3_base;        
    typedef PlaneS3< FT>                           Plane_3_base;        
    typedef LineS3< FT>                            Line_3_base;        
    typedef RayS3< FT>                             Ray_3_base;        
    typedef TriangleS3< FT>                        Triangle_3_base;        
    typedef TetrahedronS3< FT>                     Tetrahedron_3_base;        
    typedef Iso_cuboidS3< FT>                      Iso_cuboid_3_base;        
    typedef SphereS3< FT>                          Sphere_3_base;   
    typedef Aff_transformationS3< FT>              Aff_transformation_3_base;        
    typedef typename KernelBase::Point_3               Point_3;
    typedef typename KernelBase::Vector_3              Vector_3;
    typedef typename KernelBase::Direction_3           Direction_3;
    typedef typename KernelBase::Plane_3               Plane_3;
    typedef typename KernelBase::Line_3                Line_3;
    typedef typename KernelBase::Segment_3             Segment_3;
    typedef typename KernelBase::Ray_3                 Ray_3;
    typedef typename KernelBase::Triangle_3            Triangle_3;
    typedef typename KernelBase::Tetrahedron_3         Tetrahedron_3;
    typedef typename KernelBase::Aff_transformation_3  Aff_transformation_3;

    typedef CGALi::Construct<Point_3>                  Construct_point_3;
    typedef CGALi::Construct<Vector_3>                 Construct_vector_3;
    typedef CGALi::Construct<Direction_3>              Construct_direction_3;
    typedef CGALi::Construct<Segment_3>                Construct_segment_3;
    typedef CGALi::Construct<Plane_3>                  Construct_plane_3;
    typedef CGALi::Construct<Line_3>                   Construct_line_3;
    typedef CGALi::Construct<Ray_3>                    Construct_ray_3;
    typedef CGALi::Construct<Triangle_3>               Construct_triangle_3;
    typedef CGALi::Construct<Tetrahedron_3>            Construct_tetrahedron_3;
    typedef CGALi::Construct<Aff_transformation_3>     Construct_aff_transformation_3;

    Construct_point_3 
    construct_point_3_object() const 
    { return Construct_point_3(); }

    Construct_vector_3
    construct_vector_3_object() const 
    { return Construct_vector_3(); }

    Construct_direction_3
    construct_direction_3_object() const 
    { return Construct_direction_3(); }

    Construct_segment_3
    construct_segment_3_object() const 
    { return Construct_segment_3(); }

    Construct_plane_3
    construct_plane_3_object() const 
    { return Construct_plane_3(); }

    Construct_line_3
    construct_line_3_object() const 
    { return Construct_line_3(); }

    Construct_ray_3
    construct_ray_3_object() const 
    { return Construct_ray_3(); }

    Construct_triangle_3
    construct_triangle_3_object() const 
    { return Construct_triangle_3(); }

    Construct_tetrahedron_3
    construct_tetrahedron_object() const 
    { return Construct_tetrahedron_3(); }

    Construct_aff_transformation_3
    construct_aff_transformation_3_object() const 
    { return Construct_aff_transformation_3(); }

    typedef CGALi::Call_point_to_get<Point_3>              Construct_point_on_3;
    Construct_point_on_3
    construct_point_on_3_object() const 
    { return Construct_point_on_3(); }

    typedef CGALi::Call_second_point_to_get<Point_3>       Construct_second_point_on_3;
    Construct_second_point_on_3
    construct_second_point_on_3_object() const 
    { return Construct_second_point_on_3(); }

    typedef CGALi::Call_perpendicular_plane_to_get<Plane_3> Construct_perpendicular_plane_3;
    Construct_perpendicular_plane_3
    construct_perpendicular_plane_3() const 
    { return Construct_perpendicular_plane_3(); }

    typedef CGALi::p_Midpoint<Point_3>                     Construct_midpoint_3;
    Construct_midpoint_3
    construct_midpoint_3_object() const 
    { return Construct_midpoint_3(); }

    typedef CGALi::p_Circumcenter<Point_3>                 Construct_circumcenter_3;
    Construct_circumcenter_3
    construct_circumcenter_3_object() const 
    { return Construct_circumcenter_3(); }

    typedef CGALi::Call_opposite_to_get<Segment_3>         Construct_opposite_segment_3;
    Construct_opposite_segment_3
    construct_opposite_segment_3_object() const 
    { return Construct_opposite_segment_3(); }

    typedef CGALi::Call_opposite_to_get<Ray_3>             Construct_opposite_ray_3;
    Construct_opposite_ray_3
    construct_opposite_ray_3_object() const 
    { return Construct_opposite_ray_3(); }

    typedef CGALi::Call_opposite_to_get<Line_3>            Construct_opposite_line_3;
    Construct_opposite_line_3
    construct_opposite_line_3_object() const 
    { return Construct_opposite_line_3(); }

    typedef CGALi::Call_supporting_plane_to_get<Plane_3>   Construct_supporting_plane_3;
    Construct_supporting_plane_3
    construct_supporting_plane_3_object() const 
    { return Construct_supporting_plane_3(); }

    typedef CGALi::Call_transform                          Transform_3;
    Transform_3
    transform_3_object() const 
    { return Transform_2(); }

    
    typedef CGALi::Assign                                  Assign_3;
    Assign_3
    assign_3_object() const 
    { return Assign_3(); }

    typedef CGALi::Intersect                               Intersect_3;
    Intersect_3
    intersect_3_object() const 
    { return Intersect_3(); }

    typedef CGALi::Call_squared_length_to_get<FT>          Compute_squared_length_3;
    Compute_squared_length_3
    compute_squared_length_3_object() const 
    { return Compute_squared_length_3(); }

    typedef CGALi::Equal                                   Equal_3;
    Equal_3
    equal_3_object() const 
    { return Equal_3(); }

    typedef CGALi::Equal_x                                 Equal_x_3;
    Equal_x_3
    equal_x_3_object() const 
    { return Equal_x_3(); }

    typedef CGALi::Equal_y                                 Equal_y_3;
    Equal_y_3
    equal_y_3_object() const 
    { return Equal_y_3(); }

    typedef CGALi::Equal_z                                 Equal_z_3;
    Equal_z_3
    equal_z_3_object() const 
    { return Equal_z_3(); }

    typedef CGALi::Equal_xy                                Equal_xy_3;
    Equal_xy_3
    equal_xy_3_object() const 
    { return Equal_xy_3(); }

    typedef CGALi::Equal_xyz                               Equal_xyz_3;
    Equal_xyz_3
    equal_xyz_3_object() const 
    { return Equal_xyz_3(); }

    typedef CGALi::Less_x                                  Less_x_3;
    Less_x_3
    less_x_3_object() const 
    { return Less_x_3(); }

    typedef CGALi::Less_y                                  Less_y_3;
    Less_y_3
    less_y_3_object() const 
    { return Less_y_3(); }

    typedef CGALi::Less_z                                  Less_z_3;
    Less_z_3
    less_z_3_object() const 
    { return Less_z_3(); }

    typedef CGAL::p_Less_xy<Point_3>                       Less_xy_3;
    Less_xy_3
    less_xy_3_object() const 
    { return Less_xy_3(); }

    typedef CGALi::Less_xyz                                Less_xyz_3;
    Less_xyz_3
    less_xyz_3_object() const 
    { return Less_xyz_3(); }

    typedef CGALi::Compare_x                               Compare_x_3;
    Compare_x_3
    compare_x_3_object() const 
    { return Compare_x_3(); }

    typedef CGALi::Compare_y                               Compare_y_3;
    Compare_y_3
    compare_y_3_object() const 
    { return Compare_y_3(); }

    typedef CGALi::Compare_z                               Compare_z_3;
    Compare_z_3
    compare_z_3_object() const 
    { return Compare_z_3(); }

    typedef CGALi::Compare_xy                              Compare_xy_3;
    Compare_xy_3
    compare_xy_3_object() const 
    { return Compare_xy_3(); }

    typedef CGALi::Compare_xyz                             Compare_xyz_3;
    Compare_xyz_3
    compare_xyz_3_object() const 
    { return Compare_xyz_3(); }

    typedef CGAL ::p_Less_dist_to_point<Point_3>           Less_distance_to_point_3;
    Less_distance_to_point_3
    less_distance_to_point_3_object(const Point_3& p) const 
    { return Less_distance_to_point_3(p); }

    typedef CGALi::Collinear                               Collinear_3;
    Collinear_3
    collinear_3_object() const 
    { return Collinear_3(); }

    typedef CGALi::Coplanar                                Coplanar_3 ;
    Coplanar_3
    coplanar_3_object() const 
    { return Coplanar_3(); }

    typedef CGAL ::p_Orientation<Point_3>                  Orientation_3;
    Orientation_3
    orientation_3_object() const 
    { return Orientation_3(); }

    typedef CGALi::Call_is_degenerate                      Is_degenerate_3;
    Is_degenerate_3
    is_degenerate_3_object() const 
    { return Is_degenerate_3(); }

    typedef CGALi::Call_has_on                             Has_on_3;
    Has_on_3
    has_on_3_object() const 
    { return Has_on_3(); }

    typedef CGALi::Call_has_on_bounded_side                Has_on_bounded_side_3;
    Has_on_bounded_side_3
    has_on_bounded_side_3_object() const 
    { return Has_on_bounded_side_3(); }

    typedef CGALi::Call_has_on_unbounded_side              Has_on_unbounded_side_3;
    Has_on_unbounded_side_3
    has_on_unbounded_side_3_object() const 
    { return Has_on_unbounded_side_3(); }

    typedef CGALi::Call_has_on_boundary                    Has_on_boundary_3;
    Has_on_boundary_3
    has_on_boundary_3_object() const 
    { return Has_on_boundary_3(); }

    typedef CGALi::Call_has_on_positive_side               Has_on_positive_side_3;
    Has_on_positive_side_3
    has_on_positive_side_3_object() const 
    { return Has_on_positive_side_3(); }

    typedef CGALi::Call_has_on_negative_side               Has_on_negative_side_3;
    Has_on_negative_side_3
    has_on_negative_side_3_object() const 
    { return Has_on_negative_side_3(); }

    typedef CGALi::Call_oriented_side                      Oriented_side_3;
    Oriented_side_3
    oriented_side_3_object() const 
    { return Oriented_side_3(); }

    typedef CGALi::Are_ordered_along_line                  Are_ordered_along_line_3 ;
    Are_ordered_along_line_3
    are_ordered_along_line_3_object() const 
    { return Are_ordered_along_line_3(); }

    typedef CGALi::Are_strictly_ordered_along_line         Are_strictly_ordered_along_line_3;
    Are_strictly_ordered_along_line_3
    are_strictly_ordered_along_line_3_object() const 
    { return Are_strictly_ordered_along_line_3(); }

    typedef CGALi::Collinear_are_ordered_along_line        Collinear_are_ordered_along_line_3;
    Collinear_are_ordered_along_line_3
    collinear_are_ordered_along_line_3_object() const 
    { return Collinear_are_ordered_along_line_3(); }

    typedef CGALi::Collinear_are_strictly_ordered_along_line Collinear_are_strictly_ordered_along_line_3;
    Collinear_are_strictly_ordered_along_line_3
    collinear_are_strictly_ordered_along_line_3_object() const 
    { return Collinear_are_strictly_ordered_along_line_3(); }

    typedef CGALi::Side_of_oriented_sphere                 Side_of_oriented_sphere_3;
    Side_of_oriented_sphere_3
    side_of_oriented_sphere_3_object() const 
    { return Side_of_oriented_sphere_3(); }

    typedef CGALi::Side_of_bounded_sphere                  Side_of_bounded_sphere_3;
    Side_of_bounded_sphere_3
    side_of_bounded_sphere_3_object() const 
    { return Side_of_bounded_sphere_3(); }

    typedef PointCd< FT>                           Point_d_base;      

    typedef typename KernelBase::Point_d               Point_d;

    typedef CGALi::Construct<Point_d>                  Construct_point_d;

    Construct_point_d 
    construct_point_d_object() const 
    { return Construct_point_d(); }

      typedef Data_accessorS2<FT>                  Data_accessor_2;
      typedef ConicCPA2<Point_2,Data_accessor_2>    Conic_2;
    static   FT make_FT(const RT & num, const RT& denom) { return num/denom;}
    static   FT make_FT(const RT & num)                  { return num;}
    static   RT FT_numerator(const FT &r)                { return r;}
    static   RT FT_denominator(const FT &)               { return RT(1);}
};
CGAL_END_NAMESPACE

#endif // CGAL_SIMPLE_CARTESIAN_REP_H

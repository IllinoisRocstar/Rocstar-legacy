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
// source        : webS3/S3.lw
// file          : include/CGAL/SimpleCartesian/distance_predicatesS3.h
// package       : S3 (1.6)
// revision      : 1.6
// revision_date : 28 Jun 2000
// author(s)     : Stefan Schirra
//                 based on code by
//                 Andreas Fabri and
//                 Herve Brönnimann
//
// coordinator   : MPI, Saarbrücken
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef DISTANCE_PREDICATESS3_H
#define DISTANCE_PREDICATESS3_H

#include <CGAL/SimpleCartesian/PointS3.h>
#include <CGAL/SimpleCartesian/PlaneS3.h>
#include <CGAL/predicates/kernel_ftC3.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
CGAL_KERNEL_LARGE_INLINE
Comparison_result
cmp_dist_to_point(const PointS3<FT>& p,
                  const PointS3<FT>& q,
                  const PointS3<FT>& r)
{
  return cmp_dist_to_pointC3(p.x(),p.y(),p.z(),
                             q.x(),q.y(),q.z(),
                             r.x(),r.y(),r.z());
}

template < class FT >
CGAL_KERNEL_LARGE_INLINE
bool
has_larger_dist_to_point(const PointS3<FT>& p,
                         const PointS3<FT>& q,
                         const PointS3<FT>& r)
{
  return has_larger_dist_to_pointC3(p.x(),p.y(),p.z(),
                                    q.x(),q.y(),q.z(),
                                    r.x(),r.y(),r.z());
}

template < class FT >
CGAL_KERNEL_LARGE_INLINE
bool
has_smaller_dist_to_point(const PointS3<FT>& p,
                          const PointS3<FT>& q,
                          const PointS3<FT>& r)
{
  return has_smaller_dist_to_pointC3(p.x(),p.y(),p.z(),
                                     q.x(),q.y(),q.z(),
                                     r.x(),r.y(),r.z());
}
template < class FT >
CGAL_KERNEL_LARGE_INLINE
Comparison_result
cmp_signed_dist_to_plane(const PlaneS3<FT>& h,
                         const PointS3<FT>& p,
                         const PointS3<FT>& q)
{
  return cmp_signed_dist_to_directionC3(h.a(),h.b(),h.c(),
                                        p.x(),p.y(),p.z(),
                                        q,x(),q.y(),q.z());
}

template < class FT >
CGAL_KERNEL_LARGE_INLINE
bool
has_larger_signed_dist_to_plane(const PlaneS3<FT>& h,
                                const PointS3<FT>& p,
                                const PointS3<FT>& q)
{
  return has_larger_signed_dist_to_directionC3(h.a(),h.b(),h.c(),
                                               p.x(),p.y(),p.z(),
                                               q,x(),q.y(),q.z());
}

template < class FT >
CGAL_KERNEL_LARGE_INLINE
bool
has_smaller_signed_dist_to_plane(const PlaneS3<FT>& h,
                                 const PointS3<FT>& p,
                                 const PointS3<FT>& q)
{
  return has_smaller_signed_dist_to_directionC3(h.a(),h.b(),h.c(),
                                                p.x(),p.y(),p.z(),
                                                q,x(),q.y(),q.z());
}
template < class FT >
CGAL_KERNEL_LARGE_INLINE
Comparison_result
cmp_signed_dist_to_plane(const PointS3<FT>& hp,
                         const PointS3<FT>& hq,
                         const PointS3<FT>& hr,
                         const PointS3<FT>& p,
                         const PointS3<FT>& q)
{
  return cmp_signed_dist_to_planeC3(hp.x(),hp.y(),hp.z(),
                                    hq.x(),hq.y(),hq.z(),
                                    hr.x(),hr.y(),hr.z(),
                                    p.x(),p.y(),p.z(),
                                    q,x(),q.y(),q.z());
}

template < class FT >
CGAL_KERNEL_LARGE_INLINE
bool
has_larger_signed_dist_to_plane(const PointS3<FT>& hp,
                                const PointS3<FT>& hq,
                                const PointS3<FT>& hr,
                                const PointS3<FT>& p,
                                const PointS3<FT>& q)
{
  return has_larger_signed_dist_to_planeC3(hp.x(),hp.y(),hp.z(),
                                           hq.x(),hq.y(),hq.z(),
                                           hr.x(),hr.y(),hr.z(),
                                           p.x(),p.y(),p.z(),
                                           q,x(),q.y(),q.z());
}

template < class FT >
CGAL_KERNEL_LARGE_INLINE
bool
has_smaller_signed_dist_to_plane(const PointS3<FT>& hp,
                                 const PointS3<FT>& hq,
                                 const PointS3<FT>& hr,
                                 const PointS3<FT>& p,
                                 const PointS3<FT>& q)
{
  return has_smaller_signed_dist_to_planeC3(hp.x(),hp.y(),hp.z(),
                                            hq.x(),hq.y(),hq.z(),
                                            hr.x(),hr.y(),hr.z(),
                                            p.x(),p.y(),p.z(),
                                            q,x(),q.y(),q.z());
}


CGAL_END_NAMESPACE

#endif // DISTANCE_PREDICATESS3_H

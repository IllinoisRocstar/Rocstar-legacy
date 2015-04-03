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
// file          : include/CGAL/predicates/kernel_ftC3.h
// package       : C3 (5.2)
// revision      : $Revision: 1.1.1.1 $
// revision_date : $Date: 2001/07/05 22:17:49 $
// author(s)     : Herve Bronnimann, Sylvain Pion
// coordinator   : INRIA Sophia-Antipolis
//
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_PREDICATES_KERNEL_FTC3_H
#define CGAL_PREDICATES_KERNEL_FTC3_H

#include <CGAL/predicates/sign_of_determinant.h>
#include <CGAL/predicates/kernel_ftC2.h>
#include <CGAL/constructions/kernel_ftC3.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
Comparison_result
compare_lexicographically_xyzC3(const FT &px, const FT &py, const FT &pz,
                                const FT &qx, const FT &qy, const FT &qz)
{
  Comparison_result c = CGAL_NTS compare(px, qx);
  if (c != EQUAL) return c;
  c = CGAL_NTS compare(py, qy);
  if (c != EQUAL) return c;
  return CGAL_NTS compare(pz, qz);
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
bool
strict_dominanceC3(const FT &px, const FT &py, const FT &pz,
		   const FT &qx, const FT &qy, const FT &qz)
{
  return CGAL_NTS compare(px, qx) == LARGER &&
	 CGAL_NTS compare(py, qy) == LARGER &&
	 CGAL_NTS compare(pz, qz) == LARGER;
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
bool
dominanceC3(const FT &px, const FT &py, const FT &pz,
	    const FT &qx, const FT &qy, const FT &qz)
{
  return CGAL_NTS compare(px, qx) != SMALLER && 
	 CGAL_NTS compare(py, qy) != SMALLER &&
	 CGAL_NTS compare(pz, qz) != SMALLER;
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
bool
collinearC3(const FT &px, const FT &py, const FT &pz,
            const FT &qx, const FT &qy, const FT &qz,
            const FT &rx, const FT &ry, const FT &rz)
{
  FT dpx = px-rx;
  FT dqx = qx-rx;
  FT dpy = py-ry;
  FT dqy = qy-ry;
  if (sign_of_determinant2x2(dpx, dqx, dpy, dqy) != ZERO)
      return false;
  FT dpz = pz-rz;
  FT dqz = qz-rz;
  return sign_of_determinant2x2(dpx, dqx, dpz, dqz) == ZERO
      && sign_of_determinant2x2(dpy, dqy, dpz, dqz) == ZERO;
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
Orientation
orientationC3(const FT &px, const FT &py, const FT &pz,
              const FT &qx, const FT &qy, const FT &qz,
              const FT &rx, const FT &ry, const FT &rz,
              const FT &sx, const FT &sy, const FT &sz)
{
  return Orientation(sign_of_determinant3x3(qx-px,rx-px,sx-px,
                                            qy-py,ry-py,sy-py,
                                            qz-pz,rz-pz,sz-pz));
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
Orientation
coplanar_orientationC3(const FT &qx, const FT &qy, const FT &qz,
                       const FT &rx, const FT &ry, const FT &rz,
                       const FT &sx, const FT &sy, const FT &sz,
                       const FT &px, const FT &py, const FT &pz)
{
  Orientation oxy_qrs = orientationC2(qx,qy,rx,ry,sx,sy);
  if (oxy_qrs != COLLINEAR)
      return Orientation( oxy_qrs * orientationC2(qx,qy,rx,ry,px,py));
  Orientation oyz_qrs = orientationC2(qy,qz,ry,rz,sy,sz);
  if (oyz_qrs != COLLINEAR)
      return Orientation( oyz_qrs * orientationC2(qy,qz,ry,rz,py,pz));
  Orientation oxz_qrs = orientationC2(qx,qz,rx,rz,sx,sz);
  assert(oxz_qrs != COLLINEAR);
  return Orientation( oxz_qrs * orientationC2(qx,qz,rx,rz,px,pz));
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
collinear_are_ordered_along_lineC3(
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz,
     const FT &rx, const FT &ry, const FT &rz)
{
  if (px < qx) return !(rx < qx);
  if (qx < px) return !(qx < rx);
  if (py < qy) return !(ry < qy);
  if (qy < py) return !(qy < ry);
  if (pz < qz) return !(rz < qz);
  if (qz < pz) return !(qz < rz);
  return true; // p==q
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
collinear_are_strictly_ordered_along_lineC3(
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz,
     const FT &rx, const FT &ry, const FT &rz)
{
  if (px < qx) return (qx < rx);
  if (qx < px) return (rx < qx);
  if (py < qy) return (qy < ry);
  if (qy < py) return (ry < qy);
  if (pz < qz) return (qz < rz);
  if (qz < pz) return (rz < qz);
  return false; // p==q
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
bool
equal_directionC3(const FT &dx1, const FT &dy1, const FT &dz1,
                  const FT &dx2, const FT &dy2, const FT &dz2)
{
  return sign_of_determinant2x2(dx1, dy1, dx2, dy2) == ZERO
      && sign_of_determinant2x2(dx1, dz1, dx2, dz2) == ZERO
      && sign_of_determinant2x2(dy1, dz1, dy2, dz2) == ZERO
      && CGAL_NTS sign(dx1) == CGAL_NTS sign(dx2)
      && CGAL_NTS sign(dy1) == CGAL_NTS sign(dy2)
      && CGAL_NTS sign(dz1) == CGAL_NTS sign(dz2);
}

template <class FT >
CGAL_KERNEL_LARGE_INLINE
Oriented_side
side_of_oriented_planeC3(const FT &a,  const FT &b,  const FT &c, const FT &d,
                         const FT &px, const FT &py, const FT &pz)
{
  return Oriented_side(CGAL_NTS sign(a*px + b*py + c*pz +d));
}

template <class FT >
CGAL_KERNEL_LARGE_INLINE
Oriented_side
side_of_oriented_sphereC3(const FT &px, const FT &py, const FT &pz,
                          const FT &qx, const FT &qy, const FT &qz,
                          const FT &rx, const FT &ry, const FT &rz,
                          const FT &sx, const FT &sy, const FT &sz,
                          const FT &tx, const FT &ty, const FT &tz)
{
  FT ptx = px - tx;
  FT pty = py - ty;
  FT ptz = pz - tz;
  FT pt2 = CGAL_NTS square(ptx) + CGAL_NTS square(pty) + CGAL_NTS square(ptz);
  FT qtx = qx - tx;
  FT qty = qy - ty;
  FT qtz = qz - tz;
  FT qt2 = CGAL_NTS square(qtx) + CGAL_NTS square(qty) + CGAL_NTS square(qtz);
  FT rtx = rx - tx;
  FT rty = ry - ty;
  FT rtz = rz - tz;
  FT rt2 = CGAL_NTS square(rtx) + CGAL_NTS square(rty) + CGAL_NTS square(rtz);
  FT stx = sx - tx;
  FT sty = sy - ty;
  FT stz = sz - tz;
  FT st2 = CGAL_NTS square(stx) + CGAL_NTS square(sty) + CGAL_NTS square(stz);
  return Oriented_side(sign_of_determinant4x4(ptx,pty,ptz,pt2,
                                              rtx,rty,rtz,rt2,
                                              qtx,qty,qtz,qt2,
                                              stx,sty,stz,st2));
}

template <class FT >
CGAL_KERNEL_MEDIUM_INLINE
Bounded_side
side_of_bounded_sphereC3(const FT &px, const FT &py, const FT &pz,
                         const FT &qx, const FT &qy, const FT &qz,
                         const FT &rx, const FT &ry, const FT &rz,
                         const FT &sx, const FT &sy, const FT &sz,
                         const FT &tx, const FT &ty, const FT &tz)
{
  Oriented_side s = side_of_oriented_sphereC3(px, py, pz,
                                              qx, qy, qz,
                                              rx, ry, rz,
                                              sx, sy, sz,
                                              tx, ty, tz);
  Orientation o = orientationC3(px, py, pz,
                                qx, qy, qz,
                                rx, ry, rz,
                                sx, sy, sz);
  return Bounded_side(s * o);
}

template < class FT >
CGAL_KERNEL_INLINE
Comparison_result
cmp_dist_to_pointC3(const FT &px, const FT &py, const FT &pz,
                    const FT &qx, const FT &qy, const FT &qz,
                    const FT &rx, const FT &ry, const FT &rz)
{
  return CGAL_NTS compare(squared_distanceC3(px,py,pz,qx,qy,qz),
                          squared_distanceC3(px,py,pz,rx,ry,rz));
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
has_larger_dist_to_pointC3(const FT &px, const FT &py, const FT &pz,
                           const FT &qx, const FT &qy, const FT &qz,
                           const FT &rx, const FT &ry, const FT &rz)
{
  return cmp_dist_to_pointC3(px,py,pz,qx,qy,qz,rx,ry,rz) == LARGER;
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
has_smaller_dist_to_pointC3(const FT &px, const FT &py, const FT &pz,
                            const FT &qx, const FT &qy, const FT &qz,
                            const FT &rx, const FT &ry, const FT &rz)
{
  return cmp_dist_to_pointC3(px,py,pz,qx,qy,qz,rx,ry,rz) == SMALLER;
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
Comparison_result
cmp_signed_dist_to_directionC3(
     const FT &pa, const FT &pb, const FT &pc,
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz)
{
  return CGAL_NTS compare(scaled_distance_to_directionC3(pa,pb,pc,px,py,pz),
                          scaled_distance_to_directionC3(pa,pb,pc,qx,qy,qz));
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
has_larger_signed_dist_to_directionC3(
     const FT &pa, const FT &pb, const FT &pc,
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz)
{
  return cmp_signed_dist_to_directionC3(pa,pb,pc,px,py,pz,qx,qy,qz) == LARGER;
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
has_smaller_signed_dist_to_directionC3(
     const FT &pa, const FT &pb, const FT &pc,
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz)
{
  return cmp_signed_dist_to_directionC3(pa,pb,pc,px,py,pz,qx,qy,qz) == SMALLER;
}

template < class FT >
CGAL_KERNEL_MEDIUM_INLINE
Comparison_result
cmp_signed_dist_to_planeC3(
     const FT &ppx, const FT &ppy, const FT &ppz,
     const FT &pqx, const FT &pqy, const FT &pqz,
     const FT &prx, const FT &pry, const FT &prz,
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz)
{
  return Comparison_result(sign_of_determinant3x3(
	      pqx-ppx, pqy-ppy, pqz-ppz,
	      prx-ppx, pry-ppy, prz-ppz,
	      qx-px,   qy-py,   qz-pz));
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
has_larger_signed_dist_to_planeC3(
     const FT &ppx, const FT &ppy, const FT &ppz,
     const FT &pqx, const FT &pqy, const FT &pqz,
     const FT &prx, const FT &pry, const FT &prz,
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz)
{
    return cmp_signed_dist_to_planeC3(ppx, ppy, ppz, pqx, pqy, pqz,
	    prx, pry, prz, px, py, pz, qx, qy, qz) == LARGER;
}

template < class FT >
/*CGAL_NO_FILTER*/
CGAL_KERNEL_MEDIUM_INLINE
bool
has_smaller_signed_dist_to_planeC3(
     const FT &ppx, const FT &ppy, const FT &ppz,
     const FT &pqx, const FT &pqy, const FT &pqz,
     const FT &prx, const FT &pry, const FT &prz,
     const FT &px, const FT &py, const FT &pz,
     const FT &qx, const FT &qy, const FT &qz)
{
    return cmp_signed_dist_to_planeC3(ppx, ppy, ppz, pqx, pqy, pqz,
	    prx, pry, prz, px, py, pz, qx, qy, qz) == SMALLER;
}

CGAL_END_NAMESPACE

#ifdef CGAL_ARITHMETIC_FILTER_H
#include <CGAL/Arithmetic_filter/predicates/kernel_ftC3.h>
#endif

#endif // CGAL_PREDICATES_KERNEL_FTC3_H

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
// source        : rational_rotation.fw
// file          : include/CGAL/rational_rotation.h
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
 

#ifndef CGAL_RATIONAL_ROTATION_H
#define CGAL_RATIONAL_ROTATION_H

#include <algorithm>

CGAL_BEGIN_NAMESPACE

template < class NT >
void
rational_rotation_approximation( const NT &  dirx,     // dir.x()
                                 const NT &  diry,     // dir.y()
                                       NT &  sin_num,  // return
                                       NT &  cos_num,  // return
                                       NT &  denom,    // return
                                 const NT &  eps_num,  // quality_bound
                                 const NT &  eps_den )
{
#ifndef CGAL_CFG_NO_NAMESPACE
  using std::swap;
#endif // CGAL_CFG_NO_NAMESPACE

  const NT& n   = eps_num;
  const NT& d   = eps_den;
  const NT  NT0 = NT(0)  ;
  const NT  NT1 = NT(1)  ;
  CGAL_kernel_precondition( n > NT0 );
  CGAL_kernel_precondition( d > NT0 );
  NT & sin = sin_num;
  NT & cos = cos_num;
  NT & den = denom;
  NT   dx = CGAL_NTS abs(dirx);
  NT   dy = CGAL_NTS abs(diry);
  NT   sq_hypotenuse = dx*dx + dy*dy;
  NT   common_part;
  NT   diff_part;
  NT   rhs;
  bool lower_ok;
  bool upper_ok;

  if (dy > dx)
  {
     swap (dx,dy);
  }
  // approximate sin = dy / sqrt(sq_hypotenuse)
  // if ( dy / sqrt(sq_hypotenuse) < n/d )
  if (dy * dy * d * d < sq_hypotenuse * n * n)
  {
      cos = NT1;
      sin = NT0;
      den = NT1;
  }
  else
  {
      NT  p;
      NT  q;
      NT  p0 = NT0;
      NT  q0 = NT1;
      NT  p1 = NT1;
      NT  q1 = NT1;

      for(;;)
      {
          p = p0 + p1;
          q = q0 + q1;
          sin = NT(2)*p*q;
          den = p*p + q*q;

          // sanity check for approximation
          //        sin/den < dy/sqrt(hypotenuse) + n/d
          //    &&  sin/den > dy/sqrt(hypotenuse) - n/d
          // ===    sin/den - n/d  <   dy/sqrt(sq_hypotenuse)
          //    &&  sin/den + n/d  >   dy/sqrt(sq_hypotenuse)
          // ===    (sin^2 d^2 + n^2 den^2)sq_hypotenuse - 2... < dy^2 d^2 den^2
          //    &&  (sin^2 d^2 + n^2 den^2)sq_hypotenuse + 2... > dy^2 d^2 den^2

          common_part = (sin*sin*d*d + n*n*den*den)*sq_hypotenuse;
          diff_part   = NT(2)*n*sin*d*den*sq_hypotenuse;
          rhs         = dy*dy*d*d*den*den;

          upper_ok    = (common_part - diff_part < rhs);
          lower_ok    = (common_part + diff_part > rhs);

          if ( lower_ok && upper_ok )
          {
             // if ( (p*p)%2 + (q*q)%2 > NT1)
             // {
             //     sin = p*q;
             //     cos = (q*q - p*p)/2;    // exact division
             //     den = (p*p + q*q)/2;    // exact division
             // }
             // else
             // {
                    cos = q*q - p*p;
             // }

             break;
          }
          else
          {
              // if ( dy/sqrt(sq_hypotenuse) < sin/den )
              if ( dy*dy*den*den < sin*sin*sq_hypotenuse )
              {
                  p1 = p;
                  q1 = q;
              }
              else
              {
                  p0 = p;
                  q0 = q;
              }
          }
      } // for(;;)
  }
  dx = dirx;
  dy = diry;


  if (dy > dx ) { swap (sin,cos); }

  if (dx < NT0) { cos = - cos; }

  if (dy < NT0) { sin = - sin; }

  sin_num = sin;
  cos_num = cos;
  denom   = den;
}


template < class NT >
void
rational_rotation_approximation( const double& angle,
                                            NT &  sin_num,  // return
                                            NT &  cos_num,  // return
                                            NT &  denom,    // return
                                      const NT &  eps_num,  // quality_bound
                                      const NT &  eps_den )
{
#ifndef CGAL_CFG_NO_NAMESPACE
  using std::swap;
#endif // CGAL_CFG_NO_NAMESPACE

  const NT& n   = eps_num;
  const NT& d   = eps_den;
  const NT  NT0 = NT(0)  ;
  const NT  NT1 = NT(1)  ;
  CGAL_kernel_precondition( n > NT0 );
  CGAL_kernel_precondition( d > NT0 );
  NT& isin = sin_num;
  NT& icos = cos_num;
  NT& iden = denom;
  double dsin = sin(angle);
  double dcos = cos(angle);
  double dn = CGAL::to_double(n);
  double dd = CGAL::to_double(d);
  double eps = dn / dd;
  dsin = CGAL_NTS abs( dsin);
  dcos = CGAL_NTS abs( dcos);
  NT   common_part;
  NT   diff_part;
  NT   os;
  bool lower_ok;
  bool upper_ok;
  bool swapped = false;

  if (dsin > dcos)
  {
     swapped = true;
     swap (dsin,dcos);
  }
  if ( dsin < eps )
  {
      icos = NT1;
      isin = NT0;
      iden = NT1;
  }
  else
  {
      NT  p;
      NT  q;
      NT  p0 = NT0;
      NT  q0 = NT1;
      NT  p1 = NT1;
      NT  q1 = NT1;

      for(;;)
      {
          p = p0 + p1;
          q = q0 + q1;
          isin = NT(2)*p*q;
          iden = p*p + q*q;

          // XXX sanity check for approximation
          //        sin/den < dsin + n/d
          //    &&  sin/den > dsin - n/d
          //        sin < dsin * den + n/d * den
          //    &&  sin > dsin * den - n/d * den
          os          = CGAL::to_double(isin);
          diff_part   = eps  * CGAL::to_double(iden);
          common_part = dsin * CGAL::to_double(iden);

          upper_ok    = (common_part - diff_part < os);
          lower_ok    = (os < common_part + diff_part);

          if ( lower_ok && upper_ok )
          {
             // if ( (p*p)%2 + (q*q)%2 > NT1)
             // {
             //     isin = p*q;
             //     icos = (q*q - p*p)/2;    // exact division
             //     iden = (p*p + q*q)/2;    // exact division
             // }
             // else
             // {
                    icos = q*q - p*p;
             // }

             break;
          }
          else
          {
              // XXX if ( dsin < sin/den )
              if ( dsin * CGAL::to_double(iden) < CGAL::to_double(isin) )
              {
                  p1 = p;
                  q1 = q;
              }
              else
              {
                  p0 = p;
                  q0 = q;
              }
          }
      } // for(;;)
  }

  if ( swapped ) { swap (isin,icos); }

  dsin = sin( angle);
  dcos = cos( angle);
  if (dcos < 0.0) { icos = - icos; }
  if (dsin < 0.0) { isin = - isin; }

  sin_num = isin;
  cos_num = icos;
  denom   = iden;
}


CGAL_END_NAMESPACE


#endif // CGAL_RATIONAL_ROTATION_H

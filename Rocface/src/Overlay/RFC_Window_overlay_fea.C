/* *******************************************************************
 * Rocstar Simulation Suite                                          *
 * Copyright@2015, Illinois Rocstar LLC. All rights reserved.        *
 *                                                                   *
 * Illinois Rocstar LLC                                              *
 * Champaign, IL                                                     *
 * www.illinoisrocstar.com                                           *
 * sales@illinoisrocstar.com                                         *
 *                                                                   *
 * License: See LICENSE file in top level of distribution package or *
 * http://opensource.org/licenses/NCSA                               *
 *********************************************************************/
/* *******************************************************************
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   *
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES   *
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND          *
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE CONTRIBUTORS OR           *
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   *
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE    *
 * USE OR OTHER DEALINGS WITH THE SOFTWARE.                          *
 *********************************************************************/
// $Id: RFC_Window_overlay_fea.C,v 1.24 2008/12/06 08:43:29 mtcampbe Exp $

/* \file RFC_Window_overlay_fea.C
 * This file implements the feature-detection algorithm using url-thresholding.
 */
/* Author: Xiangmin Jiao
 * Created:  Feb., 2002
 */

#include "RFC_Window_overlay.h"
#include "Overlay_primitives.h"
#include "Timing.h"
#include <functional>
#include <cstdio>
#include <iostream>
#include <fstream>
#include <algorithm>
#include <cmath>

RFC_BEGIN_NAME_SPACE

using  namespace std;

const float RFC_Window_overlay::r2d=180./3.1415926535;

/** @addtogroup fdp Feature-Detection Primitives
 *  @{
 */
/** Read in the control file for feature detection. The control file
 *  should be named as <window name>.fc. It should contain five lines. 
 *  The first three lines correspond to the parameters for face angle, 
 *  angle defect, and edge angle, respectively. The fourth line controls 
 *  filteration rules, including the minimum edge length for 
 *  open-ended 1-features, whether to apply long-falseness checking with
 *  strong endedness, and whether whether to snap the feature onto the
 *  reference mesh. The last line controls verbose level. 
 *  A sample control file (default) is:

 0.76604444 0.98480775  3 0.98480775  
 1.3962634  0.314159265 3
 0.17364818 0.96592583  3
 6 1 0 0
 1

 * For RSRM dataset, use the following control files:
 0.5 0.6 3 0.17365  # cos(face_angle_ub) cos(face_angle_lb) face_angle_r cos(weak_end)
 1.39626 0.314159 3 # angle_defect_ub angle_defect_lb angle_defect_r
 0 0.5 3            # cos(turn_angle_ub) cos(turn_angle_lb) turn_angle_r
 6 1 1 0            # min-length-of-ridge long-falseness-rule  strong-ended-check snapping
 1                  # verbose level

 * If the control file is missing, then the default values (above)
 * will be used. These default values should work for most cases.
 * For some extreme cases, adjusting the signal-to-noise ratios (the
 * last parameters of the first three lines) and the minimum edge
 * (line 4) should suffice. 
 */
void
RFC_Window_overlay::init_feature_parameters() {
  std::string fname = string(name())+".fc";
  std::ifstream f(fname.c_str());

  if ( f) {
    std::string buf;
    std::cout << "Reading in parameters from file " << fname 
	      << "..." << std::flush;
    f >> _cos_uf >> _cos_lf >> _rf >> _cos_weakend; getline(f, buf);
    f >> _ud >> _ld >> _rd; getline(f, buf);
    f >> _cos_ue >> _cos_le >> _re; getline(f, buf);
    f >> _min_1f_len >> _long_falseness_check 
      >> _strong_ended >> _snap_on_features; getline(f, buf);
    f >> verb; getline(f, buf);

    RFC_assertion( _cos_uf>=0 && _cos_uf<=1 && _cos_lf<=1 && _cos_lf>=_cos_uf);
    RFC_assertion( _rf==0 || _rf >=1);
    RFC_assertion( _ud>=0 && _ud<=180/r2d && _ld>=0 && _ld <= _ud);
    RFC_assertion( _rd==0 || _rd>=1);
    RFC_assertion( _cos_ue>=0 && _cos_ue<=1 && _cos_le<=1 && _cos_le>=_cos_ue);
    RFC_assertion( _re==0 || _re >=1);
    RFC_assertion( _min_1f_len >=1);
    std::cout << "Done" << std::endl;
  }
  else {
    // Use default parameters and write the parameters into the file
    _cos_uf=cos(50/r2d); _cos_lf=cos(15/r2d); _rf=3;  _cos_weakend = _cos_lf;
    _ud=60/r2d;          _ld=18/r2d;          _rd=3;
    _cos_ue=cos(60/r2d); _cos_le=cos(30/r2d); _re=3;
    _min_1f_len=6; _long_falseness_check = true;
    _strong_ended = true; _snap_on_features = false;
    verb=1;
  }

  if ( verb) {
    std::cout << "Using the following threshold to detect features in "
	      << name() << ":\n\n";
    
    std::cout << _cos_uf << ' ' << _cos_lf << ' ' << _rf << ' ' << _cos_weakend
	      << " # cos(face_angle_ub) cos(face_angle_lb) face_angle_r cos(weak_end)" << std::endl
	      << _ud << ' ' << _ld << ' ' << _rd
	      << " # angle_defect_ub angle_defect_lb angle_defect_r" << std::endl
	      << _cos_ue << ' ' << _cos_le << ' ' << _re
	      << " # cos(turn_angle_ub) cos(turn_angle_lb) turn_angle_r" << std::endl
	      << _min_1f_len << ' ' <<_long_falseness_check << ' '
	      << _strong_ended << ' ' << _snap_on_features 
	      << " # min-length-of-ridge long-falseness-rule "
	      << " strong-ended-check snapping" << std::endl
	      << verb << " # verbose level" << std::endl;
  }
}

/** Compute the cosine of the face angle (dihedral angle) at an 
 *  edge. When the face angle was computed the first time, it is saved and
 *  subsequent calls for the same edge will simply return the stored value.
 *  The face angle is defined to be pi if the edge is on the border.
 */
float 
RFC_Window_overlay::cos_face_angle( Halfedge *h, Halfedge *hopp) {
  // Precondition: Face angles are initialized to HUGE_VALF.
  RFC_Pane_overlay *p1=acc.get_pane(h);
  float &t = p1->get_cos_face_angle(h);
  if ( t<HUGE_VALF) return t;
  
  if ( acc.is_border( h) || acc.is_border( hopp) )
    t = -1;
  else {
    Overlay_primitives        op;
    Vector_3 v1 = op.get_face_normal( h, Point_2(0.5,0));
    Vector_3 v2 = op.get_face_normal( hopp, Point_2(0.5,0));
    
    t=std::min(v1 * v2 / sqrt( (v1*v1) * (v2*v2)),1.);
    RFC_Pane_overlay *p2=acc.get_pane(hopp);
    if ( p1 != p2) p2->get_cos_face_angle( hopp) = t;
  }
  return t;
}

/** Compute the cosine of the edge angle at a vertex between two incident
 *  feature edges. 
 */
float RFC_Window_overlay::
cos_edge_angle( const Halfedge *h1, const Halfedge *h2) {
  Vector_3 v1 = get_tangent( h1);
  Vector_3 v2 = get_tangent( h2);

  float d=std::max( -1., std::min( (v1*v2)/sqrt((v1*v1)*(v2*v2)), 1.));
  if ( acc.get_destination( h1) == acc.get_destination( h2) ||
       acc.get_origin( h1) == acc.get_origin( h2))
    return -d;
  else
    return d;
}

/** Compute the cosine of the edge angle at a vertex in a given feature curve.
 *  It saves the solution by associating it with the vertex. When the routine
 *  is invoked on the same vertex next time, it will return the saved value
 *  if the vertex is not at the end of the feature curve. 
 */
float RFC_Window_overlay::
cos_edge_angle(const Feature_1 &f1, Feature_1::const_iterator it, bool isloop)
{
  Vertex *v;
  if ( !isloop) {
    if (it==f1.begin()) {
      v=acc.get_origin( *it);
      acc.get_pane(v)->get_cos_edge_angle( v) = HUGE_VALF;
      return -1;
    }
    else if ( it==f1.end()) {
      v=acc.get_destination( f1.back());
      acc.get_pane(v)->get_cos_edge_angle( v) = HUGE_VALF;
      return -1.;
    }
  }

  if ( it==f1.begin() || it==f1.end()) {
    v = acc.get_origin(f1.front());
    float &t = acc.get_pane(v)->get_cos_edge_angle( v);
    if ( t == HUGE_VALF)
      t = cos_edge_angle( f1.front(), f1.back());
    return t;
  }
  else {
    v = acc.get_origin( *it);
    float &t = acc.get_pane(v)->get_cos_edge_angle( v);
    if ( t == HUGE_VALF) {
      Feature_1::const_iterator ip=it; --ip;
      t = cos_edge_angle( *it, *ip);
    }
    return t;
  }
}

/** Compute the angle defect of a vertex. The angle defect at v is defined as:
 *  1. the difference between 2*pi and the sum of its angles in incident faces
 *     if v is not a border vertex;
 *  2. the difference between pi and the sum of its angles in incident faces
 *     if v is a border vertex.
 *  When the angle defect was computed for a vertex for the first time, it is
 *  saved, and subsequent calls for the same vertex will return saved value.
 */
float 
RFC_Window_overlay::comp_angle_defect( Vertex *v) {
  const float pi = 3.1415926535;

  // Precondition: d was initialized to HUGE_VALF
  float &t=acc.get_pane(v)->get_angle_defect(v), d=t;
  if ( d < HUGE_VALF) return d;
  
  float angle_sum = 0.;

  Halfedge *h = acc.get_halfedge( v), *h0=h;
  bool is_border = false;
  do {
    if ( !acc.is_border(h))
      angle_sum += acos( -cos_edge_angle( h, acc.get_next(h)));
    else 
      is_border = true;
  } while ( (h=acc.get_next_around_destination( h)) != h0);

  return t = abs((2-is_border)*pi - angle_sum);
}

template <class T>
T squares( const T &t) { return t*t; }


/** Determine whether a vertex is strong (either theta-strong or 
 *  relatively strong) in angle defect.
 */
bool
RFC_Window_overlay::is_strong_ad( Vertex *v) {
  // Determine whether the edge is theta-strong
  float d = comp_angle_defect( v);
  if ( d >= _ud) { 
    return true;
  }
  else if ( d >= _ld) {
    float max_ad=0;
    Halfedge *h0 = acc.get_halfedge( v), *h1=h0;
    // Loop through incident edges of v
    do {
      float a = comp_angle_defect( acc.get_origin(h1));
      max_ad = std::max( max_ad, a);
    } while ( (h1=acc.get_next_around_destination(h1))!=h0);
    
    if ( d >= _rd*max_ad)
      return true;
  }
  return false;
}

/** Determine whether a vertex is relatively strong in edge angle 
 *  within a give feature.
 */
bool 
RFC_Window_overlay::is_rstrong_ea( const Feature_1 &f1, 
				   Feature_1::const_iterator hprev, 
				   Feature_1::const_iterator hnext, 
				   float cos_ea, bool isloop) {
  if ( cos_ea > _cos_le) return false;
  float cos_max = std::min( cos_edge_angle( f1, hprev, isloop),
			    cos_edge_angle( f1, hnext, isloop));
  return acos(cos_ea) >= _re*acos(cos_max);
}

/** @} end of fdp */

/** @addtogroup mf1 Manipulation of Strong Curves
 * @{
 */
/** Merge two feature curves into one at vertex v. After the operation, 
 *  f1 becomes the union of f1 and f2, and f2 becomes empty.
 */
void
RFC_Window_overlay::merge_features_1(Vertex *v, Feature_1 &f1, Feature_1 &f2) {
  RFC_assertion( !f1.empty() && f1!=f2);
  if ( f2.empty()) return;

  Vertex *dst, *src;
  
  if ( (dst=acc.get_destination( f1.back())) == 
       acc.get_origin( f2.front()) && v==dst)
    f1.splice( f1.end(), f2);
  else if ( (src=acc.get_origin( f1.front())) == 
	    acc.get_destination( f2.back()) && v==src) {
    f2.splice( f2.end(), f1);
    f1.swap( f2);
  }
  else if ( dst == acc.get_destination( f2.back()) && v==dst) {
    while ( !f2.empty()) {
      f1.push_back( acc.get_opposite( f2.back()));
      f2.pop_back();
    }
  }
  else {
    RFC_assertion( src == acc.get_origin( f2.front()) && v==src);
    while ( !f2.empty()) {
      f1.push_front( acc.get_opposite( f2.front()));
      f2.pop_front();
    }
  }
  RFC_assertion( f2.empty());
}

/** Determine whether a curve is false strong.
 */
bool
RFC_Window_overlay::check_false_strong_1( Feature_1 &f1) {
  if ( cos_face_angle( f1.front(),NULL)==-1) return false;

  Vertex *src = acc.get_origin(f1.front());
  Vertex *dst = acc.get_destination( f1.back());

  // is a loop or strong at both ends
  if ( src == dst || is_on_feature( src) && is_on_feature( dst)
       && (!_f0_ranks.empty() || is_feature_0(src) || is_feature_0(dst)) || 
       cos_face_angle(f1.front(),NULL)<0 && cos_face_angle(f1.back(),NULL)<0)
    return false;

  // Is it weak ended? If so, pop out the weak edges
  if ( _strong_ended) {
    if ( !f1.empty())  for (;;) {
      Halfedge *h=f1.front(), *hopp=acc.get_opposite(h);

      if ( is_on_feature( src) || cos_face_angle( h, hopp) <= _cos_uf || is_strong_ad( src))
	break;
      else {
	acc.get_pane(h)->unset_strong_edge(h);
	acc.get_pane( hopp)->unset_strong_edge( hopp);
	f1.pop_front();
      }
      if ( !f1.empty()) 
	src = acc.get_origin( f1.front());
      else
	return true;
    }

    if ( !f1.empty())  for (;;) {
      Halfedge *h=f1.back();
      Halfedge *hopp=acc.get_opposite(h);

      if ( is_on_feature( dst) || cos_face_angle( h, hopp) <= _cos_uf 
	   || is_strong_ad( src))
	break;
      else {
	acc.get_pane(h)->unset_strong_edge(h);
	acc.get_pane( hopp)->unset_strong_edge( hopp);
	f1.pop_back();
      }
      if ( !f1.empty()) 
	dst = acc.get_destination( f1.back());
      else
	return true;
    }
  }

  // is it too short?
  if ( _min_1f_len) {
    int c = !(is_strong_ad( src) || is_feature_0( src)) + 
            !(is_strong_ad( dst) || is_feature_0( dst));
    if ( int(f1.size()) <= c*_min_1f_len)
      return true;
  }

  // is it too close to another features?
  if ( _long_falseness_check) {
    if ( _f_list_1.empty() || 
	 (is_strong_ad( src) || is_feature_0( src)) &&
	 (is_strong_ad( dst) || is_feature_0( dst)))
      return false;
    for ( Feature_1::const_iterator it=++f1.begin(); it!=f1.end(); ++it) {
      Vertex *v = acc.get_origin( *it);
      Halfedge *h0 = acc.get_halfedge( v), *h1=h0;
      // Loop through incident edges of v
      do {
	if ( is_on_feature( acc.get_origin( h1))) { v=NULL; break; }
      } while ( (h1=acc.get_next_around_destination(h1))!=h0);
      if (v==NULL) continue;
      else return false;
    }
    return true;
  }
  return false;
}

/** Subdivide a feature curve by splitting it at 0-features.
 *  Some false strong edges may be filtered out during this step.
 */
void RFC_Window_overlay::
subdiv_feature_curve( const Feature_1 &f1, 
		      Feature_list_1 &new_flist, int &dropped) {
  
  // We loop through all vertices in the curve to locate the 0-features
  list< Feature_1::const_iterator>  divs;
  Vertex *src = acc.get_origin(f1.front());
  Vertex *trg = acc.get_destination( f1.back());
  bool isloop = (src==trg);

  // A 1-feature is dangling if one of its end point is a terminus
  bool isdangling = !isloop && 
    (_f0_ranks.find( src) == _f0_ranks.end() || 
     _f0_ranks.find( trg) == _f0_ranks.end());

  float cos_min_fa_before=1, cos_min_fa_after=1;
  Feature_1::const_iterator f1end = f1.end(), hfirst = f1end, hlast=f1end;

  for (Feature_1::const_iterator hprev=f1.begin(),hi=++f1.begin(),hnext=hi;
       hi!=f1end; hprev=hi,hi=hnext) {
    ++hnext;
    Vertex *v = acc.get_origin(*hi);
    if ( is_feature_0( v)) { RFC_assertion( hi==f1.begin()); continue; }

    float cos_ea = cos_edge_angle( f1, hi, isloop);
    if ( cos_ea <= _cos_ue || 
	 is_rstrong_ea( f1, hprev, hnext, cos_ea, isloop)) { 
      divs.push_back( hi); 
      // When breaking loops, mark breakpoints as corners
      set_feature_0( acc.get_origin( *hi));
    }
    else if ( isdangling) {
      // Mark transitions between strong and week edges for danging 1-features
      // as 0-feature
      float dpre = cos_face_angle( *hprev, acc.get_opposite( *hprev));
      float d = cos_face_angle( *hi, acc.get_opposite( *hi));

      if ( hfirst == f1end && dpre < cos_min_fa_before)
	cos_min_fa_before = dpre;

      if ( std::min( dpre,d) <= _cos_weakend && 
	   std::max( dpre,d) > _cos_weakend ) {
	if ( hfirst == f1end) hfirst = hi;
	hlast = hi; cos_min_fa_after = 1.;
      }

      if ( d < cos_min_fa_before) cos_min_fa_after = d;
    }
  }

  if ( isdangling && divs.empty() ) {
    if ( hfirst!=f1end && cos_min_fa_before>_cos_weakend &&
	 _f0_ranks.find( src) == _f0_ranks.end() ) 
    { divs.push_back( hfirst); set_feature_0( acc.get_origin( *hfirst)); }

    if ( hlast!=f1end && hlast!=hfirst && cos_min_fa_after>_cos_weakend &&
	 _f0_ranks.find( trg) == _f0_ranks.end()) 
    { divs.push_back( hlast); set_feature_0( acc.get_origin( *hlast)); }
  }

  if ( isloop) {
    // Finally, we process end vertices
    Feature_1::const_iterator hi = f1.begin();
    float cos_ea = cos_edge_angle( f1, hi, isloop);
    if ( cos_ea <= _cos_ue || 
	 is_rstrong_ea( f1,f1end,++f1.begin(),cos_ea,isloop)) {
      // When breaking loops, mark breakpoints as corners
      divs.push_back( f1end); set_feature_0( acc.get_origin( *hi));
    }
  }
  else
    divs.push_back( f1end);
  
  // Now subdivide the curve into sub-curves
  Feature_list_1   subcur;
  if ( divs.size()==0)
    subcur.push_back( f1);
  else {
    subcur.push_back( Feature_1( f1.begin(),divs.front()));
    list< Feature_1::const_iterator>::const_iterator dit=divs.begin();
    for ( list< Feature_1::const_iterator>::const_iterator 
	    dinext=dit; ++dinext != divs.end(); dit=dinext)
      subcur.push_back( Feature_1( *dit, *dinext));
    
    if ( isloop) {
      Feature_1 &newf=subcur.front();
      newf.insert( newf.begin(), *dit, f1end);
    }
  }
  
  // Check the sub-curves to filter out false-strongness.
  for (Feature_list_1::iterator sit=subcur.begin();sit!=subcur.end();++sit) {
    if ( check_false_strong_1( *sit)) {
      for ( Feature_1::const_iterator i=sit->begin(); i!=sit->end(); ++i) {
	acc.get_pane(*i)->unset_strong_edge(*i); ++dropped;
	Halfedge *hopp=acc.get_opposite(*i);
	acc.get_pane( hopp)->unset_strong_edge( hopp);
      }
    }
    else {
      // Set all vertices on-feature
      set_on_feature( acc.get_origin( sit->front()));
      for ( Feature_1::const_iterator i=sit->begin(); i!=sit->end(); ++i)
	set_on_feature( acc.get_destination( *i));
      
      Vertex *src = acc.get_origin(sit->front());
      Vertex *dst = acc.get_destination(sit->back());
      
      if ( src!=dst) {	set_feature_0( src); set_feature_0( dst); }
      new_flist.push_back( Feature_1()); new_flist.back().swap(*sit);
    }
  }
}

/** Remove a false strong curve. */
void 
RFC_Window_overlay::remove_feature_1( Feature_1 &f) {
  
  for ( Feature_1::iterator it=f.begin(), iend=f.end(); it != iend; ++it) {
    unset_strong_edge( *it);  unset_strong_edge( acc.get_opposite( *it));
  }

  for ( Feature_1::iterator it=f.begin(), iend=f.end(); it != iend; ++it) {
    Halfedge *k = *it, *kopp = acc.get_opposite( k);
    RFC_assertion( !acc.get_pane(k)->_f_n_index.empty());
    do {
      Halfedge *h0 = k, *h = h0;
      do {
	RFC_assertion( !is_feature_1( h));
	RFC_Pane_overlay &pane = *acc.get_pane( h);
	pane._f_n_index[ pane.get_index( h)] = -1;
      } while ( (h=acc.get_next_around_destination(h)) != h0);
    } while ( (k=(k==kopp)?*it:kopp) != *it);
  }
}

/** @} end of mf1 */

/** @addtogroup mf0 Manipulation of Strong Vertices. 
 *  @{
 */
/** Identify the 0-features.
 */
void
RFC_Window_overlay::identify_features_0() {
  Feature_list_1                 new_flist;
  _f_list_1.swap( new_flist); 

  Feature_list_1::iterator flit;
  // Split feature curves at known feature vertices
  for ( flit=new_flist.begin(); flit!=new_flist.end(); ++flit) {
    Feature_list_1 subf;
    Feature_1::iterator fit=flit->begin(),fprev=fit;
    for ( ++fit; fit!=flit->end(); ++fit) {
      if ( is_feature_0( acc.get_origin( *fit))) {
	subf.push_back( Feature_1( fprev,fit));
	fprev=fit;
      }
    }

    if ( is_feature_0( acc.get_origin( *flit->begin())) || subf.empty()) {
      subf.push_back( Feature_1( fprev,fit));
    }
    else {
      Feature_1 &newf=subf.front();
      newf.insert( newf.begin(), fprev, flit->end());
    }
    
    // Assign the ranks and mark the ones with rank 2.
    for (Feature_list_1::iterator sit=subf.begin(); sit!=subf.end(); ++sit) {
      _f_list_1.push_back( *sit);
    }
  }

  // Determine the ranks of the vertices
  RFC_assertion( _f0_ranks.empty());
  for ( flit=_f_list_1.begin(); flit!=_f_list_1.end(); ++flit) {
    Vertex *src = acc.get_origin(flit->front());
    Vertex *dst = acc.get_destination(flit->back());

    map<Vertex*,int>::iterator i=_f0_ranks.find(src);
    if ( i==_f0_ranks.end())
      _f0_ranks.insert(make_pair(src,-1-(src==dst)));
    else
      i->second = abs(i->second)+1+(src==dst);
    if ( src!=dst) {
      if ( (i=_f0_ranks.find(dst))==_f0_ranks.end())
	_f0_ranks.insert(make_pair(dst,-1));
      else
	i->second = abs(i->second)+1;
    }
  }

  // Determine the connectivity at rank-2 vertices. 
  std::map<Vertex*,std::vector<Feature_1*> >   turn_maps;
  for ( flit=_f_list_1.begin(); flit!=_f_list_1.end(); ++flit) {
    // Assign the ranks and mark the ones with rank 2.
    Vertex *src = acc.get_origin(flit->front());
    map<Vertex*,int>::iterator i=_f0_ranks.find(src);
    if ( i!=_f0_ranks.end() && abs(i->second)==2)
      turn_maps[ src].push_back( &*flit);

    Vertex *dst = acc.get_destination(flit->back());
    i=_f0_ranks.find(dst);
    if ( i!=_f0_ranks.end() && abs(i->second)==2)
      turn_maps[ dst].push_back( &*flit);
  }

  if ( !turn_maps.empty()) {
    std::map<Vertex*,std::vector<Feature_1*> >::iterator i;
    RFC_assertion_code(for ( i=turn_maps.begin(); i!=turn_maps.end(); ++i)
		       RFC_assertion( i->second.size()==2));

    new_flist.clear();
    // Merge at weak rank-2 vertices
    for ( flit=_f_list_1.begin(); flit!=_f_list_1.end(); ++flit) {
      if ( flit->empty()) continue;
      Feature_1 &f1 = *flit;

      bool  modified=false;
      Vertex *src = acc.get_origin(f1.front());
      Vertex *dst = acc.get_destination(f1.back());
      
      if ( src==dst) {
 	if ( (i=turn_maps.find( src)) != turn_maps.end()) {
	  turn_maps.erase( i);
	  if ( !is_strong_ad( src)) {
	    acc.get_pane( src)->unset_feature_0(src);
	    _f0_ranks.erase( _f0_ranks.find(src));
	  }
	  else {
	    acc.get_pane( src)->set_feature_0(src);
	    _f0_ranks[src]=2;
	  }
 	}
      }
      else for ( int c=0; c<2; ++c) {
	for (;;) {
	  Vertex *v=(c?acc.get_destination(f1.back()):
		     acc.get_origin(f1.front()));
	  if ( (i=turn_maps.find( v))==turn_maps.end()) break;
	  Feature_1 *f2;
	  if ( (f2 = i->second[0]) == &f1) f2 = i->second[1];
	  turn_maps.erase( i);
	  if ( is_feature_0(v)) {
	    acc.get_pane( v)->unset_feature_0(v);
	    _f0_ranks.erase( _f0_ranks.find(v));
	  }
	  if ( f2 == &f1) break;
	  merge_features_1( v, f1, *f2);
	  
	  Vertex *u;
	  if ( (u=acc.get_destination(f1.back())) != v && 
	       (i=turn_maps.find(u)) != turn_maps.end()) {
	    if ( i->second[0] == f2) i->second[0] = &f1; 
	    if ( i->second[1] == f2) i->second[1] = &f1; 
	  }
	  else if ((u=acc.get_origin(f1.front())) != v && 
		   (i=turn_maps.find(u)) != turn_maps.end()) {
	    if ( i->second[0] == f2) i->second[0] = &f1; 
	    if ( i->second[1] == f2) i->second[1] = &f1; 
	  }
	  modified = true;
	}
      }

      // Split at strong rank-2 vertices. 
      if ( modified) {
	Feature_list_1 subf;
	int dropped=0;
	subdiv_feature_curve( f1, subf, dropped);
	RFC_assertion( dropped==0);
	f1.clear();
	for (Feature_list_1::iterator it=subf.begin(); it!=subf.end(); ++it) {
	  new_flist.push_back( Feature_1());
	  Vertex *v=acc.get_origin(it->front());
	  if ( is_feature_0(v) && _f0_ranks.find(v)==_f0_ranks.end()) 
	    _f0_ranks[ v] = 2;
	  new_flist.back().swap( *it);
	}
      }
      else {
	new_flist.push_back( Feature_1());
	new_flist.back().swap( f1);
      }
    }
    _f_list_1.swap( new_flist);
  }
  RFC_assertion( turn_maps.empty());

  // Fill 0-feature list
  for ( map<Vertex*,int>::iterator it=_f0_ranks.begin(); 
	it!=_f0_ranks.end(); ++it) {
    // Adjust the rank for termini
    if ( it->second == -1 && is_strong_ad( it->first)) it->second = 1;

    RFC_assertion( is_feature_0( it->first));
    _f_list_0.push_back( Feature_0( it->first));
  }
  _f_list_0.sort();
}

/** Remove the given 0-feature from the list */
RFC_Window_overlay::Feature_list_0::iterator
RFC_Window_overlay::remove_feature_0( Feature_list_0::iterator i) {
  Vertex *v = i->vertex();
  acc.get_pane(v)->unset_feature_0(v);
  return _f_list_0.erase(i);
}

/** @} end of mf0 */

/** @defgroup pifd Public Interface for Feature Detection
 * @{
 */
/** The main entry of feature detection. */
void 
RFC_Window_overlay::detect_features() {
  int size_edges=0, dropped=0;

  // Initializing data arrays.
  Pane_set::iterator it=_pane_set.begin(), iend=_pane_set.end();
  for ( ; it != iend; ++it) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*it->second;
    int num_hedgs=pane._hds.size_of_halfedges();
    int num_verts=pane._hds.size_of_vertices();
    size_edges+=num_hedgs/2;

    pane.init_face_angle(); pane.init_angle_defect(); pane.init_edge_angle();
    pane._is_f_1.clear(); pane._is_f_1.resize( num_hedgs, false);
    pane._is_f_0.clear(); pane._is_f_0.resize( num_verts, false);
    pane._is_on_f.clear(); pane._is_on_f.resize( num_verts, false);
  }
  std::vector< pair<float, Halfedge*> > tstrong_edges, rstrong_edges;   
  tstrong_edges.reserve(size_edges/10);
  rstrong_edges.reserve(size_edges/10);

  float t0=get_wtime(), totaltime=0; 
  // loop through all panes and primary halfedges in each pane
  for ( it=_pane_set.begin(); it != iend; ++it) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*it->second;
    
    // Detect theta-strong edges
    for ( RFC_Pane_overlay::HDS::Halfedge_iterator 
	    hit=pane.hds().halfedges_begin(), hend=pane.hds().halfedges_end(); 
	  hit!=hend; ++ ++hit) {
      Halfedge *h = &*hit, *hopp = acc.get_opposite(h);
      if ( pane.id() <= acc.get_pane(hopp)->id()) {
	// Determine whether the edge is theta-strong
	float d = cos_face_angle( h, hopp);
	
	if ( d < _cos_uf) tstrong_edges.push_back( make_pair(d,h));
      }
    }
  }
  float t1=get_wtime(); totaltime+=t1-t0;
  if ( verb >= 3) {
    std::cout << "\tIdentified " << tstrong_edges.size() 
	      << " theta-strong edges with Theta=" << acos( _cos_uf)*r2d
	      << " in " << t1-t0 << " sec." << std::endl;
    t0=get_wtime(); 
  }

  sort(tstrong_edges.begin(),tstrong_edges.end());
  t1=get_wtime();  totaltime+=t1-t0;
  if ( verb >= 3) {
    std::cout << "\tSorted theta-strong edges in " 
	      << t1-t0 << " sec." << std::endl;
  }
  
  vector<std::pair<float,Halfedge*> > iedges; iedges.reserve(16);
  _f_list_1.clear();

  // Sort the strong edges into strong curves
  unmark_alledges(); 
  float min_fa_r=HUGE_VALF;
  t0=get_wtime(); 
  for ( vector< pair<float,Halfedge*> >::iterator
	  it=tstrong_edges.begin(),iend=tstrong_edges.end(); it!=iend; ++it){
    Halfedge *h=it->second;
    if ( acc.marked( h)) continue;
    Halfedge *hopp=acc.get_opposite( h);

    // Create a new list
    Feature_1 f1;
    f1.push_back( h);

    // Mark the edge and its opposite
    acc.get_pane(h)->set_strong_edge(h);  acc.mark( h); 
    acc.get_pane(hopp)->set_strong_edge(hopp); acc.mark( hopp);

    Vertex *src=acc.get_origin( f1.front());
    Vertex *dst=acc.get_destination(f1.back());

    // Traverse forwards and backwards along the curve, and append an 
    // r-strong edge to the curve until we reach a non-strong curve.
    for (int c=0; c<2; ++c) {
      for (;;) {
	Halfedge *h = ((c==0)?f1.back():f1.front());
	if ( src == dst || c==0&&is_on_feature(dst) || c&&is_on_feature(src))
	  break;
	
	// Get the unmarked 1-feature with mimimum angle
	iedges.clear();
	Halfedge *h0=acc.get_opposite(h);
	float d0=cos_face_angle( h, h0);
	pair<float, Halfedge*> t0(d0,(c==0)?h0:h);
	pair<float, Halfedge*> cos_max(HUGE_VALF,NULL);
	
	Halfedge *h1=acc.get_next_around_origin(t0.second);
	do {
	  Halfedge *h1o=acc.get_opposite(h1);
	  float d = cos_face_angle(h1, h1o);
	  pair<float, Halfedge*> t(d,h1);
	  iedges.push_back( t);

	  if ( t < cos_max) cos_max = t;
	} while ( (h1=acc.get_next_around_origin(h1)) != t0.second);

	bool is_strong=true;
	const Vector_3 v1=get_tangent( t0.second);
	if ( cos_max.first>_cos_uf && iedges.size()>1) {
	  for ( int i=0,s=iedges.size(); i<s; ++i) {
	    const Vector_3 v2=get_tangent( iedges[i].second);
	    const Real t = std::max(-v1*v2/sqrt((v1*v1)*(v2*v2)),0.);
	    iedges[i].first = acos(iedges[i].first)*t;
	  }
	  sort(iedges.rbegin(),iedges.rend());
	  cos_max = iedges[0];
	  if ( cos(cos_max.first)>_cos_lf) break;

	  is_strong = cos_max.first >= iedges[1].first*_rf ||
	    is_on_feature( acc.get_destination(cos_max.second)) ||
	    is_strong_ad( acc.get_destination(cos_max.second));
	  min_fa_r=std::min(min_fa_r,cos_max.first/iedges[1].first);
	}
	const Vector_3 v2=get_tangent( cos_max.second);
	float a = -v1*v2/sqrt((v1*v1)*(v2*v2));
	if ( a<_cos_ue || a<cos_face_angle(cos_max.second,NULL)) break;
	
	rstrong_edges.push_back(make_pair(cos_face_angle(cos_max.second,0),
					  cos_max.second));
	
	h = cos_max.second;
	if ( acc.marked( h)) break;
	Halfedge *hopp = acc.get_opposite( h);

	acc.mark( h);    acc.get_pane(h)->set_strong_edge( h);
	acc.mark( hopp); acc.get_pane(hopp)->set_strong_edge( hopp);
	if (c==0) {
	  f1.push_back( h); 
	  dst = acc.get_destination( h); 
	}
	else {
	  f1.push_front( hopp);
	  src = acc.get_origin( hopp);
	}
	if ( !is_strong) break;
      }
    }

    Feature_list_1 flist;
    subdiv_feature_curve( f1, flist, dropped);
    _f_list_1.splice( _f_list_1.end(), flist);
  }

  t1=get_wtime();  totaltime+=t1-t0;
  if ( verb >= 3) {
    std::cout << "\tIdentified " << rstrong_edges.size()
	      << " r-strong edges with r=" << _rf 
	      << " in " << t1-t0 << " sec." << std::endl;
    sort(rstrong_edges.begin(), rstrong_edges.end());
    dump_strong_edges( tstrong_edges, rstrong_edges);
    t0=get_wtime();
  }

  identify_features_0();

  totaltime+=t1-t0;
  if ( verb >= 1) {
    std::cout << "\tFound " << _f_list_1.size() << " ridges and " 
	      << _f_list_0.size() << " corners and dropped "
	      << dropped << " false-strong edges.\n"
	      << "\tDone in " << totaltime << " sec." << std::endl;
#ifndef DEBUG
    if ( verb >= 2)
#endif
      print_features();
  }
  
  // Compute the bounding boxes of the 1-features
  for ( Feature_list_1::iterator it=_f_list_1.begin();
	it != _f_list_1.end(); ++it) {
    it->bbox += Bbox_3(acc.get_origin( it->front())->point());
    for (Feature_1::iterator hi=it->begin(), hiend=it->end(); hi!=hiend; ++hi){
      it->bbox += Bbox_3(acc.get_destination( *hi)->point());
    }
  }

  // Finalization
  unmark_alledges();

  free_vector( tstrong_edges); rstrong_edges.clear();
  for ( it=_pane_set.begin(); it != _pane_set.end(); ++it) {
    RFC_Pane_overlay &pane = (RFC_Pane_overlay &)*it->second;

    free_vector( pane._fd_1);
    free_vector( pane._ad_0);
    free_vector( pane._ea_0);
  }
}

/** @} end of pifd */

RFC_END_NAME_SPACE







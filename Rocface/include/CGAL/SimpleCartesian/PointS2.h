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
// $Id: PointS2.h,v 1.3 2008/12/06 08:43:27 mtcampbe Exp $

#ifndef CGAL_OPT_SIMPLE_CARTESIAN_POINT_2_H
#define CGAL_OPT_SIMPLE_CARTESIAN_POINT_2_H

CGAL_BEGIN_NAMESPACE

template <class FT>
class PointS2 {
  typedef  PointS2<FT>                        Self;
  typedef  VectorS2<FT>                       Vector;
  typedef  Aff_transformationS2<FT>           Aff_transformation;
public:
  PointS2() {}
  // PointS2(const Self &v); // use default copy
  PointS2(const Origin &) : _x(0), _y(0) {}
  PointS2(const FT &hx, const FT &hy, const FT &hw) : _x(hx/hw), _y(hy/hw) {}
  PointS2(const FT &x, const FT &y) : _x(x), _y(y) {}
  ~PointS2() {}

  Self &operator=(const Self &p) { _x = p._x; _y = p._y; return *this; }

  bool operator==(const Self &p)  const { return x()==p.x() && y()==p.y(); }
  bool operator==(const Origin &) const { return x()==FT(0) && y()==FT(0); }
  bool operator<(const Self& p) const 
    { return lexicographically_xy_smaller(*this, p); }
       
  int  id() const { return (int)this; }
       
  FT   hx() const { return x(); }
  FT   hy() const { return y(); }
  FT   hw() const { return FT(1); }
  FT   homogeneous(int i) const { return (i == 2) ? FT(1) : cartesian(i); }
              
  const FT&  x() const { return _x; }
  const FT&  y() const { return _y; }
  FT&  x() { return _x; }
  FT&  y() { return _y; }

  const FT&  cartesian(int i) const {
    CGAL_kernel_precondition( (i == 0) || (i == 1) );
    return (i == 0) ? x() : y();
  }

  FT&  cartesian(int i) {
    CGAL_kernel_precondition( (i == 0) || (i == 1) );
    return (i == 0) ? x() : y();
  }

  FT& operator[](int i) { return cartesian(i); }
  const FT& operator[](int i) const { return cartesian(i); }
       
  int  dimension() const { return 2; }

  Self& operator+=(const Vector &v) {
    _x += v.x(); _y+=v.y();
    return *this;
  }
  Self& operator-=(const Vector &v) {
    _x -= v.x(); _y-=v.y();
    return *this;
  }

  Bbox_2 bbox() const { return Bbox_2(x(),y(),x(),y()); }
  
  Self   transform(const Aff_transformation &t) const 
    { return t.transform(*this); }
  
private:
  FT   _x,_y;
};

CGAL_END_NAMESPACE

#include <CGAL/Origin.h>
#include <CGAL/SimpleCartesian/VectorS2.h>
#include <CGAL/SimpleCartesian/Aff_transformationS2.h>
#include <CGAL/Bbox_2.h>
#include <CGAL/number_utils.h>

CGAL_BEGIN_NAMESPACE

template < class FT >
inline
PointS2<FT> 
operator+(const PointS2<FT>& p, const VectorS2<FT>& v)
{ return PointS2<FT>(p.x() + v.x(), p.y() + v.y()) ; }

template < class FT >
inline
PointS2<FT> 
operator-(const PointS2<FT>& p, const VectorS2<FT>& v)
{ return PointS2<FT>(p.x() - v.x(), p.y() - v.y()) ; }

template < class FT >
inline
PointS2<FT> 
operator+(const Origin& , const VectorS2<FT>& v)
{ return PointS2<FT>(v) ; }

template < class FT >
inline
PointS2<FT> operator-(const Origin& , const VectorS2<FT>& v)
{ return PointS2<FT>(-v) ; }

template < class FT >
inline
VectorS2<FT> 
operator-(const PointS2<FT>& p, const PointS2<FT>& q)
{ return VectorS2<FT>(p.x() - q.x(), p.y() - q.y()) ; }

template < class FT >
inline
VectorS2<FT> 
operator-(const PointS2<FT>& p, const Origin& )
{ return VectorS2<FT>(p) ; }

template < class FT >
inline
VectorS2<FT> 
operator-(const Origin& , const PointS2<FT>& p)
{ return VectorS2<FT>(-p.x(), -p.y()) ; }


#ifndef CGAL_NO_OSTREAM_INSERT_POINTS2
template < class FT >
std::ostream& 
operator<<(std::ostream& os, const PointS2<FT>& p)
{
    switch(os.iword(IO::mode)) 
    {
    case IO::ASCII :
        return os << p.x() << ' ' << p.y();
    case IO::BINARY :
        write(os, p.x());
        write(os, p.y());
        return os;
    default:
        return os << "PointS2(" << p.x() << ", " << p.y() << ')';
    }
}
#endif // CGAL_NO_OSTREAM_INSERT_POINTS2

#ifndef CGAL_NO_ISTREAM_EXTRACT_POINTS2
template < class FT >
std::istream& 
operator>>(std::istream& is, PointS2<FT>& p)
{
    FT x, y;
    switch(is.iword(IO::mode)) 
    {
    case IO::ASCII :
        is >> x >> y;
        break;
    case IO::BINARY :
        read(is, x);
        read(is, y);
        break;
    default:
        CGAL_kernel_assertion_msg(false,"Stream must be in ascii or binary mode"); 
        // throw ios_base::failure("Stream must be in ascii or binary mode");
    }
    p = PointS2<FT>(x, y);
    return is;
}
#endif // CGAL_NO_ISTREAM_EXTRACT_POINTS2

CGAL_END_NAMESPACE

#endif // CGAL_OPT_SIMPLE_CARTESIAN_POINT_2_H







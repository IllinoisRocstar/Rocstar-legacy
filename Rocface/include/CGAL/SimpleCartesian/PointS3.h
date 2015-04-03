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
// $Id: PointS3.h,v 1.3 2008/12/06 08:43:27 mtcampbe Exp $

#ifndef CGAL_OPT_SIMPLE_CARTESIAN_POINT_3_H
#define CGAL_OPT_SIMPLE_CARTESIAN_POINT_3_H

CGAL_BEGIN_NAMESPACE

template < class FT >
class PointS3 {
public:
  typedef  PointS3<FT>                        Self;
  typedef  VectorS3<FT>                       Vector;
  typedef  Aff_transformationS3<FT>           Aff_transformation;

  PointS3() {}
  // PointS3(const Self &v); // use default copy
  PointS3(const Origin &) : _x(0), _y(0), _z(0) {}
  PointS3(const FT &hx, const FT &hy, const FT& hz, const FT &hw) 
    : _x(hx/hw), _y(hy/hw), _z(hz/hw) {}
  PointS3(const FT &x, const FT &y, const FT &z) : _x(x), _y(y), _z(z) {}
  ~PointS3() {}

  Self &operator=(const Self &p) {
    _x=p._x; _y=p._y; _z=p._z;
    return *this;
  }

  bool operator==(const Self &p)  const 
    { return x()==p.x() && y()==p.y() && z()==p.z(); }
  bool operator==(const Origin &) const 
    { return x()==FT(0) && y()==FT(0) && z()==FT(0); }
  bool operator<(const Self& p) const 
    { return lexicographically_xyz_smaller(*this, p); }
       
  int  id() const { return (int)this; }
       
  FT   hx() const { return x(); }
  FT   hy() const { return y(); }
  FT   hz() const { return z(); }
  FT   hw() const { return FT(1); }
  FT   homogeneous(int i) const { return (i == 3) ? FT(1) : cartesian(i); }

  const FT&  x() const { return _x; }
  const FT&  y() const { return _y; }
  const FT&  z() const { return _z; }
  FT&  x() { return _x; }
  FT&  y() { return _y; }
  FT&  z() { return _z; }

  const FT&  cartesian(int i) const {
    CGAL_kernel_precondition( i >= 0 && i <= 2 );
    return (i==0) ? x() :
           (i==1) ? y() : z() ;
  }

  FT&  cartesian(int i) {
    CGAL_kernel_precondition( i >= 0 && i <= 2 );
    return (i==0) ? x() :
           (i==1) ? y() : z() ;
  }

  FT& operator[](int i) { return cartesian(i); }
  const FT& operator[](int i) const { return cartesian(i); }
       
  int  dimension() const { return 3; }

  Self& operator+=(const Vector &v) {
    _x += v.x(); _y+=v.y(); _z+=v.z();
    return *this;
  }
  Self& operator-=(const Vector &v) {
    _x -= v.x(); _y-=v.y(); _z-=v.z();
    return *this;
  }
  Bbox_3 bbox() const { return Bbox_3(x(),y(),z(),x(),y(),z()); }

  Self   transform(const Aff_transformation &t) const
  { return t.transform(*this); }

protected:
  FT   _x,_y,_z;
};

CGAL_END_NAMESPACE

#include <CGAL/Origin.h>
#include <CGAL/SimpleCartesian/VectorS3.h>
#include <CGAL/SimpleCartesian/Aff_transformationS3.h>
#include <CGAL/Bbox_3.h>
#include <CGAL/number_utils.h>

CGAL_BEGIN_NAMESPACE



template < class FT >
inline
PointS3<FT>
operator+(const PointS3<FT>& p, const VectorS3<FT>& v)
{ return PointS3<FT>(p.x() + v.x(), p.y() + v.y(), p.z() + v.z()); }

template < class FT >
inline
PointS3<FT>
operator-(const PointS3<FT>& p, const VectorS3<FT>& v)
{ return PointS3<FT>(p.x() - v.x(), p.y() - v.y(), p.z() - v.z()); }

template < class FT >
inline
PointS3<FT>
operator+(const Origin& , const VectorS3<FT>& v)
{ return PointS3<FT>(v); }

template < class FT >
inline
PointS3<FT>
operator-(const Origin& , const VectorS3<FT>& v)
{ return PointS3<FT>(-v); }

template < class FT >
inline
VectorS3<FT>
operator-(const PointS3<FT>& p, const PointS3<FT>& q)
{ return VectorS3<FT>(p.x() - q.x(), p.y() - q.y(), p.z() - q.z()); }


template < class FT >
inline
VectorS3<FT>
operator-(const PointS3<FT>& p, const Origin& )
{ return VectorS3<FT>(p); }


template < class FT >
inline
VectorS3<FT>
operator-(const Origin& , const PointS3<FT>& p)
{ return VectorS3<FT>(-p.x(), -p.y(), -p.z()); }


#ifndef CGAL_NO_OSTREAM_INSERT_POINTS3
template < class FT >
std::ostream& operator<<(std::ostream& os, const PointS3<FT>& p)
{
    switch(os.iword(IO::mode)) {
    case IO::ASCII :
        return os << p.x() << ' ' << p.y()  << ' ' << p.z();
    case IO::BINARY :
        write(os, p.x());
        write(os, p.y());
        write(os, p.z());
        return os;
    default:
        os << "PointS3(" << p.x() << ", " << p.y()  << ", " << p.z() << ")";
        return os;
    }
}
#endif // CGAL_NO_OSTREAM_INSERT_POINTS3

#ifndef CGAL_NO_ISTREAM_EXTRACT_POINTS3
template < class FT >
std::istream& operator>>(std::istream& is, PointS3<FT>& p)
{
    FT x, y, z;
    switch(is.iword(IO::mode)) {
    case IO::ASCII :
        is >> x >> y >> z;
        break;
    case IO::BINARY :
        read(is, x);
        read(is, y);
        read(is, z);
        break;
    default:
        CGAL_kernel_assertion_msg(false,"Stream must be in ascii or binary mode"
); 
        // throw ios_base::failure("Stream must be in ascii or binary mode");
        break;
    }
    p = PointS3<FT>(x, y, z);
    return is;
}
#endif // CGAL_NO_ISTREAM_EXTRACT_POINTS3


CGAL_END_NAMESPACE

#endif // CGAL_OPT_SIMPLE_CARTESIAN_POINT_3_H







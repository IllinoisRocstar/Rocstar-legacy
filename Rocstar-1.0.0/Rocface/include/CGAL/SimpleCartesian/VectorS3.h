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
// $Id: VectorS3.h,v 1.3 2008/12/06 08:43:27 mtcampbe Exp $

#ifndef CGAL_OPT_SIMPLE_CARTESIAN_VECTOR_3_H
#define CGAL_OPT_SIMPLE_CARTESIAN_VECTOR_3_H

CGAL_BEGIN_NAMESPACE

template < class FT >
class VectorS3 {
public:
  typedef VectorS3<FT>                            Self;
  typedef DirectionS3<FT>                         Direction;
  typedef Aff_transformationS3<FT>                Aff_transformation;

  VectorS3() {}
  // VectorS3(const Self &v); // use default copy
  VectorS3(const Null_vector &) : _x(0), _y(0), _z(0) {}
  VectorS3(const FT &hx, const FT &hy, const FT& hz, const FT &hw) 
    : _x(hx/hw), _y(hy/hw), _z(hz/hw) {}
  VectorS3(const FT &x, const FT &y, const FT &z) : _x(x), _y(y), _z(z) {}
  ~VectorS3() {}

  Self &operator=(const Self &v) {
    _x=v._x; _y=v._y; _z=v._z;
    return *this;
  }

  bool operator==(const Self &v) const 
    { return _x==v._x && _y==v._y && _z==v._z; }
  bool operator==(const Null_vector &) const 
    { return _x==FT(0) && _y==FT(0) && _z==FT(0); }
  bool operator!=(const Null_vector &) const 
    { return _x!=FT(0) || _y!=FT(0) || _z!=FT(0); }
       
  int  id() const { return (int)this; }
       
  FT   hx() const { return _x; }
  FT   hy() const { return _y; }
  FT   hz() const { return _z; }
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

  Self operator+(const Self &v) const 
    { return Self(_x + v._x, _y + v._y, _z + v._z); }
  Self operator-(const Self &v) const 
    { return Self(_x - v._x, _y - v._y, _z - v._z); }
  Self operator-(const Null_vector&) const { return *this; }
  Self operator-() const { return Self(-_x, -_y, -_z); }

  Self& operator+=(const Self &v) {
    _x += v._x; _y+=v._y; _z+=v._z;
    return *this;
  }
  Self& operator-=(const Self &v) {
    _x -= v._x; _y-=v._y; _z-=v._z;
    return *this;
  }

  FT   operator*(const Self &v) const 
    { return _x * v._x + _y * v._y + _z*v._z; }
  Self operator/(const FT &c)   const { return Self( _x/c, _y/c, _z/c); }

  Self& operator*=(const FT c)
    { _x *= c; _y *=c; _z *= c; return *this; }
  Self& operator/=(const FT c) 
    { _x /= c; _y /=c; _z /= c; return *this; }

  Self& operator*=(const Self &v)
    { _x *= v._x; _y *=v._y; _z *=v._z; return *this; }
  Self& operator/=(const Self &v) 
    { _x /= v._x; _y /=v._y; _z /=v._z; return *this; }

  Self transform(const Aff_transformation &t) const 
    { return t.transform(*this); }

  friend  class DirectionS3<FT>;
  const Direction& direction() const 
    { return reinterpret_cast<const Direction&>(*this); }

  FT squared_norm() const { return _x*_x+_y*_y+_z*_z; }
protected:
  FT   _x,_y,_z;
};

template < class FT >
inline 
VectorS3<FT> 
operator*(const FT& c, const VectorS3<FT>& w)
{ return VectorS3<FT>( c* w.x(), c * w.y(), c * w.z()) ; }

template < class FT >
VectorS3<FT> 
cross_product(const VectorS3<FT>& v, const VectorS3<FT>& w)
{
    return VectorS3<FT>( v.y() * w.z() - v.z() * w.y() ,
                         v.z() * w.x() - v.x() * w.z() ,
                         v.x() * w.y() - v.y() * w.x() );
}

template < class FT >
std::ostream &operator<<(std::ostream &os, const VectorS3<FT> &v)
{
  switch(os.iword(IO::mode)) {
  case IO::ASCII :
    return os << v.x() << ' ' << v.y()  << ' ' << v.z();
  case IO::BINARY :
    write(os, v.x());
    write(os, v.y());
    write(os, v.z());
    return os;
  default:
    os << "VectorS3(" << v.x() << ", " << v.y() <<  ", " << v.z() << ")";
    return os;
  }
}

template < class FT >
std::istream &operator>>(std::istream &is, VectorS3<FT> &v)
{
  switch(is.iword(IO::mode)) {
  case IO::ASCII :
    is >> v.x() >> v.y() >> v.z();
        break;
  case IO::BINARY :
    read(is, v.x());
    read(is, v.y());
    read(is, v.z());
    break;
  default:
    std::cerr << "\nStream must be in ascii or binary mode\n";
    break;
  }
  return is;
}

CGAL_END_NAMESPACE

#endif // CGAL_OPT_SIMPLE_CARTESIAN_VECTOR_3_H







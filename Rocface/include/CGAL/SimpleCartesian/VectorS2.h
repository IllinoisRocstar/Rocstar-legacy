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
// $Id: VectorS2.h,v 1.4 2008/12/06 08:43:27 mtcampbe Exp $

#ifndef CGAL_OPT_SIMPLE_CARTESIAN_VECTOR_2_H
#define CGAL_OPT_SIMPLE_CARTESIAN_VECTOR_2_H

CGAL_BEGIN_NAMESPACE

template <class FT>
class VectorS2 {
public:
  typedef VectorS2<FT>                          Self;
  typedef DirectionS2<FT>                       Direction;
  typedef Aff_transformationS2<FT>              Aff_transformation;

  VectorS2() {}
  // VectorS2(const Self &v); // use default copy
  VectorS2(const Null_vector &) : _x(0), _y(0) {}
  VectorS2(const FT &hx, const FT &hy, const FT &hw) : _x(hx/hw), _y(hy/hw) {}
  VectorS2(const FT &x, const FT &y) : _x(x), _y(y) {}
  ~VectorS2() {}

  Self &operator=(const Self &v) { _x=v._x; _y=v._y; return *this; } 

  bool operator==(const Self &v) const { return _x==v._x && _y==v._y; }
  bool operator==(const Null_vector &) const { return _x==FT(0) && _y==FT(0); }
       
  int  id() const { return (int)this; }
       
  FT   hx() const { return _x; }
  FT   hy() const { return _y; }
  FT   hw() const { return FT(1); }
  FT   homogeneous(int i) const { return (i == 2) ? FT(1) : cartesian(i); }

  const FT&  x() const { return _x; }
  const FT&  y() const { return _y; }
  FT&  x() { return _x; }
  FT&  y() { return _y; }

  const FT&  cartesian(int i) const {
    CGAL_kernel_precondition( (i == 0) || (i == 1) );
    return (i == 0) ? _x : _y;
  }

  FT&  cartesian(int i) {
    CGAL_kernel_precondition( (i == 0) || (i == 1) );
    return (i == 0) ? _x : _y;
  }

  FT& operator[](int i) { return cartesian(i); }
  const FT& operator[](int i) const { return cartesian(i); }
       
  int  dimension() const { return 2; }

  Self operator+(const Self &v) const { return Self(_x + v._x, _y + v._y); }
  Self operator-(const Self &v) const { return Self(_x - v._x, _y - v._y); }
  Self operator-(const Null_vector&) const { return *this; }
  Self operator-() const { return Self(-_x, -_y); }

  FT   operator*(const Self &v) const { return _x * v._x + _y * v._y; }
  Self operator/(const FT &c)   const { return Self( _x/c, _y/c); }


  Self& operator+=(const Self &v) {
    _x += v._x; _y+=v._y;
    return *this;
  }
  Self& operator-=(const Self &v) {
    _x -= v._x; _y-=v._y;
    return *this;
  }
  Self& operator*=(const FT c)
    { _x *= c; _y *=c; return *this; }
  Self& operator/=(const FT c) 
    { _x /= c; _y /=c; return *this; }

  Self& operator*=(const Self &v)
    { _x *= v._x; _y *=v._y; return *this; }
  Self& operator/=(const Self &v) 
    { _x /= v._x; _y /=v._y; return *this; }

  Self perpendicular(const Orientation &o) const {
    CGAL_kernel_precondition( o != COLLINEAR );
    if (o == COUNTERCLOCKWISE) return Self(-_y, _x); 
    else return Self(_y, -_x);
  }
  Self transform(const Aff_transformation &t) const 
    { return t.transform(*this); }

  friend  class DirectionS2<FT>;
  const Direction& direction() const 
    { return reinterpret_cast<const Direction&>(*this); }
private:
  FT   _x,_y;
};

template < class _RR >
inline VectorS2<_RR> operator*(const _RR &c, const VectorS2<_RR> &v) { 
  return VectorS2<_RR>( c* v.x(), c * v.y()); 
}

template < class _RR >
std::ostream &operator<<(std::ostream &os, const VectorS2<_RR> &v) {
  switch(os.iword(IO::mode)) {
  case IO::ASCII :
    return os << v.x() << ' ' << v.y();
  case IO::BINARY :
    write(os, v.x());
    write(os, v.y());
    return os;
  default:
    return os << "VectorS2(" << v.x() << ", " << v.y() << ')';
  }
}

template < class _RR >
std::istream &operator>>(std::istream &is, VectorS2<_RR > &v) {
  switch(is.iword(IO::mode)) {
  case IO::ASCII :
    is >> v.x() >> v.y();
    break;
  case IO::BINARY :
    read(is, v.x());
    read(is, v.y());
    break;
  default:
    std::cerr << "\nStream must be in ascii or binary mode\n";
    break;
  }
  return is;
}

CGAL_END_NAMESPACE

#endif // CGAL_OPT_SIMPLE_CARTESIAN_VECTOR_2_H







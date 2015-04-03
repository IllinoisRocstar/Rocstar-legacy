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
// $Id: Vector_n.h,v 1.5 2008/12/06 08:43:26 mtcampbe Exp $

//===============================================================
// This file defines an adapator to cast an array into a vector
//    that supports arithmetic operators. There are three adapators
//    defined: Array_n, for a non-const array of size n, Array_n_const,
//    for a const array of size n, and Vector_n, for dynamically-allocated
//    vector of size n.
//    
// Author: Xiangmin Jiao
// Creation date:   Jan. 6, 2001
//===============================================================

#ifndef RFC_VECTOR_N_H
#define RFC_VECTOR_N_H

#include "rfc_basic.h"
#include <functional>

RFC_BEGIN_NAME_SPACE

class Array_n;
class Vector_n;

// Adapator to map an array into a vector that supports arithmetic operators.
class Array_n_const {
public:
  typedef Array_n_const                   Self;
  typedef Real                            Value;
  typedef Real *                          Pointer;
  typedef Real const *                    Const_pointer;
  typedef Real &                          Reference;
  typedef const Real&                     Const_reference;
  typedef unsigned int                    Size;

  friend class Array_n;
  friend class Vector_n;
  friend bool operator==( const Array_n_const &v1, const Array_n_const& v2);
  friend Vector_n operator+( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n operator-( const Array_n_const& v1, const Array_n_const& v2);
  friend Real operator*( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n operator*( Real a, const Array_n_const& v1);
  friend Vector_n operator*( const Array_n_const& v1, Real a);
  friend Vector_n operator/( const Array_n_const& v1, Real a);
  friend Vector_n multiply( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n divide( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n min( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n max( const Array_n_const& v1, const Array_n_const& v2);
  
  Array_n_const() : _start(0), _end(0) {}
  Array_n_const( const Real *v, Size n) : _start(v), _end(v+n) { }
  Array_n_const( const Real *v1, const Real *v2) : _start(v1), _end(v2) { }
  Array_n_const( const Self &v) : _start(v._start), _end(v._end) {}

  Const_reference operator[](Size i) const { return _start[i]; }

  Size dimension() const { return _end-_start; }

  Const_pointer begin() const { return _start; }
  Const_pointer end() const { return _end; }

  Real squared_norm() const {
    Real t=0;
    for ( const Real *p=_start; p!=_end; ++p) t += *p * *p;
    return t;
  }

protected:
  const Real *_start, *_end;

  Self &operator=( const Self&); 
};

class Array_n {
public:
  typedef Array_n                         Self;
  typedef Real                            Value;
  typedef Real *                          Pointer;
  typedef Real const *                    Const_pointer;
  typedef Real &                          Reference;
  typedef const Real&                     Const_reference;
  typedef unsigned int                    Size;

  friend class Vector_n;

  Array_n() : _start(0), _end(0) {}
  Array_n( Real *v, Size n) : _start(v), _end(v+n) {}
  Array_n( Real *v1, Real *v2) : _start(v1), _end(v2) {}
  Array_n( const Self &v) : _start(v._start), _end(v._end) {}

  operator const Array_n_const&() const 
  { return reinterpret_cast< const Array_n_const&>(*this); }

  Reference operator[](Size i) { return _start[i]; }
  Const_reference operator[](Size i) const { return _start[i]; }

  Size dimension() const { return _end-_start; }

  Const_pointer begin() const { return _start; }
  Const_pointer end() const   { return _end; }
  Pointer begin() { return _start; }
  Pointer end()   { return _end; }

  Real squared_norm() const { return ((Array_n_const*)this)->squared_norm(); }

  Self &operator=( const Array_n_const& v) {
    if ( _start != v._start) { // Check not assigning to itself.
      RFC_assertion( _end-_start == v._end-v._start);
      std::copy( v._start,v._end,_start);
    }
    return *this;
  }
  inline Self &operator=( const Self& v);
  inline Self &operator=( const Vector_n& v);
  
  Self& operator+=( const Array_n_const &v1) {
    RFC_assertion( _end-_start == v1._end-v1._start);
    const Real *q=v1._start;
    for ( Real *p=_start; p!=_end; ++p, ++q) *p += *q;
    return *this;
  }

  Self& operator-=( const Array_n_const &v1) {
    RFC_assertion( _end-_start == v1._end-v1._start);
    const Real *q=v1._start;
    for ( Real *p=_start; p!=_end; ++p, ++q) *p -= *q;
    return *this;
  }

  Self& operator*=( Real t) {
    for ( Real *p=_start; p!=_end; ++p) *p *= t;    return *this;
  }

  Self& operator*=( const Array_n_const &v1) {
    RFC_assertion( _end-_start == v1._end-v1._start);
    const Real *q=v1._start;
    for ( Real *p=_start; p!=_end; ++p, ++q) *p *= *q;
    return *this;
  }

  Self& operator/=( Real t) {
    for ( Real *p=_start; p!=_end; ++p) { *p = (t==0) ? 1. : *p / t; }
    return *this;
  }

  Self& operator/=( const Array_n_const &v1) {
    RFC_assertion( _end-_start == v1._end-v1._start);
    const Real *q=v1._start;
    for ( Real *p=_start; p!=_end; ++p, ++q) { *p = (*q==0) ? 1. : *p / *q; }
    return *this;
  }

  Self& invert() {
    for ( Real *p=_start; p!=_end; ++p) { *p = (*p==0) ? 0. : 1. / *p; }
    return *this;
  }

protected:
  Real *_start, *_end;
};

class Vector_n : public Array_n {
public:
  typedef Vector_n                        Self;
  typedef Array_n                         Base;
  typedef Real                            Value;
  typedef Real *                          Pointer;
  typedef Real const *                    Const_pointer;
  typedef Real &                          Reference;
  typedef const Real&                     Const_reference;
  typedef unsigned int                    Size;

  friend Vector_n operator+( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n operator-( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n operator*( Real a, const Array_n_const& v1);
  friend Vector_n operator*( const Array_n_const& v1, Real a);
  friend Vector_n operator/( const Array_n_const& v1, Real a);
  friend Vector_n multiply( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n divide( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n min( const Array_n_const& v1, const Array_n_const& v2);
  friend Vector_n max( const Array_n_const& v1, const Array_n_const& v2);

  Vector_n() {}
  explicit Vector_n( Size n) : Base( _values, n) { RFC_assertion( n<=MAX); }
  explicit Vector_n( Size n, Real x)  : Base( _values, n)
  { RFC_assertion(n<=MAX); std::fill( _start, _end, x); }
  Vector_n( const Vector_n &v) : Base( _values, v.dimension()) 
  { RFC_assertion(dimension()<=MAX); std::copy( v._start, v._end, _values); }
  
  operator const Array_n_const&() const {
    return reinterpret_cast<const Base&>(*this);
  }

  Self &operator=( const Array_n_const& v) {
    Base::operator=( v);
    return *this;
  }

  Self& operator+=( const Array_n_const &v) {
    Base::operator+=( v);
    return *this;
  }

  Self& operator-=( const Array_n_const &v) {
    Base::operator-=( v);
    return *this;
  }

  Self& operator*=( Real t) {
    Base::operator*=( t);
    return *this;
  }

  Self& operator*=( const Array_n_const &v1) {
    RFC_assertion( _end-_start == v1._end-v1._start);
    const Real *q=v1._start;
    for ( Real *p=_start; p!=_end; ++p, ++q) *p *= *q;
    return *this;
  }

  Self& operator/=( Real t) {
    Base::operator/=( t);
    return *this;
  }

  Self& operator/=( const Array_n_const &v) {
    Base::operator/=( v);
    return *this;
  }

  Self& invert() {
    Base::invert();
    return *this;
  }

protected:
  enum { MAX=9};
  Real _values[MAX];
  Vector_n( const Array_n_const &v) : Base( _values, v.dimension()) {
    RFC_assertion( v.dimension() <= MAX);
    std::copy( v._start,v._end,_values);
  }
};

Array_n &Array_n::operator=( const Array_n& v) {
  return *this = (const Array_n_const&)v;
}
Array_n &Array_n::operator=( const Vector_n& v) {
  return *this = (const Array_n_const&)v;
}

inline bool operator==( const Array_n_const &v1, const Array_n_const& v2) {
  if ( v1._end-v1._start != v2._end-v2._start) return false; 
  for ( const Real *p=v1._start, *q=v2._start; p!=v1._end; ++p,++q) {
    if ( *p != *q) return false;
  }
  return true; 
}

inline Vector_n operator+( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Vector_n v3( v1._end-v1._start);
  const Real *p=v1._start, *q=v2._start;
  for ( Real *r=v3._start; p!=v1._end; ++p, ++q, ++r) *r = *p + *q;
  return v3;
}

inline Vector_n operator-( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Vector_n v3( v1._end-v1._start);

  const Real *p=v1._start, *q=v2._start;
  for ( Real *r=v3._start; p!=v1._end; ++p, ++q, ++r)
    *r = *p - *q;
  return v3;
}

inline Real operator*( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Real t(0);

  for ( const Real *p=v1._start, *q=v2._start; p!=v1._end; ++p, ++q) 
    t += *p * *q;
  return t;
}

inline Vector_n operator*( Real a, const Array_n_const& v1) {
  Vector_n v2( v1._end-v1._start);
  const Real *p=v1._start;
  for ( Real *q=v2._start; p!=v1._end; ++p, ++q) *q = a * *p;
  return v2;
}

inline Vector_n operator*( const Array_n_const& v1, Real a) {
  Vector_n v2( v1._end-v1._start);
  const Real *p=v1._start;
  for ( Real *q=v2._start; p!=v1._end; ++p, ++q) *q = a * *p;
  return v2;
}

inline Vector_n operator/( const Array_n_const& v1, Real a) {
  Vector_n v2( v1._end-v1._start);
  const Real *p=v1._start;
  for ( Real *q=v2._start; p!=v1._end; ++p, ++q) *q = (a==0) ? 1.: *p / a;
  return v2;
}

inline Vector_n multiply( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Vector_n v3( v1._end-v1._start);
  const Real *p=v1._start, *q=v2._start;
  for ( Real *r=v3._start; p!=v1._end; ++p, ++q, ++r) *r = *p * *q;
  return v3;
}

inline Vector_n divide( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Vector_n v3( v1._end-v1._start);
  const Real *p=v1._start, *q=v2._start;
  for ( Real *r=v3._start; p!=v1._end; ++p, ++q, ++r) *r = (*q==0)? 1. : *p / *q;
  return v3;
}

inline Vector_n min( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Vector_n v3( v1._end-v1._start);
  const Real *p=v1._start, *q=v2._start;
  for ( Real *r=v3._start; p!=v1._end; ++p, ++q, ++r) *r = std::min(*p,*q);
  return v3;
}

inline Vector_n max( const Array_n_const& v1, const Array_n_const& v2) {
  RFC_assertion( v1._end-v1._start == v2._end-v2._start);
  Vector_n v3( v1._end-v1._start);
  const Real *p=v1._start, *q=v2._start;
  for ( Real *r=v3._start; p!=v1._end; ++p, ++q, ++r) *r = std::max(*p,*q);
  return v3;
}

std::ostream &operator<<( std::ostream &os,  const Array_n_const& v);

RFC_END_NAME_SPACE

#endif







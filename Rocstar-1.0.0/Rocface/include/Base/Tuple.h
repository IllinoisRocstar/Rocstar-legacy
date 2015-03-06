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
// $Id: Tuple.h,v 1.5 2008/12/06 08:43:26 mtcampbe Exp $

#ifndef RFC_TUPLE_H
#define RFC_TUPLE_H

#include "rfc_basic.h"

RFC_BEGIN_NAME_SPACE

template <class T>
struct Tuple_traits {
  typedef T                       Value;
  typedef const T                 Value_opposite;
  typedef T*                      Pointer;
  typedef const T*                Const_pointer;
  typedef T&                      Reference;
  typedef const T&                Init_reference;
  typedef const T&                Const_reference;
  typedef T*                      Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return *p; }
};

template <class T>
struct Tuple_traits<const T> {
  typedef const T                 Value;
  typedef T                       Value_opposite;
  typedef const T* const          Pointer;
  typedef const T* const          Const_pointer;
  typedef const T&                Reference;
  typedef const T&                Init_reference;
  typedef const T&                Const_reference;
  typedef const T*                Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return *p; }
};


template <class T>
struct Tuple_traits<T*> {
  typedef T*                      Value;
  typedef const T*                Value_opposite;
  typedef T**                     Pointer;
  typedef const T**               Const_pointer;
  typedef T*&                     Reference;
  typedef T*                      Init_reference;
  typedef const T*&               Const_reference;
  typedef T**                     Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return *p; }
};

template <class T>
struct Tuple_traits<const T*> {
  typedef const T*                Value;
  typedef T*                      Value_opposite;
  typedef T** const               Pointer;
  typedef const T** const         Const_pointer;
  typedef const T*&               Reference;
  typedef const T*                Init_reference;
  typedef const T*&               Const_reference;
  typedef const T**               Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return *p; }
};

template <class T>
struct Tuple_traits<T&> {
  typedef T&                      Value;
  typedef const T&                Value_opposite;
  typedef T*                      Pointer;
  typedef const T*                Const_pointer;
  typedef T&                      Reference;
  typedef T&                      Init_reference;
  typedef const T&                Const_reference;
  typedef T**                     Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return **p; }
};

template <class T>
struct Tuple_traits<const T&> {
  typedef const T&                Value;
  typedef T&                      Value_opposite;
  typedef T * const               Pointer;
  typedef const T * const         Const_pointer;
  typedef const T&                Reference;
  typedef const T&                Init_reference;
  typedef const T&                Const_reference;
  typedef const T**               Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return **p; }
};


template <class T>
struct Tuple_traits<T*&> {
  typedef T*&                     Value;
  typedef const T*&               Value_opposite;
  typedef T**                     Pointer;
  typedef const T**               Const_pointer;
  typedef T*&                     Reference;
  typedef T*&                     Init_reference;
  typedef const T*&               Const_reference;
  typedef T**                     Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return **p; }
};

template <class T>
struct Tuple_traits<const T*&> {
  typedef const T*&               Value;
  typedef T*&                     Value_opposite;
  typedef T** const               Pointer;
  typedef const T** const         Const_pointer;
  typedef const T*&               Reference;
  typedef const T*&               Init_reference;
  typedef const T*&               Const_reference;
  typedef const T**               Pointer_equivalent;
  static Reference ref(Pointer_equivalent p) { return **p; }
};

template < class T >
struct Two_tuple {
  typedef Tuple_traits<T>                       Traits;
  typedef typename Traits::Value                Value;
  typedef typename Traits::Value_opposite       Value_opposite;
  typedef typename Traits::Pointer              Pointer;
  typedef typename Traits::Const_pointer        Const_pointer;
  typedef typename Traits::Reference            Reference;
  typedef typename Traits::Const_reference      Const_reference;
  typedef typename Traits::Init_reference       Init_reference;
  typedef typename Traits::Pointer_equivalent   Pointer_equivalent;

  typedef Two_tuple<T>                          Self;
  typedef const Two_tuple<Value_opposite>       Const_self;

  Two_tuple() {}
  Two_tuple(Init_reference p, Init_reference q)
    : v0(p), v1(q) {}
  explicit Two_tuple( Const_reference t) : v0(t), v1(t) {}

  Const_reference operator[](int i) const {
    return Traits::ref(reinterpret_cast<Pointer_equivalent>
		       (const_cast<Self*>(this))+i);
  }
  Reference operator[](int i)  {
    return Traits::ref(reinterpret_cast<Pointer_equivalent>(this)+(i&1));
  }

  int  dimension() const { return 2; }
  int  id() const { return static_cast<int>(this); }
  bool operator==( const Self& x) const { return v0==x.v0 && v1==x.v1; }
  bool operator!=( const Self& x) const { return v0!=x.v0 || v1!=x.v1; }

  std::ostream& print(std::ostream &os, const char* s) const {
    switch(os.iword(IO::mode)) {
    case IO::ASCII :
      return os << v0 << ' ' << v1;
    case IO::BINARY :
      return os << v0 << v1;
    default:
      return os << s << "(" << v0 << ' ' << v1 << ' ' << ")";
    }
  }

  operator Const_self&() const
    { return reinterpret_cast<Const_self&>(*this); }

  Self  operator+( const Self& t) const 
  { return Self( v0+t.v0, v1+t.v1); }

  Self  operator-( const Self& t) const 
  { return Self( v0-t.v0, v1-t.v1); }

  Self& operator+=( const Self &t) 
  { v0+=t.v0; v1+=t.v1; return *this; }

  Self& operator-=( const Self &t) 
  { v0-=t.v0; v1-=t.v1; return *this; }

  Value operator*( const Self& t) const 
  { return v0*t.v0+v1*t.v1; }

  Self& operator*=( Const_reference t)
  {  v0*=t; v1*=t; return *this; }

  Self  operator/( Const_reference t) const 
  { return Self( v0/t, v1/t); }

  Self& operator/=( Const_reference t)
  {  v0/=t; v1/=t; return *this; }

  Self multiply(const Self &b) const {
    return Self( v0 *b.v0, v1 *b.v1);
  }

  Self divide(const Self &b) const {
    return Self( b.v0==Value(0) ? Value(0) : v0 /b.v0, 
		 b.v1==Value(0) ? Value(0) : v1 /b.v1);
  }

protected:
  Value  v0;
  Value  v1;
};


template < class T >
std::ostream &operator<<(std::ostream &os, const Two_tuple<T> &s) 
{
  return s.print(os, "Two_tuple");
}


template < class T >
std::istream &operator>>(std::istream &is, Two_tuple<T> &s)
{
  is >> s[0] >> s[1];
  return is;
}

template < class T >
Two_tuple<T> operator*(const T &a, const Two_tuple<T> &s)
{
  return Two_tuple<T>( a *s[0], a*s[1]);
}

template < class T >
struct Three_tuple {
  typedef Tuple_traits<T>                       Traits;
  typedef typename Traits::Value                Value;
  typedef typename Traits::Value_opposite       Value_opposite;
  typedef typename Traits::Pointer              Pointer;
  typedef typename Traits::Const_pointer        Const_pointer;
  typedef typename Traits::Reference            Reference;
  typedef typename Traits::Const_reference      Const_reference;
  typedef typename Traits::Init_reference       Init_reference;
  typedef typename Traits::Pointer_equivalent   Pointer_equivalent;

  typedef Three_tuple<T>                        Self;
  typedef const Three_tuple<Value_opposite>     Const_self;

  Three_tuple() {}
  Three_tuple(Init_reference p, Init_reference q, Init_reference r)
    : v0(p), v1(q), v2(r) {}
  explicit Three_tuple( Const_reference t) : v0(t), v1(t), v2(t) {}

  bool operator==( const Self& x) const
  { return v0==x.v0 && v1==x.v1 && v2==x.v2; }
  bool operator!=( const Self& x) const
  { return v0!=x.v0 || v1!=x.v1 || v2!=x.v2; }

  Const_reference operator[](int i) const {
    return Traits::ref(reinterpret_cast<Pointer_equivalent>
		       (const_cast<Self*>(this))+i);
  }
  Reference operator[](int i)  {
    return Traits::ref(reinterpret_cast<Pointer_equivalent>(this)+i);
  }

  int  id() const { return static_cast<int>(this); }
  int  dimension() const { return 3; }

  std::ostream& print(std::ostream &os, const char *s) const {
    switch(os.iword(IO::mode)) {
    case IO::ASCII :
      return os << v0 << ' ' << v1 << ' ' << v2;
    case IO::BINARY :
      return os << v0 << v1 << v2;
    default:
      return os << s << "(" << v0 << ' ' << v1 
		<< ' ' << v2 << ' ' << ")";
    }
  }
  
  operator Const_self&() const
    { return reinterpret_cast<Const_self&>(*this); }

  Self  operator+( const Self& t) const 
  { return Self( v0+t.v0, v1+t.v1, v2+t.v2); }

  Self  operator-( const Self& t) const 
  { return Self( v0-t.v0, v1-t.v1, v2-t.v2); }

  Self& operator+=( const Self &t) 
  { v0+=t.v0; v1+=t.v1; v2+=t.v2; return *this; }

  Self& operator-=( const Self &t) 
  { v0-=t.v0; v1-=t.v1; v2-=t.v2; return *this; }

  Value operator*( const Self& t) const 
  { return v0*t.v0+v1*t.v1+v2*t.v2; }

  Self& operator*=( Const_reference t)
  {  v0*=t; v1*=t; v2*=t; return *this; }

  Self  operator/( Const_reference t) const 
  { return Self( v0/t, v1/t, v2/t); }

  Self& operator/=( Const_reference t)
  {  v0/=t; v1/=t; v2/=t; return *this; }

  Self multiply(const Self &b) const {
    return Self( v0 *b.v0, v1 *b.v1, v2 *b.v2);
  }

  Self divide(const Self &b) const {
    return Self( b.v0==Value(0) ? Value(0) : v0 /b.v0, 
		 b.v1==Value(0) ? Value(0) : v1 /b.v1, 
		 b.v2==Value(0) ? Value(0) : v2 /b.v2);
  }
protected:
  Value  v0;
  Value  v1;
  Value  v2;
};


template < class T >
std::ostream &operator<<(std::ostream &os, const Three_tuple<T> &s) 
{
  return s.print(os, "Three_tuple");
}

template < class T >
std::istream &operator>>(std::istream &is, Three_tuple<T> &s)
{
  is >> s[0] >> s[1] >> s[2];
  return is;
}

template < class T >
Three_tuple<T> operator*(const T &a, const Three_tuple<T> &s)
{
  return Three_tuple<T>( a*s[0], a*s[1], a*s[2]);
}

template < class T >
struct Four_tuple {
  typedef Tuple_traits<T>                       Traits;
  typedef typename Traits::Value                Value;
  typedef typename Traits::Value_opposite       Value_opposite;
  typedef typename Traits::Pointer              Pointer;
  typedef typename Traits::Const_pointer        Const_pointer;
  typedef typename Traits::Reference            Reference;
  typedef typename Traits::Const_reference      Const_reference;
  typedef typename Traits::Init_reference       Init_reference;
  typedef typename Traits::Pointer_equivalent   Pointer_equivalent;

  typedef Four_tuple<T>                         Self;
  typedef const Two_tuple<Value_opposite>       Const_self;

  Four_tuple() {}
  Four_tuple(Init_reference p, Init_reference q, 
	     Init_reference r, Init_reference s)
    : v0(p), v1(q), v2(r), v3(s) {}
  explicit Four_tuple( Const_reference t) : v0(t), v1(t), v2(t), v3(t) {}

  bool operator==( const Self& x) const
  { return v0==x.v0 && v1==x.v1 && v2==x.v2 && v3==x.v3; }
  bool operator!=( const Self& x) const
  { return v0!=x.v0 || v1!=x.v1 || v2!=x.v2 || v3!=x.v3; }

  Const_reference operator[](int i) const {
    return Traits::ref(reinterpret_cast<Pointer_equivalent>
		       (const_cast<Self*>(this))+i);
  }
  Reference operator[](int i)  {
    return Traits::ref(reinterpret_cast<Pointer_equivalent>(this)+i);
  }

  int  id() const { return static_cast<int>(this); }
  int  dimension() const { return 4; }

  std::ostream& print(std::ostream &os, const char *s) const {
    switch(os.iword(IO::mode)) {
    case IO::ASCII :
      return os << v0 << ' ' << v1 << ' ' << v2 << ' ' << v3;
    case IO::BINARY :
      return os << v0 << v1 << v2 << v3;
    default:
      return os << s << "(" << v0 << ' ' << v1 
		<< ' ' << v2 << ' ' << v3 << ")";
    }
  }

  operator Const_self&() const
    { return reinterpret_cast<Const_self&>(*this); }

  Self  operator+( const Self& t) const 
  { return Self( v0+t.v0, v1+t.v1, v2+t.v2, v3+t.v3); }
  Self  operator-( const Self& t) const 
  { return Self( v0-t.v0, v1-t.v1, v2-t.v2, v3-t.v3); }
  Self& operator+=( const Self &t) 
  { v0+=t.v0; v1+=t.v1; v2+=t.v2; v3+=t.v3; return *this; }
  Self& operator-=( const Self &t) 
  { v0-=t.v0; v1-=t.v1; v2-=t.v2; v3-=t.v3; return *this; }
  Value operator*( const Self& t) const 
  { return v0*t.v0 + v1*t.v1 + v2*t.v2 + v3*t.v3; }

  Self& operator*=( Const_reference t)
  {  v0*=t; v1*=t; v2*=t; v3*=t; return *this; }

  Self  operator/( Const_reference t) const 
  { return Self( v0/t, v1/t, v2/t, v3/t); }

  Self& operator/=( Const_reference t)
  {  v0/=t; v1/=t; v2/=t; v3/=t; return *this; }

  Self multiply(const Self &b) const {
    return Self( v0 *b.v0, v1 *b.v1, v2 *b.v2, v3 *b.v3);
  }

  Self divide(const Self &b) const {
    return Self( b.v0==Value(0) ? Value(0) : v0 /b.v0, 
		 b.v1==Value(0) ? Value(0) : v1 /b.v1, 
		 b.v2==Value(0) ? Value(0) : v2 /b.v2, 
		 b.v3==Value(0) ? Value(0) : v3 /b.v3);
  }
protected:
  Value  v0;
  Value  v1;
  Value  v2;
  Value  v3;
};

template < class T >
std::ostream &operator<<(std::ostream &os, const Four_tuple<T> &s)
{
  return s.print(os, "Four_tuple");
}

template < class T >
std::istream &operator>>(std::istream &is, Four_tuple<T> &s)
{
  is >> s[0] >> s[1] >> s[2] >> s[3];
  return is;
}

template < class T >
Four_tuple<T> operator*(const T &a, const Four_tuple<T> &s)
{
  return Four_tuple<T>( a*s[0], a*s[1], a*s[2], a*s[3]);
}

RFC_END_NAME_SPACE

#endif // RFC_TUPLE_H







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
// $Id: In_place_list_n.h,v 1.3 2008/12/06 08:43:27 mtcampbe Exp $

// ======================================================================
//
// Copyright (c) 1997, 1998, 1999 The CGAL Consortium

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
//
// release       : CGAL-2.1
// release_date  : 2000, January 11
//
// file          : include/CGAL/In_place_list.h
// package       : STL_Extension (2.17)
// chapter       : CGAL_Chapter: STL Extensions for CGAL
// source        : stl_extension.fw
// revision      : Revision: 1.10
// revision_date : Date: 1999/11/10 13:37:40
// author(s)     : Michael Hoffmann
//                 Lutz Kettner
//
// coordinator   : INRIA, Sophia Antipolis
//
// A doubly linked list managing items in place.
// email         : cgal@cs.uu.nl
//
// ======================================================================

// ======================================================================
//
// Copyright (c) 2001 Xiangmin Jiao
//
// This file is modified from CGAL/In_place_list.h to support an
//   n-dimensional doubly-connected linked list managing items in place 
//   (where inserted items are not copied). This generalized data structure
//   allows an item to be in multiple In_place_list's at the same time.
//
// ======================================================================

#ifndef RFC_IN_PLACE_LIST_N_H
#define RFC_IN_PLACE_LIST_N_H 1

#include "rfc_basic.h"
#include <iterator>
#include <algorithm>
#include <cstddef>

RFC_BEGIN_NAME_SPACE

// Forward declarations
template <class T> class _In_place_list_n_iterator;
template <class T, bool managed=false> class In_place_list_n;

template < class T, int n=1>
struct In_place_list_n_base {
  In_place_list_n_base() 
  { for (int i=0; i<n; ++i) {next_link[i]=prev_link[i]=0;} }

  T* next_link[n];        // forward pointer
  T* prev_link[n];        // backwards pointer
};

template <class T>
class _In_place_list_n_iterator {
protected:
  T* node;
  int dim;
public:
  friend  class In_place_list_n<T,false>;
  friend  class In_place_list_n<T,true>;

  typedef _In_place_list_n_iterator<T> Self;

  typedef T               value_type;
  typedef T*              pointer;
  typedef T&              reference;
  typedef std::size_t     size_type;
  typedef std::ptrdiff_t  difference_type;
  typedef std::bidirectional_iterator_tag   iterator_category;

  explicit _In_place_list_n_iterator( int d=0) : node(0), dim(d) {}
  explicit _In_place_list_n_iterator(T* x, int d=0) : node(x), dim(d) {}

  // Use default copy constructor and copy assignment operator.

  bool  operator==( const Self& x) const { return node == x.node &&dim==x.dim;}
  bool  operator!=( const Self& x) const { return node != x.node ||dim!=x.dim;}
  T&    operator*()  const { return *node; }
  T*    operator->() const { return  node; }
  Self& operator++() {
    node = node->next_link[dim];
    return *this;
  }
  Self  operator++(int) {
    Self tmp = *this;
    ++*this;
    return tmp;
  }
  Self& operator--() {
    node = node->prev_link[dim];
    return *this;
  }
  Self  operator--(int) {
    Self tmp = *this;
    --*this;
    return tmp;
  }
};

#ifdef CGAL_CFG_NO_ITERATOR_TRAITS
#ifndef CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT
template < class T>
inline  std::bidirectional_iterator_tag
iterator_category( const  _In_place_list_n_iterator<T>&) {
  return std::bidirectional_iterator_tag();
}
template < class T>
inline  T*
value_type( const  _In_place_list_n_iterator<T>&) {
  return (T*)(0);
}
template < class T>
inline  std::ptrdiff_t*
distance_type( const  _In_place_list_n_iterator<T>&) {
  return (std::ptrdiff_t*)(0);
}
#endif // CGAL_LIMITED_ITERATOR_TRAITS_SUPPORT
#endif // CGAL_CFG_NO_ITERATOR_TRAITS //

template <class T, bool managed>
class In_place_list_n {
protected:
  T*           node;
  std::size_t  length;
  int          dim;

  T*   get_node()           { return new T;}
  T*   get_node(const T& t) { return new T(t);}
  void put_node(T* p)       { delete p;}

  //
  // Bidirectional List Managing Objects in Place
  // --------------------------------------------
  //
  // DEFINITION An object of the class In_place_list_n<T,bool> is a
  // sequence that supports bidirectional iterators and allows constant time
  // insert and erase operations anywhere within the sequence. The
  // functionality is similar to the `list<T>' in the STL.
  //
  // The In_place_list_n<T,bool> manages element items in place. Two
  // pointers `T*' are expected in the class. For example the base class
  // `In_place_list_n_base<T>' can be used.
  //
  // The In_place_list_n<T,bool> does not copy element items during
  // insertion (unless otherwise stated for a function). On removal or
  // destruction of the list the element items are not deleted by default.
  // The second template parameter `bool' has to be set to `false' in this
  // case. If the In_place_list_n<T,bool> should take the responsibility
  // for the stored objects the `bool' parameter could be set to `true', in
  // which case the list will delete removed items and will delete all
  // remaining items on destruction. In any case, the `destroy()' member
  // function deletes all elements.
  //
  // On purpose, these two possible versions of In_place_list_n<T,bool>
  // are not assignment compatible to avoid confusions between the different
  // storage responsibilities.
  //
  // PARAMETERS
  //
  // The full classname is `In_place_list_n<T,bool managed = false, T*
  // T::*next = &T::next_link, T* T::*prev = &T::prev_link>'. As long as no
  // default template arguments are supported, only
  // In_place_list_n<T,bool> is provided.
  //
  // TYPES

public:
  typedef T               value_type;
  typedef T*              pointer;
  typedef const T*        const_pointer;
  typedef T&              reference;
  typedef const T&        const_reference;
  typedef std::size_t     size_type;
  typedef std::ptrdiff_t  difference_type;

  typedef _In_place_list_n_iterator<T>          iterator;
  typedef _In_place_list_n_iterator<const T>    const_iterator;

  typedef std::reverse_iterator<iterator>       reverse_iterator;
  typedef std::reverse_iterator<const_iterator> const_reverse_iterator;

  typedef In_place_list_n<T,managed>  Self;

  // CREATION
  //
  // New creation variable is: `l'

  explicit In_place_list_n( int d=0) : length(0), dim(d) {
    // introduces an empty list.
    node = get_node();
    (*node).next_link[dim] = node;
    (*node).prev_link[dim] = node;
  }
  void set_dimension( int d) { dim = d; }

  void swap(Self& x) {
    std::swap(node, x.node);
    std::swap(length, x.length);
    std::swap(dim, x.dim);
  }

  // ACCESS MEMBER FUNCTIONS

  iterator       begin() { return iterator(node->next_link[dim],dim); }
  const_iterator begin() const { return const_iterator(node->next_link[dim],dim); }
  iterator       end() { return iterator(node,dim); }
  const_iterator end() const { return const_iterator(node,dim); }

  reverse_iterator       rbegin() { return reverse_iterator(end()); }
  const_reverse_iterator rbegin() const {
    return const_reverse_iterator(end());
  }
  reverse_iterator       rend() { return reverse_iterator(begin()); }
  const_reverse_iterator rend() const {
    return const_reverse_iterator(begin());
  }

  bool            empty() const    { return length == 0; }
  size_type       size() const     { return length; }
  size_type       max_size() const { return size_type(-1); }

  reference       front()          { return *begin(); }
  const_reference front() const    { return *begin(); }
  reference       back()           { return *node->prev_link[dim]; }
  const_reference back() const     { return *node->prev_link[dim]; }

  // INSERTION

  iterator insert(iterator position, T& x) {
    // inserts `t' in front of iterator `pos'. The return value points
    // to the inserted item.
    x.next_link[dim] = position.node;
    x.prev_link[dim] = (*position.node).prev_link[dim];
    (*((*position.node).prev_link[dim])).next_link[dim] = &x;
    (*position.node).prev_link[dim] = &x;
    ++length;
    return iterator(&x,dim);
  }
  iterator insert(T* pos, T& x) {
    return insert( iterator(pos,dim), x);
  }
  void push_front(T& x) { insert(begin(), x); }
  // inserts an item in front of list `l'.

  void push_back(T& x)  { insert(end(), x); }
  // inserts an item at the back of list `l'.

  void insert(iterator position, size_type n);
  // inserts n copies of `T()' in front of iterator `pos'.

  void insert(iterator position, size_type n, const T& x);
  // inserts n copies of `t' in front of iterator `pos'.

  void insert( T* pos, size_type n) { insert( iterator(pos,dim), n); }
  void insert( T* pos, size_type n, const T& x = T()) {
    insert( iterator(pos,dim), n, x);
  }

  template <class InputIterator>
  void insert(iterator pos, InputIterator first, InputIterator last) {
    // inserts the range [`first, last') in front of iterator `pos'.
    while (first != last)
      insert(pos, *get_node(*first++));
  }

  template <class InputIterator>
  void insert(T* pos, InputIterator first, InputIterator last) {
    // inserts the range [`first, last') in front of iterator `pos'.
    while (first != last)
      insert(pos, *get_node(*first++));
  }

  void insert(T* pos, const T* first, const T* last) {
    insert( iterator(pos,dim), const_iterator(first,dim),
            const_iterator(last,dim));
  }


  // REMOVAL

  void erase(iterator i) {
    // removes the item from list `l', where `pos' refers to.
    RFC_assertion( length > 0);
    (*((*i.node).prev_link[dim])).next_link[dim] = (*i.node).next_link[dim];
    (*((*i.node).next_link[dim])).prev_link[dim] = (*i.node).prev_link[dim];
    if (managed)
      put_node(i.node);
    --length;
  }
  void erase(T* pos)  { erase( iterator( pos,dim)); }

  void pop_front() { erase(begin()); }
  // removes the first item from list `l'.

  void pop_back() {
    // removes the last item from list `l'.
    iterator tmp = end();
    erase(--tmp);
  }

  void erase(iterator first, iterator last);
  // removes the items in the range [`first, last') from list `l'.

  void erase(T* first, T* last) {
    erase( iterator(first,dim), iterator(last,dim));
  }

  void clear() { erase( begin(), end()); }

  // CREATION (Continued)

  explicit In_place_list_n(size_type n, const T& value, int d=0) : length(0), dim(d) {
    // introduces a list with n items, all initialized with copies of
    // value.
    node = get_node();
    (*node).next_link[dim] = node;
    (*node).prev_link[dim] = node;
    insert(begin(), n, value);
  }

  template <class InputIterator>
  In_place_list_n( InputIterator first, InputIterator last, int d=0) : length(0), dim(d) {
    // a list with copies from the range [`first,last').
    node = get_node();
    (*node).next_link[dim] = node;
    (*node).prev_link[dim] = node;
    insert( begin(), first, last);
  }

  In_place_list_n(const T* first, const T* last, int d=0) : length(0), dim(d) {
    // a list with copies from the range [`first,last').
    node = get_node();
    (*node).next_link[dim] = node;
    (*node).prev_link[dim] = node;
    insert(begin(), first, last);
  }
  In_place_list_n(const Self& x) : length(0), dim(x.dim) {
    // copy constructor. Each item in `l1' is copied.
    node = get_node();
    (*node).next_link[dim] = node;
    (*node).prev_link[dim] = node;
    insert(begin(), x.begin(), x.end());
  }
  ~In_place_list_n() {
    erase(begin(), end());
    put_node(node);
  }

  Self& operator=(const Self& x);

  void destroy();

  template <class InputIterator>
  void assign( InputIterator first, InputIterator last) {
    erase( begin(), end());
    insert( begin(), first, last);
  }

  void assign( size_type n, const T& t) {
    erase( begin(), end());
    insert( begin(), n, t);
  }

  void resize( size_type sz, T c = T()) {
    if ( sz > size())
      insert( end(), sz - size(), c);
    else if ( sz < size()) {
      iterator i = begin();
      while ( sz-- > 0)
        ++i;
      erase( i, end());
    }  // else do nothing
  }

  // COMPARISON OPERATIONS

  bool operator==( const Self& y) const {
    return size() == y.size() && std::equal(begin(), end(), y.begin());
  }

  bool operator!=( const Self& y) const {
    return size() != y.size() || ! std::equal(begin(),end(),y.begin());
  }

  bool operator<(const Self& y) const {
    return std::lexicographical_compare( begin(),end(),
                                         y.begin(),y.end());
  }
  bool operator> ( const Self& i) const { return i < *this; }
  bool operator<=( const Self& i) const { return !(i < *this); }
  bool operator>=( const Self& i) const { return !(*this < i); }

  // SPECIAL LIST OPERATIONS

protected:
  void transfer(iterator position, iterator first, iterator last) {
    // move the range [`first, last') before the position.
    (*((*last.node).prev_link[dim])).next_link[dim] = position.node;
    (*((*first.node).prev_link[dim])).next_link[dim] = last.node;
    (*((*position.node).prev_link[dim])).next_link[dim] = first.node;
    T* tmp = (*position.node).prev_link[dim];
    (*position.node).prev_link[dim] = (*last.node).prev_link[dim];
    (*last.node).prev_link[dim] = (*first.node).prev_link[dim];
    (*first.node).prev_link[dim] = tmp;
  }

public:
  void splice(iterator position, Self& x) {
    // inserts the list x before position `pos' and x becomes empty.
    // It takes constant time. Precondition: `&l != &x'.
    if (!x.empty()) {
      transfer(position, x.begin(), x.end());
      length += x.length;
      x.length = 0;
    }
  }
  void splice(T* position, Self& x) {
    splice( iterator(position,dim), x);
  }
  void splice( iterator position, Self& x, iterator i) {
    // inserts an element pointed to by i from list x before position
    // `pos' and removes the element from x. It takes constant time. i
    // is a valid dereferenceable iterator of x. The result is
    // unchanged if `pos == i' or `pos == ++i'.
    iterator j = i;
    if (position == i || position == ++j) return;
    transfer(position, i, j);
    ++length;
    --x.length;
  }
  void splice(T* position, Self& x, T* i) {
    splice( iterator(position,dim), x, iterator(i,dim));
  }
  void splice(iterator pos, Self& x, iterator first, iterator last) {
    // inserts elements in the range [`first, last') before position
    // `pos' and removes the elements from x. It takes constant time
    // if `&x == $l'; otherwise, it takes linear time. [`first,
    // last') is a valid range in x. Precondition: `pos' is not in the
    // range [`first, last').
    if (first != last) {
      if (&x != this) {
        difference_type n = 0;
        std::distance(first, last, n);
        x.length -= n;
        length += n;
      }
      transfer(pos, first, last);
    }
  }
  void splice(T* p, Self& x, T* first, T* last) {
    splice( iterator(p,dim), x, iterator(first,dim), iterator(last,dim));
  }

  void remove(const T& value);
  // erases all elements e in the list l for which `e == value'.
  // It is stable. Precondition: a suitable `operator==' for the
  // type T.

  void reverse();
  // reverses the order of the elements in `l' in linear time.

  void unique();
  // erases all but the first element from every consecutive group
  // of equal elements in the list `l'. Precondition: a suitable
  // `operator==' for the type T.

  void merge(Self& x);
  // merges the list x into the list `l' and x becomes empty. It is
  // stable. Precondition: Both lists are increasingly sorted. A
  // suitable `operator<' for the type T.

  void sort();
  // sorts the list `l' according to the `operator<' in time O(n
  // log n) where `n = size()'. It is stable. Precondition: a
  // suitable `operator<' for the type T.
};

template <class T, bool managed>
void In_place_list_n<T,managed>::
insert(_In_place_list_n_iterator<T> position, std::size_t n) {
  while (n--)
    insert(position, *get_node());
}

template <class T, bool managed>
void In_place_list_n<T,managed>::
insert(_In_place_list_n_iterator<T> position, std::size_t n, const T& x) {
  while (n--)
    insert(position, *get_node(x));
}

template <class T, bool managed>
void In_place_list_n<T,managed>::
erase( _In_place_list_n_iterator<T> first,
       _In_place_list_n_iterator<T> last) {
  while (first != last)
    erase(first++);
}

template <class T, bool managed>
In_place_list_n<T,managed>&
In_place_list_n<T,managed>::
operator=(const In_place_list_n<T,managed>& x) {
  if (this != &x) {
    dim = x.dim;
    iterator first1 = begin();
    iterator last1  = end();
    const_iterator first2 = x.begin();
    const_iterator last2  = x.end();
    while (first1 != last1 && first2 != last2) {
      // Save the pointer values before assignment.
      // Assignment avoids unneccassary delete's and new's.
      T* tmp1 = (*first1).next_link[dim];
      T* tmp2 = (*first1).prev_link[dim];
      *first1 = *first2++;
      (*first1).next_link[dim] = tmp1;
      (*first1).prev_link[dim] = tmp2;
      ++first1;
    }
    if (first2 == last2)
      erase(first1, last1);
    else
      insert(last1, first2, last2);
  }
  return *this;
}

template <class T, bool managed>
void In_place_list_n<T,managed>::
destroy() {
  iterator first = begin();
  iterator last  = end();
  while( first != last) {
    iterator i = first++;
    put_node(i.node);
  }
  length = 0;
  (*node).next_link[dim] = node;
  (*node).prev_link[dim] = node;
}

template <class T, bool managed>
void In_place_list_n<T,managed>::remove(const T& value) {
  iterator first = begin();
  iterator last = end();
  while (first != last) {
    iterator next = first;
    ++next;
    if (*first == value)
      erase(first);
    first = next;
  }
}

template <class T, bool managed>
void In_place_list_n<T,managed>::reverse() {
  if (size() < 2) return;
  for (iterator first = ++begin(); first != end();) {
    iterator old = first++;
    transfer(begin(), old, first);
  }
}

template <class T, bool managed>
void In_place_list_n<T,managed>::unique() {
  iterator first = begin();
  iterator last = end();
  if (first == last) return;
  iterator next = first;
  while (++next != last) {
    if (*first == *next)
      erase(next);
    else
      first = next;
    next = first;
  }
}

template <class T, bool managed>
void In_place_list_n<T,managed>::merge(In_place_list_n<T,managed>& x) {
  iterator first1 = begin();
  iterator last1 = end();
  iterator first2 = x.begin();
  iterator last2 = x.end();
  while (first1 != last1 && first2 != last2)
    if (*first2 < *first1) {
      iterator next = first2;
      transfer(first1, first2, ++next);
      first2 = next;
    } else
      ++first1;
  if (first2 != last2)
    transfer(last1, first2, last2);
  length += x.length;
  x.length= 0;
}

template <class T, bool managed>
void In_place_list_n<T,managed>::sort() {
  if (size() < 2) return;
  In_place_list_n<T,managed> carry;
  In_place_list_n<T,managed> counter[64];
  int fill = 0;
  while (!empty()) {
    carry.splice(carry.begin(), *this, begin());
    int i = 0;
    while(i < fill && !counter[i].empty()) {
      counter[i].merge(carry);
      carry.swap(counter[i++]);
    }
    carry.swap(counter[i]);
    if (i == fill)
      ++fill;
  }
  for (int i = 1; i < fill; ++i)
    counter[i].merge(counter[i-1]);
  swap(counter[fill-1]);
}


// Undef shorter names (g++/egcs)
//#undef _In_place_list_n_iterator

RFC_END_NAME_SPACE
#endif // RFC_IN_PLACE_LIST_N_H //
// EOF //







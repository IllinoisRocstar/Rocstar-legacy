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
// $Id: Bbox_2.h,v 1.6 2008/12/06 08:43:26 mtcampbe Exp $

#ifndef CGAL_OPT_BBOX_2_H
#define CGAL_OPT_BBOX_2_H

#include <CGAL/basic.h>
#include <cmath>

CGAL_BEGIN_NAMESPACE

class Bbox_2 
{
public:
  Bbox_2() {
    _min[0] = _min[1] = HUGE_VAL;
    _max[0] = _max[1] = -HUGE_VAL;
  }
  Bbox_2(double x_min, double y_min, double x_max, double y_max) 
  { _min[0]=x_min; _min[1]=y_min; _max[0]=x_max; _max[1]=y_max; }

  // Bbox_2(const Bbox_2 &);
  // Bbox_2 &operator=(const Bbox_2 &b);
  // bool operator==(const Bbox_2 &b) const;
  // bool operator!=(const Bbox_2 &b) const;

  int     dimension() const { return 2; }
  double  xmin() const { return _min[0]; }
  double  ymin() const { return _min[1]; }
  double  xmax() const { return _max[0]; }
  double  ymax() const { return _max[1]; }


  double min(int i) const {
    CGAL_kernel_precondition( (i == 0 ) || ( i == 1 ) );
    if(i == 0) {
      return xmin();
    }
    return ymin();
  }

  double max(int i) const {
    CGAL_kernel_precondition( (i == 0 ) || ( i == 1 ) );
    if(i == 0) {
      return xmax();
    }
    return ymax();
  }

  Bbox_2  operator+(const Bbox_2& b) const {
    Bbox_2 box=*this;  box+=b;  return box;
  }

  Bbox_2&  operator+=(const Bbox_2& b) {
    _min[0] = std::min(_min[0], b._min[0]);
    _min[1] = std::min(_min[1], b._min[1]);

    _max[0] = std::max(_max[0], b._max[0]);
    _max[1] = std::max(_max[1], b._max[1]);

    return *this;
  }
private:
  double  _min[2],_max[2];
};

#ifndef NO_OSTREAM_INSERT_BBOX_2
inline
std::ostream&
operator<<(std::ostream &os, const Bbox_2 &b)
{
  switch(os.iword(IO::mode)) {
  case IO::ASCII :
    os << b.xmin() << ' ' << b.ymin() << ' '
       << b.xmax() << ' ' << b.ymax();
    break;
  case IO::BINARY :
    write(os, b.xmin());
    write(os, b.ymin());
    write(os, b.xmax());
    write(os, b.ymax());
    break;
  default:
    os << "Bbox_2(" << b.xmin() << ", " << b.ymin() << ", "
       << b.xmax() << ", " << b.ymax() << ")";
    break;
  }
  return os;
}
#endif // NO_OSTREAM_INSERT_BBOX_2



#ifndef NO_ISTREAM_EXTRACT_BBOX_2
inline
std::istream&
operator>>(std::istream &is, Bbox_2 &b)
{
  double xmin, ymin, xmax, ymax;

  switch(is.iword(IO::mode)) {
  case IO::ASCII :
    is >> xmin >> ymin >> xmax >> ymax;
    break;
  case IO::BINARY :
    read(is, xmin);
    read(is, ymin);
    read(is, xmax);
    read(is, ymax);
    break;
  }
  b = Bbox_2(xmin, ymin, xmax, ymax);
  return is;
}
#endif // NO_ISTREAM_EXTRACT_BBOX_2

inline bool do_overlap(const Bbox_2& bb1, const Bbox_2& bb2)
{
  // check for emptiness ??
  if (bb1.xmax() < bb2.xmin() || bb2.xmax() < bb1.xmin())
    return false;
  if (bb1.ymax() < bb2.ymin() || bb2.ymax() < bb1.ymin())
    return false;
  return true;
}

CGAL_END_NAMESPACE

#include <CGAL/cgalopt.h>

CGAL_OPT_BEGIN_NAMESPACE
using CGAL::Bbox_2;

inline bool do_overlap_eps(const Bbox_2& bb1, const Bbox_2& bb2, 
			   const double eps) {
  // check for emptiness ??
  if (bb1.xmax()+eps < bb2.xmin() || bb2.xmax()+eps < bb1.xmin())
    return false;
  if (bb1.ymax()+eps < bb2.ymin() || bb2.ymax()+eps < bb1.ymin())
    return false;
  return true;
}

inline bool do_overlap_strict(const Bbox_2& bb1, const Bbox_2& bb2)
{
  if (bb1.xmax() <= bb2.xmin() || bb2.xmax() <= bb1.xmin())
    return false;
  if (bb1.ymax() <= bb2.ymin() || bb2.ymax() <= bb1.ymin())
    return false;
  return true;
}

template <class InIter>
Bbox_2 get_Bbox_2(InIter first, InIter last) {
  Bbox_2  box;

  while (first != last) {
    box += first->bbox();
    ++first;
  }
  return box;
}

CGAL_OPT_END_NAMESPACE

#endif // CGAL_OPT_BBOX_2_H







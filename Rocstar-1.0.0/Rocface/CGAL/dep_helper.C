//----------------------------------------------------------------------//
// This is just a dummy program for extracting CGAL files.
//----------------------------------------------------------------------//
#include <CGAL/basic.h>

// Check version number.
#if ( CGAL_VERSION_NR < 1002000100 )
#error "CGAL version CGAL_VERSION is too old. Support only version 2.0 and newer."
#endif

#include <CGAL/Simple_cartesian.h>
#include <CGAL/Segment_2_Segment_2_intersection.h>

// Half-edge data structure
#include <CGAL/Halfedge_data_structure_using_vector.h>
#include <CGAL/Halfedge_data_structure_decorator.h>

// and Polyhedron
#include <CGAL/Polyhedron_incremental_builder_3.h>

// Squared-distance
#include <CGAL/distance_predicates_3.h>

// Miscellaneous
#include <CGAL/Range_tree_k.h>

// Standard C++ headers
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <cctype>
#include <istream>
#include <ostream>

int main()
{
  return 0;
}


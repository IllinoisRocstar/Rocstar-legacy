// ======================================================================
//
// Copyright (c) 1999 The CGAL Consortium

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
// release       : CGAL-2.2
// release_date  : 2000, September 30
//
// file          : include/CGAL/assertions.h
// package       : Kernel_basic (3.14)
// source        : 
// author(s)     : Geert-Jan Giezeman and Sven Sch�nherr
//
// coordinator   : MPI, Saarbruecken
//
// email         : contact@cgal.org
// www           : http://www.cgal.org
//
// ======================================================================

#ifndef CGAL_CONFIG_H
#include <CGAL/config.h>
#endif // CGAL_CONFIG_H

#ifndef CGAL_ASSERTIONS_H
#define CGAL_ASSERTIONS_H

CGAL_BEGIN_NAMESPACE

// types
// =====

enum Failure_behaviour { ABORT, EXIT, EXIT_WITH_SUCCESS, CONTINUE };

// function declarations
// =====================
// failure functions
// -----------------
void assertion_fail      ( const char*, const char*, int, const char*);
void precondition_fail   ( const char*, const char*, int, const char*);
void postcondition_fail  ( const char*, const char*, int, const char*);

// warning function
// ----------------
void warning_fail( const char*, const char*, int, const char*);


// macro definitions
// =================
// assertions
// ----------


#if defined(CGAL_KERNEL_NO_ASSERTIONS) || defined(CGAL_NO_ASSERTIONS) \
  || defined(NDEBUG)
#  define CGAL_assertion(EX) ((void)0)
#  define CGAL_assertion_msg(EX,MSG) ((void)0)
#  define CGAL_assertion_code(CODE)
#else
#  define CGAL_assertion(EX) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_assertion_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_assertion_code(CODE) CODE
#endif // CGAL_KERNEL_NO_ASSERTIONS

#if defined(CGAL_KERNEL_NO_ASSERTIONS) || defined(CGAL_NO_ASSERTIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || defined(NDEBUG)
#  define CGAL_exactness_assertion(EX) ((void)0)
#  define CGAL_exactness_assertion_msg(EX,MSG) ((void)0)
#  define CGAL_exactness_assertion_code(CODE)
#else
#  define CGAL_exactness_assertion(EX) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_exactness_assertion_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_exactness_assertion_code(CODE) CODE
#endif // CGAL_KERNEL_NO_ASSERTIONS

#if defined(CGAL_KERNEL_NO_ASSERTIONS) || defined(CGAL_NO_ASSERTIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_assertion(EX) ((void)0)
#  define CGAL_expensive_assertion_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_assertion_code(CODE)
#else
#  define CGAL_expensive_assertion(EX) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_assertion_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_assertion_code(CODE) CODE
#endif // CGAL_KERNEL_NO_ASSERTIONS

#if defined(CGAL_KERNEL_NO_ASSERTIONS) || defined(CGAL_NO_ASSERTIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_exactness_assertion(EX) ((void)0)
#  define CGAL_expensive_exactness_assertion_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_exactness_assertion_code(CODE)
#else
#  define CGAL_expensive_exactness_assertion(EX) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_exactness_assertion_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::assertion_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_exactness_assertion_code(CODE) CODE
#endif // CGAL_KERNEL_NO_ASSERTIONS


// preconditions
// -------------

#if defined(CGAL_KERNEL_NO_PRECONDITIONS) || defined(CGAL_NO_PRECONDITIONS) \
  || defined(NDEBUG)
#  define CGAL_precondition(EX) ((void)0)
#  define CGAL_precondition_msg(EX,MSG) ((void)0)
#  define CGAL_precondition_code(CODE)
#else
#  define CGAL_precondition(EX) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_precondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_precondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_PRECONDITIONS

#if defined(CGAL_KERNEL_NO_PRECONDITIONS) || defined(CGAL_NO_PRECONDITIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || defined(NDEBUG)
#  define CGAL_exactness_precondition(EX) ((void)0)
#  define CGAL_exactness_precondition_msg(EX,MSG) ((void)0)
#  define CGAL_exactness_precondition_code(CODE)
#else
#  define CGAL_exactness_precondition(EX) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_exactness_precondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_exactness_precondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_PRECONDITIONS

#if defined(CGAL_KERNEL_NO_PRECONDITIONS) || defined(CGAL_NO_PRECONDITIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_precondition(EX) ((void)0)
#  define CGAL_expensive_precondition_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_precondition_code(CODE)
#else
#  define CGAL_expensive_precondition(EX) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_precondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_precondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_PRECONDITIONS

#if defined(CGAL_KERNEL_NO_PRECONDITIONS) || defined(CGAL_NO_PRECONDITIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_exactness_precondition(EX) ((void)0)
#  define CGAL_expensive_exactness_precondition_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_exactness_precondition_code(CODE)
#else
#  define CGAL_expensive_exactness_precondition(EX) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_exactness_precondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::precondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_exactness_precondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_PRECONDITIONS


// postconditions
// --------------

#if defined(CGAL_KERNEL_NO_POSTCONDITIONS) || defined(CGAL_NO_POSTCONDITIONS) \
  || defined(NDEBUG)
#  define CGAL_postcondition(EX) ((void)0)
#  define CGAL_postcondition_msg(EX,MSG) ((void)0)
#  define CGAL_postcondition_code(CODE)
#else
#  define CGAL_postcondition(EX) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_postcondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_postcondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_POSTCONDITIONS

#if defined(CGAL_KERNEL_NO_POSTCONDITIONS) || defined(CGAL_NO_POSTCONDITIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || defined(NDEBUG)
#  define CGAL_exactness_postcondition(EX) ((void)0)
#  define CGAL_exactness_postcondition_msg(EX,MSG) ((void)0)
#  define CGAL_exactness_postcondition_code(CODE)
#else
#  define CGAL_exactness_postcondition(EX) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_exactness_postcondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_exactness_postcondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_POSTCONDITIONS

#if defined(CGAL_KERNEL_NO_POSTCONDITIONS) || defined(CGAL_NO_POSTCONDITIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_postcondition(EX) ((void)0)
#  define CGAL_expensive_postcondition_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_postcondition_code(CODE)
#else
#  define CGAL_expensive_postcondition(EX) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_postcondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_postcondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_POSTCONDITIONS

#if defined(CGAL_KERNEL_NO_POSTCONDITIONS) || defined(CGAL_NO_POSTCONDITIONS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_exactness_postcondition(EX) ((void)0)
#  define CGAL_expensive_exactness_postcondition_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_exactness_postcondition_code(CODE)
#else
#  define CGAL_expensive_exactness_postcondition(EX) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_exactness_postcondition_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::postcondition_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_exactness_postcondition_code(CODE) CODE
#endif // CGAL_KERNEL_NO_POSTCONDITIONS


// warnings
// --------

#if defined(CGAL_KERNEL_NO_WARNINGS) || defined(CGAL_NO_WARNINGS) \
  || defined(NDEBUG)
#  define CGAL_warning(EX) ((void)0)
#  define CGAL_warning_msg(EX,MSG) ((void)0)
#  define CGAL_warning_code(CODE)
#else
#  define CGAL_warning(EX) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_warning_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_warning_code(CODE) CODE
#endif // CGAL_KERNEL_NO_WARNINGS

#if defined(CGAL_KERNEL_NO_WARNINGS) || defined(CGAL_NO_WARNINGS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || defined(NDEBUG)
#  define CGAL_exactness_warning(EX) ((void)0)
#  define CGAL_exactness_warning_msg(EX,MSG) ((void)0)
#  define CGAL_exactness_warning_code(CODE)
#else
#  define CGAL_exactness_warning(EX) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_exactness_warning_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_exactness_warning_code(CODE) CODE
#endif // CGAL_KERNEL_NO_WARNINGS

#if defined(CGAL_KERNEL_NO_WARNINGS) || defined(CGAL_NO_WARNINGS) \
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_warning(EX) ((void)0)
#  define CGAL_expensive_warning_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_warning_code(CODE)
#else
#  define CGAL_expensive_warning(EX) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_warning_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_warning_code(CODE) CODE
#endif // CGAL_KERNEL_NO_WARNINGS

#if defined(CGAL_KERNEL_NO_WARNINGS) || defined(CGAL_NO_WARNINGS) \
  || (!defined(CGAL_KERNEL_CHECK_EXACTNESS) && !defined(CGAL_CHECK_EXACTNESS))\
  || (!defined(CGAL_KERNEL_CHECK_EXPENSIVE) && !defined(CGAL_CHECK_EXPENSIVE))\
  || defined(NDEBUG)
#  define CGAL_expensive_exactness_warning(EX) ((void)0)
#  define CGAL_expensive_exactness_warning_msg(EX,MSG) ((void)0)
#  define CGAL_expensive_exactness_warning_code(CODE)
#else
#  define CGAL_expensive_exactness_warning(EX) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, 0))
#  define CGAL_expensive_exactness_warning_msg(EX,MSG) \
   ((EX)?((void)0): ::CGAL::warning_fail( # EX , __FILE__, __LINE__, MSG))
#  define CGAL_expensive_exactness_warning_code(CODE) CODE
#endif // CGAL_KERNEL_NO_WARNINGS


// failure handler declarations
// ==========================
// failure handler
// ---------------
typedef
    void
    (*Failure_function)(
        const char*, const char*, const char*, int, const char*);

Failure_function
set_error_handler( Failure_function handler);

Failure_function
set_warning_handler( Failure_function handler);

// failure behaviour handler
// -------------------------
Failure_behaviour
set_error_behaviour(Failure_behaviour eb);

Failure_behaviour
set_warning_behaviour(Failure_behaviour eb);

CGAL_END_NAMESPACE

#endif // CGAL_ASSERTIONS_H


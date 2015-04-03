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
// General compiler_config file.

//+--------------------------------------------------------------------------
// The compiler has to provide a Standard Template Library
//+--------------------------------------------------------------------------
// STL test ok

//+--------------------------------------------------------------------------
//| The flag CGAL_CFG_CCTYPE_MACRO_BUG is set, if a compiler defines the
//| standard C library functions in cctype (isdigit etc.) as macros.
//| According to the standard they have to be functions.
//+--------------------------------------------------------------------------
#if (defined(__GNUC__) || defined(mips) || defined(__mips) || defined(_CRAYT3E))
#define CGAL_CFG_CCTYPE_MACRO_BUG 1
#endif

//+--------------------------------------------------------------------------
//| When a dynamic cast involves a pointer to a not yet instantiated 
//| template class, some compilers give an error.
//| This program is used to detect this problem.
//| "CGAL_CFG_DYNAMIC_CAST_BUG.C", line 45: error(3105): the type in a
//|           dynamic_cast must be a pointer or reference to a complete class
//|           type, or void *
//|     W< L<int> >* wl_ptr = dynamic_cast<W< L<int> >* >( &wp );
//|                                        ^
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && defined(__sparc))
#define CGAL_CFG_DYNAMIC_CAST_BUG 1
#endif

//+--------------------------------------------------------------------------
//| This flag is set, if the compiler does not match a function
//| argument of type typename T::A correctly.
//| (e.g. SGI 7.2)
//+--------------------------------------------------------------------------
//#define CGAL_CFG_MATCHING_BUG_1 1

//+--------------------------------------------------------------------------
//| This flag is set, if the compiler does not match the most
//| specialized instance of a function template correctly,
//| but complains about multiple matches.
//| (e.g. SGI 7.2)
//+--------------------------------------------------------------------------
//#define CGAL_CFG_MATCHING_BUG_2 1

//+--------------------------------------------------------------------------
//| When template implementation files are not included in the source files,
//| a compiler may attempt to find the unincluded template bodies
//| automatically. For example, suppose that the following conditions are
//| all true.
//|
//| - template entity ABC::f is declared in file xyz.h
//| - an instantiation of ABC::f is required in a compilation
//| - no definition of ABC::f appears in the source code processed by the
//|   compilation
//| 
//| In this case, the compiler may look to see if the source file xyz.n exists,
//| where n is .c, .C, .cpp, .CPP, .cxx, .CXX, or .cc. If this feature is
//| missing, the flag CGAL_CFG_NO_AUTOMATIC_TEMPLATE_INCLUSION is set.
//+--------------------------------------------------------------------------
#if (defined(__GNUC__))
#define CGAL_CFG_NO_AUTOMATIC_TEMPLATE_INCLUSION 1
#define CGAL_CFG_RETURN_TYPE_BUG_2 1
#endif

//+--------------------------------------------------------------------------
//| The byte order of a machine architecture distinguishes into
//| big-endian and little-endian machines.
//| The following definition is set if it is a little-endian machine.
//+--------------------------------------------------------------------------
#if (defined(__i386__)  || defined(__i386)  || defined(i386)) || \
    (defined(__ia64__)  || defined(__ia64)  || defined(ia64)) || \
    (defined(__amd64__) || defined(__amd64) || defined(__x86_64__))
#define CGAL_CFG_NO_BIG_ENDIAN 1
#elif (defined(__sparc__) || defined(__sparc) || defined(sparc)) || \
  defined(__APPLE__) || (defined(ppc64)) || (defined(mips) || defined(__mips)) || \
      (defined(_POWER) || defined(_CRAYT3E) || defined(__DECCXX)) 
// #else
// #error "Can't determine byte order--patch this"
#endif

//+--------------------------------------------------------------------------
//| This flag is set if the compiler doesn't support the operator dynamic_cast.
//+--------------------------------------------------------------------------
// #define CGAL_CFG_NO_DYNAMIC_CAST 1

//+--------------------------------------------------------------------------
//| If a compiler doesn't like explicit specification of 
//| template arguments in template function calls, the flag
//| CGAL_CFG_NO_EXPLICIT_TEMPLATE_FUNCTION_ARGUMENT_SPECIFICATION is set.
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && defined(__sparc))
#define CGAL_CFG_NO_EXPLICIT_TEMPLATE_FUNCTION_ARGUMENT_SPECIFICATION 1
#endif

//+--------------------------------------------------------------------------
//| Iterator traits are documented in the Dec. 1996 C++ Standard draft.
//| The following definition is set if iterator are not fully supported 
//| including their use in a template class, as a default template
//| argument and as a return type of global function.
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && defined(__sparc))
#define CGAL_CFG_NO_ITERATOR_TRAITS 1
#endif

//+--------------------------------------------------------------------------
//| This flag is set if the compiler doesn't support the operator Koenig
//| lookup. That is, it does not search in the namespace of the arguments for
//| the function.
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && (defined(mips) || defined(__mips)))
#define CGAL_CFG_NO_KOENIG_LOOKUP 1
#endif

//+--------------------------------------------------------------------------
//| If a compiler doesn't know the locale classic
//| CGAL_CFG_NO_LOCALE is set. 
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__))
#define CGAL_CFG_NO_LOCALE 1
#endif

//+--------------------------------------------------------------------------
//| If a compiler (or assembler or linker) has problems with long names
//| CGAL_CFG_NO_LONGNAME_PROBLEM is set.
//+--------------------------------------------------------------------------
//#define CGAL_CFG_NO_LONGNAME_PROBLEM 1

//+--------------------------------------------------------------------------
//| If a compiler doesn't know namespaces, the flag
//| CGAL_CFG_NO_NAMESPACE is set.
//+--------------------------------------------------------------------------
//#define CGAL_CFG_NO_NAMESPACE 1

//+--------------------------------------------------------------------------
//| If a compiler doesn't support partial specialisation of class templates,
//| the flag CGAL_CFG_NO_PARTIAL_CLASS_TEMPLATE_SPECIALISATION is set.
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && defined(__sparc))
#define CGAL_CFG_NO_PARTIAL_CLASS_TEMPLATE_SPECIALISATION 1
#endif

//+--------------------------------------------------------------------------
//| If a compiler doesn't like explicit partial specification of 
//| template arguments in template function calls, the flag
//| CGAL_CFG_NO_PARTIAL_TEMPLATE_FUNCTION_ARGUMENT_SPECIFICATION is set.
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && defined(__sparc))
#define CGAL_CFG_NO_PARTIAL_TEMPLATE_FUNCTION_ARGUMENT_SPECIFICATION 1
#endif

//+--------------------------------------------------------------------------
//| The parameter types of member functions might contain a scope
//| operator. This works as long as the member function is implemented
//| inline in the class. If the member function is implemented external
//| not all compilers are able to parse the scope operators correctly.
//| The following definition is set if the compiler fails parsing.
//+--------------------------------------------------------------------------
//#define CGAL_CFG_NO_SCOPE_MEMBER_FUNCTION_PARAMETERS 1

//+--------------------------------------------------------------------------
//| The flag CGAL_CFG_NO_STANDARD_HEADERS is set, if a compiler does not 
//| support the new standard headers (i.e. without the .h suffix).
//+--------------------------------------------------------------------------
#define CGAL_CFG_NO_STANDARD_HEADERS 1

//+--------------------------------------------------------------------------
//| The flag CGAL_CFG_NO_STDC_NAMESPACE is set, if a compiler does not
//| put the parts of the standard library inherited from the standard
//| C library in namespace std. (only tests for the symbols used in CGAL)
//+--------------------------------------------------------------------------
#if (!defined(__GNUC__) && (defined(__sparc__) || defined(mips) || defined(__mips)) || \
    (defined(_POWER) || defined(_CRAYT3E)))
#define CGAL_CFG_NO_STDC_NAMESPACE 1
#endif

//+--------------------------------------------------------------------------
//| The flag CGAL_CFG_NO_STDIO_NAMESPACE is set, if a compiler does not
//| put the IO standard library in namespace std.
//+--------------------------------------------------------------------------
//#define CGAL_CFG_NO_STDIO_NAMESPACE 1

//+--------------------------------------------------------------------------
//| If a compiler doesn't know the namespace std, the flag
//| CGAL_CFG_NO_STD_NAMESPACE is set. Some compilers know namespace std
//| but don't implement namespaces in general.
//+--------------------------------------------------------------------------
//#define CGAL_CFG_NO_STD_NAMESPACE 1

//+--------------------------------------------------------------------------
//| Checks whether the compiler wants to have a <> in friend declarations
//| of template functions.
//+--------------------------------------------------------------------------
#if (! defined(__GNUC__) && (defined(_POWER) || defined(_CRAYT3E)))
#define CGAL_CFG_NO_TEMPLATE_FRIEND_DISTINCTION 1
#endif

//+--------------------------------------------------------------------------
//| Nested templates in template parameter, such as 'template <
//| template <class T> class A>' are not supported by any compiler. 
//| The following definition is set if they are not supported.
//+--------------------------------------------------------------------------
#if (defined(__GNUC__) || defined(__sparc)) || \
    (defined(mips) || defined(__mips) || defined(_CRAYT3ER))
#define CGAL_CFG_NO_TMPL_IN_TMPL_PARAM 1
#endif

//+--------------------------------------------------------------------------
//| If a compiler complains about typename, when passing a dependent
//| type as template parameter, the flag CGAL_CFG_TYPENAME_BUG is set.
//+--------------------------------------------------------------------------
//#define CGAL_CFG_TYPENAME_BUG 1







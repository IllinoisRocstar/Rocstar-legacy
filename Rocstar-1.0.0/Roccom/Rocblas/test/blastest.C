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
// $Id: blastest.C,v 1.8 2008/12/06 08:43:19 mtcampbe Exp $

//  Name:   blastest.C
//  Author: Greg Mackey and Xiangmin Jiao
//  Date:   3/3/2002
//
//  Testing program for Rocblas.
//
//  Last modified by Xiangmin Jiao on 08/14/2002. Simplified the 
//       way of selecting test cases.

#include <iostream>
#include <stdio.h>
#include <cassert>
#include "roccom.h"
#include "Rocblas.h"

using namespace std;

#define NPANE 2
#define NROW  2
#define NCOL  3

//Function declarations.
void initData();
void printNodalData(const char *title);
void printStridedNodalData(const char *title);
void printBothNodalData(const char *title);
void printElementalData(const char *title);
void printStridedElementalData(const char *title);
void printBothElementalData(const char *title);

//Nodal data.
double testxn[NPANE][NROW*NCOL][3];
double testyn[NPANE][NROW*NCOL][3];
double testzn[NPANE][NROW*NCOL][3];

//Strided nodal data.
double testxns[NPANE][3][NROW*NCOL];
double testyns[NPANE][3][NROW*NCOL];
double testzns[NPANE][3][NROW*NCOL];

//Elemental data.
double testxe[NPANE][(NROW-1)*(NCOL-1)][3];
double testye[NPANE][(NROW-1)*(NCOL-1)][3];
double testze[NPANE][(NROW-1)*(NCOL-1)][3];

//Strided elemental data.
double testxes[NPANE][3][(NROW-1)*(NCOL-1)];
double testyes[NPANE][3][(NROW-1)*(NCOL-1)];
double testzes[NPANE][3][(NROW-1)*(NCOL-1)];

//Panel data.
double paneTestn[NPANE][NROW*NCOL];
double paneTeste[NPANE][(NROW-1)*(NCOL-1)];

//Scalar data.
double testscal = 10, s = 3;
double scalSPane[NPANE] = {4,6};
double scalWin[3] = {1,2,3};
double scalPane[NPANE][3] = {{1,2,3},{4,5,6}};

//Coordinate data.
int coord[NPANE][NROW*NCOL][3];

//Multiplicity data.
int nmults[NPANE][NROW*NCOL];
int emults[NPANE][(NROW-1)*(NCOL-1)];

int doSwap = 1;

int main(int argc, char *argv[]) {
   int choice1, choice2, choice3=0, choice4=0;
   int dims[2] = {NCOL, NROW};

   initData();

   std::cout << "Initializing Roccom" << std::endl;
   COM_init(&argc, &argv);

   std::cout << "Creating window \"twin\"" << std::endl;
   COM_new_window("twin");

   //Register normal attributes.
   COM_new_attribute("twin.xn", 'n', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.yn", 'n', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.zn", 'n', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.pn", 'n', COM_DOUBLE, 1, "m/s");
   COM_new_attribute("twin.mn", 'n', COM_INTEGER, 1, "");
   COM_new_attribute("twin.xe", 'e', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.ye", 'e', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.ze", 'e', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.pe", 'e', COM_DOUBLE, 1, "m/s");
   COM_new_attribute("twin.me", 'e', COM_INTEGER, 1, "");

   //Register staggered attributes.
   COM_new_attribute("twin.xns", 'n', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.yns", 'n', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.zns", 'n', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.xes", 'e', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.yes", 'e', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.zes", 'e', COM_DOUBLE, 3, "m/s");

   //Scalar attribute registration and initialization.
   COM_new_attribute("twin.aw", 'w', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.ap", 'p', COM_DOUBLE, 3, "m/s");
   COM_new_attribute("twin.asw", 'w', COM_DOUBLE, 1, "m/s");
   COM_new_attribute("twin.asp", 'p', COM_DOUBLE, 1, "m/s");

   COM_set_array("twin.aw", 0, &scalWin[0]);
   COM_set_array("twin.asw", 0, &testscal);

   for(int pid = 0; pid < NPANE; pid++) {
      //Initialize meshes.
      COM_set_size("twin.nc", pid+1, NROW*NCOL);
      COM_set_array("twin.nc", pid+1, &coord[pid][0][0]);
      COM_set_array("twin.:st2:", pid+1, &dims[0]);

      //Initialize normal attributes.
      COM_set_array("twin.xn", pid+1, &testxn[pid][0][0]);
      COM_set_array("twin.yn", pid+1, &testyn[pid][0][0]);
      COM_set_array("twin.zn", pid+1, &testzn[pid][0][0]);
      COM_set_array("twin.pn", pid+1, &paneTestn[pid][0]);
      COM_set_array("twin.mn", pid+1, &nmults[pid][0]);
      COM_set_array("twin.xe", pid+1, &testxe[pid][0][0]);
      COM_set_array("twin.ye", pid+1, &testye[pid][0][0]);
      COM_set_array("twin.ze", pid+1, &testze[pid][0][0]);
      COM_set_array("twin.pe", pid+1, &paneTeste[pid][0]);
      COM_set_array("twin.me", pid+1, &emults[pid][0]);

      //Initialize staggered attributes.
      COM_set_array("twin.xns", pid+1, &testxns[pid][0][0], 1);
      COM_set_array("twin.yns", pid+1, &testyns[pid][0][0], 1);
      COM_set_array("twin.zns", pid+1, &testzns[pid][0][0], 1);
      COM_set_array("twin.xes", pid+1, &testxes[pid][0][0], 1);
      COM_set_array("twin.yes", pid+1, &testyes[pid][0][0], 1);
      COM_set_array("twin.zes", pid+1, &testzes[pid][0][0], 1);

      //Initialize scalar attributes.
      COM_set_size("twin.ap", pid+1, 1);
      COM_set_array("twin.ap", pid+1, &scalPane[pid][0]);
      COM_set_size("twin.asp", pid+1, 1);
      COM_set_array("twin.asp", pid+1, &scalSPane[pid]);
   }

   COM_window_init_done("twin");

   Rocblas_load_module("TEST");

   std::string a1("twin.x"), a2("twin.y"), a3("twin.z"), ap("twin.p"), 
     aa("twin.a"), am("twin.m");

   //Output menus and read in choices.
   cout << "\nChoose type of data:\n\n"
        << "\t1) Nodal\n"
        << "\t2) Elemental\n\n"
        << "Enter your choice: ";

   cin >> choice1;
   if ( choice1 == 1) { 
     a1.append("n"); a2.append("n"); a3.append("n"); 
     ap.append("n"); am.append("n"); 
   }
   else if (choice1 == 2) { 
     a1.append("e"); a2.append("e"); a3.append("e"); 
     ap.append("e"); am.append("e"); 
   }

   cout << "\nChoose organization of data:\n\n"
        << "\t1) Contiguous\n"
        << "\t2) Strided\n"
        << "\t3) Contiguous -> Strided\n"
        << "\t4) Strided -> Contiguous\n\n"
        << "Enter your choice: ";

   cin >> choice2;
   if ( choice2 == 2)
   { a1.append("s"); a2.append("s"); a3.append("s"); }
   else if ( choice2 == 3)
   { a3.append("s"); }
   else if ( choice2 == 4)
   { a1.append("s"); a2.append("s"); }

   if ( choice1 != 3) {
     cout << "\nChoose data shape of second parameter:\n\n"
	  << "\t1) Vector / Matrix\n"
	  << "\t2) 1D Scalar (Windowed)\n"
	  << "\t3) 1D Scalar (Panel)\n"
	  << "\t4) 3D Scalar (Windowed)\n"
	  << "\t5) 3D Scalar (Panel)\n"
	  << "\t6) Scalar (Pointer)\n"
	  << "\t7) 1D Scalar (Windowed with multipliers)\n\n"
	  << "Enter your choice: ";
     
     cin >> choice3;
     if ( choice3 == 2) aa.append( "sw");
     else if ( choice3 == 3) aa.append( "sp");
     else if ( choice3 == 4) aa.append( "w");
     else if ( choice3 == 5) aa.append( "p");
     else if ( choice3 == 7) aa.append( "w");

     cout << "\nChoose which operation:\n\n"
	  << "\t1) Addition\n"
	  << "\t2) Subtraction\n"
	  << "\t3) Multiplication\n"
	  << "\t4) Division\n"
	  << "\t5) Swap\n"
	  << "\t6) Assign\n"
	  << "\t7) Dot\n"
	  << "\t8) 2-Norm\n"
	  << "\t9) axpy\n\n"
	  << "Enter your choice: ";
     
     cin >> choice4;
   }
   
   std::string op("TEST.");
   
   switch ( choice4) {
   case 1: op.append("add"); break;
   case 2: op.append("sub");  break;
   case 3: op.append("mul");  break;
   case 4: op.append("div");  break;
   case 5: op.append("swap");  break;
   case 6: op.append("copy");  break;
   case 7: op.append("dot");  break;
   case 8: op.append("nrm2");  break;
   default: op.append("axpy");
   }
   if ( choice3 == 6) op.append("_scalar");
   
   // Write out initial data
   if ( choice1 == 1 && choice2 == 1)
     printNodalData("Before:");
   else if ( choice1 == 1 && choice2 == 2)
     printStridedNodalData("Before:");
   else if ( choice1 == 1)
     printBothNodalData("Before:");
   else if ( choice1 == 2 && choice2 == 1)
     printElementalData("Before:");
   else if ( choice1 == 2 && choice2 == 2)
     printStridedElementalData("Before:");
   else if ( choice1 == 2)
     printBothElementalData("Before:");
   else {
     printBothNodalData("Before:");
     printBothElementalData("");
   }

   std::cout << "Performing operation " << op << std::endl;
   
   int func = COM_get_function_handle( op.c_str()); assert(func);
   int arg1 = COM_get_attribute_handle( a1.c_str()); assert(arg1);
   int arg2 = COM_get_attribute_handle( a2.c_str()); assert(arg2);
   int arg3 = COM_get_attribute_handle( a3.c_str()); assert(arg3);
   int arga = COM_get_attribute_handle( aa.c_str()); 
   int argm = COM_get_attribute_handle( am.c_str()); 
   int argp = COM_get_attribute_handle( ap.c_str()); 

   switch ( choice4) {
   case 0:
   case 5:
     COM_call_function( func, &arg1, &arg3); break;
   case 1:
   case 2:
   case 3:
   case 4:
     if ( choice3 == 2 || choice3 == 3 || choice3 == 4 || choice3 == 5) {
       COM_call_function( func, &arga, &arg1, &arg2); 
       COM_call_function( func, &arg1, &arga, &arg3); 
     }
     else if ( choice3 == 6) {
       COM_call_function( func, &arg1, &s, &arg2, &doSwap); 
       COM_call_function( func, &arg1, &s, &arg3, &doSwap); 
     }
     else
       COM_call_function( func, &arg1, &arg2, &arg3); 
     break;
   case 6:
     if ( choice3 == 2 || choice3 == 3 || choice3 == 4 || choice3 == 5) {
       COM_call_function( func, &arga, &arg3); 
     }
     else if ( choice3 == 6)
       COM_call_function( func, &s, &arg3); 
     else {
       COM_call_function( func, &arg1, &arg3);
     }
     break;
   case 7:
     if ( choice3 == 2 || choice3 == 3 || choice3 == 4 || choice3 == 5)
       COM_call_function( func, &arg1, &arg2, &arga);
     else if ( choice3 == 7)
       COM_call_function( func, &arg1, &arg2, &arga, &argm);
     else
     { std::cout << "Unavailable option" << std::endl; exit(-1); }
     break;
   case 8:
     if ( choice3 == 2 || choice3 == 3 || choice3 == 4 || choice3 == 5)
       COM_call_function( func, &arg2, &arga);
     else if ( choice3 == 7)
       COM_call_function( func, &arg2, &arga, &argm);
     else
       COM_call_function( func, &arg2, &argp);
     break;
   case 9:
     if ( choice3 == 2 || choice3 == 3 || choice3 == 4 || choice3 == 5)
       COM_call_function( func, &arga, &arg1, &arg2, &arg3);
     else if ( choice3 == 6)
       COM_call_function( func, &s, &arg1, &arg2, &arg3);
     else if ( choice3 == 1)
       COM_call_function( func, &arg1, &arg1, &arg2, &arg3);
     else
     { std::cout << "Unavailable option" << std::endl; exit(-1); }
     break;
   }

   // Write out solution
   if ( choice1 == 1 && choice2 == 1)
     printNodalData("After:");
   else if ( choice1 == 1 && choice2 == 2)
     printStridedNodalData("After:");
   else if ( choice1 == 1)
     printBothNodalData("After:");
   else if ( choice1 == 2 && choice2 == 1)
     printElementalData("After:");
   else if ( choice1 == 2 && choice2 == 2)
     printStridedElementalData("After:");
   else if ( choice1 == 2)
     printBothElementalData("After:");
   else {
     printBothNodalData("After:");
     printBothElementalData("");
   }

   std::cout << "\nDeleting windows" << std::endl;
   COM_delete_window("twin");

   Rocblas_unload_module( "TEST");
   std::cout << "Finalizing Roccom" << std::endl;
   COM_finalize();
}

//Initializes data.
void initData() {
   int i, j;

   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW*NCOL; j++) {
         //Initialize coordinate data.
         coord[i][j][0] = (i * NROW * NCOL) + j;
         coord[i][j][1] = j;
         coord[i][j][2] = 1;

         //Initialize contiguous nodal data.
         testxn[i][j][0] = j+1;
         testxn[i][j][1] = j+2;
         testxn[i][j][2] = j+3;
         testyn[i][j][0] = j+4;
         testyn[i][j][1] = j+5;
         testyn[i][j][2] = j+6;
         testzn[i][j][0] = 0;
         testzn[i][j][1] = 0;
         testzn[i][j][2] = 0;

         //Initialize staggered nodal data.
         testxns[i][0][j] = j+1;
         testxns[i][1][j] = j+2;
         testxns[i][2][j] = j+3;
         testyns[i][0][j] = j+4;
         testyns[i][1][j] = j+5;
         testyns[i][2][j] = j+6;
         testzns[i][0][j] = 0;
         testzns[i][1][j] = 0;
         testzns[i][2][j] = 0;

         //Initialize panel nodal data.
         paneTestn[i][j] = 4;

         //Initialize multiplicity data.
         nmults[i][j] = 2;
         //nmults[i][j] = i*NROW*NCOL + j + 1;
      }

      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         //Initialize contiguous elemental data.
         testxe[i][j][0] = j+1;
         testxe[i][j][1] = j+2;
         testxe[i][j][2] = j+3;
         testye[i][j][0] = j+4;
         testye[i][j][1] = j+5;
         testye[i][j][2] = j+6;
         testze[i][j][0] = 0;
         testze[i][j][1] = 0;
         testze[i][j][2] = 0;

         //Initialize staggered elemental data.
         testxes[i][0][j] = j+1;
         testxes[i][1][j] = j+2;
         testxes[i][2][j] = j+3;
         testyes[i][0][j] = j+4;
         testyes[i][1][j] = j+5;
         testyes[i][2][j] = j+6;
         testzes[i][0][j] = 0;
         testzes[i][1][j] = 0;
         testzes[i][2][j] = 0;

         //Initialize panel elemental data.
         paneTeste[i][j] = 4;

         //Initialize multiplicity data.
         emults[i][j] = 2;
         //emults[i][j] = i*(NROW-1)*(NCOL-1) + j + 1;
      }
   }
}

//Prints contiguous nodal data.
void printNodalData(const char *title) {
   int i, j, k;

   std::cout << "\n" << title << "\nx =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++) {
         for(k = 0; k < 3; k++)
            std::cout << testxn[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\ny =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++) {
         for(k = 0; k < 3; k++)
            std::cout << testyn[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nz =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++) {
         for(k = 0; k < 3; k++)
            std::cout << testzn[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n\np =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++)
         std::cout << paneTestn[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\naw =\n";
   for(i = 0; i < 3; i++)
      std::cout << scalWin[i] << " ";

   std::cout << "\n\nap =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++)
         std::cout << scalPane[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\nasw = " << testscal << "\n";

   std::cout << "\nasp =\n";
   for(i = 0; i < NPANE; i++)
      std::cout << scalSPane[i] << " ";

   std::cout << "\n\ns = " << s << "\n";
}

//Prints staggered nodal data.
void printStridedNodalData(const char *title) {
   int i, j, k;

   std::cout << "\n" << title << "\nxs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < NROW * NCOL; k++)
            std::cout << testxns[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nys =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < NROW * NCOL; k++)
            std::cout << testyns[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nzs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < NROW * NCOL; k++)
            std::cout << testzns[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n\np =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++)
         std::cout << paneTestn[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\naw =\n";
   for(i = 0; i < 3; i++)
      std::cout << scalWin[i] << " ";

   std::cout << "\n\nap =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++)
         std::cout << scalPane[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\nasw = " << testscal << "\n";

   std::cout << "\nasp =\n";
   for(i = 0; i < NPANE; i++)
      std::cout << scalSPane[i] << " ";

   std::cout << "\n\ns = " << s << "\n";
}

//Prints both contiguous and staggered nodal data.
void printBothNodalData(const char *title) {
   int i, j, k;

   std::cout << "\n" << title << "\nx =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++) {
         for(k = 0; k < 3; k++)
            std::cout << testxn[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\ny =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++) {
         for(k = 0; k < 3; k++)
            std::cout << testyn[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nz =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++) {
         for(k = 0; k < 3; k++)
            std::cout << testzn[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n" << "\nxs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < NROW * NCOL; k++)
            std::cout << testxns[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nys =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < NROW * NCOL; k++)
            std::cout << testyns[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nzs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < NROW * NCOL; k++)
            std::cout << testzns[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n\np =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < NROW * NCOL; j++)
         std::cout << paneTestn[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\naw =\n";
   for(i = 0; i < 3; i++)
      std::cout << scalWin[i] << " ";

   std::cout << "\n\nap =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++)
         std::cout << scalPane[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\nasw = " << testscal << "\n";

   std::cout << "\nasp =\n";
   for(i = 0; i < NPANE; i++)
      std::cout << scalSPane[i] << " ";

   std::cout << "\n\ns = " << s << "\n";
}

//Prints contiguous elemental data.
void printElementalData(const char *title) {
   int i, j, k;

   std::cout << "\n" << title << "\nx =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         for(k = 0; k < 3; k++)
            std::cout << testxe[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\ny =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         for(k = 0; k < 3; k++)
            std::cout << testye[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nz =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         for(k = 0; k < 3; k++)
            std::cout << testze[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n\np =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++)
         std::cout << paneTeste[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\naw =\n";
   for(i = 0; i < 3; i++)
      std::cout << scalWin[i] << " ";

   std::cout << "\n\nap =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++)
         std::cout << scalPane[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\nasw = " << testscal << "\n";

   std::cout << "\nasp =\n";
   for(i = 0; i < NPANE; i++)
      std::cout << scalSPane[i] << " ";

   std::cout << "\n\ns = " << s << "\n";
}

//Prints staggered elemental data.
void printStridedElementalData(const char *title) {
   int i, j, k;

   std::cout << "\n" << title << "\nxs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < (NROW-1)*(NCOL-1); k++)
            std::cout << testxes[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nys =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < (NROW-1)*(NCOL-1); k++)
            std::cout << testyes[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nzs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < (NROW-1)*(NCOL-1); k++)
            std::cout << testzes[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n\np =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++)
         std::cout << paneTeste[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\naw =\n";
   for(i = 0; i < 3; i++)
      std::cout << scalWin[i] << " ";

   std::cout << "\n\nap =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++)
         std::cout << scalPane[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\nasw = " << testscal << "\n";

   std::cout << "\nasp =\n";
   for(i = 0; i < NPANE; i++)
      std::cout << scalSPane[i] << " ";

   std::cout << "\n\ns = " << s << "\n";
}

//Prints both contiguous and staggered elemental data.
void printBothElementalData(const char *title) {
   int i, j, k;

   std::cout << "\n" << title << "\nx =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         for(k = 0; k < 3; k++)
            std::cout << testxe[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\ny =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         for(k = 0; k < 3; k++)
            std::cout << testye[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nz =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++) {
         for(k = 0; k < 3; k++)
            std::cout << testze[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n" << title << "\nxs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < (NROW-1)*(NCOL-1); k++)
            std::cout << testxes[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nys =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < (NROW-1)*(NCOL-1); k++)
            std::cout << testyes[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\nzs =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++) {
         for(k = 0; k < (NROW-1)*(NCOL-1); k++)
            std::cout << testzes[i][j][k] << " ";
         std::cout << "     ";
      }
      std::cout << "\n";
   }

   std::cout << "\n\np =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < (NROW-1)*(NCOL-1); j++)
         std::cout << paneTeste[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\naw =\n";
   for(i = 0; i < 3; i++)
      std::cout << scalWin[i] << " ";

   std::cout << "\n\nap =\n";
   for(i = 0; i < NPANE; i++) {
      for(j = 0; j < 3; j++)
         std::cout << scalPane[i][j] << " ";
      std::cout << "     ";
   }

   std::cout << "\n\nasw = " << testscal << "\n";

   std::cout << "\nasp =\n";
   for(i = 0; i < NPANE; i++)
      std::cout << scalSPane[i] << " ";

   std::cout << "\n\ns = " << s << "\n";
}







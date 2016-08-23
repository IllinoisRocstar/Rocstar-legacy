#!/bin/tcsh
# Regression test for FSI beam using parallel Elmer module and driver
# In this test Elmer essentially solves the problem of a 3D cantilever 
# beam subject to w=1 distributed load on top (in y-direction). The 
# beam span is along z-direction and cross section is b=0.1 and h=0.05 
# with length L = 1. The exact value for 1D equivalent beam difflection
# is delta=wL^4/(8*E*I) and I = 1/12*b*h^3 which is delta = 1.2e-6 .

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt 


if( -d ParElasticBeam3D_LoadFunction ) then
  echo "removing ParElasticBeam3D_LoadFunction directory"
  /bin/rm -r ParElasticBeam3D_LoadFunction
endif

mkdir ParElasticBeam3D_LoadFunction
cd ParElasticBeam3D_LoadFunction

cp -r  $2/share/Testing/test_data/ParElasticBeam3D_LoadFunction/* .

mpirun -np 2 $3/SolverModuleDriverParallel -com-mpi -fsi -loads 1.0

if( ! -e maxMinDisp.dat ) then
  echo "It appears parallel Elmer solver/module did not finish properly!"
  cd ..
  exit 1
endif

printf "ParElasticBeam3D_LoadFunction:Works=" >> ${TmpOut}

echo "DiffPath = $4"
set STEST0=`$4/diffdatafiles -p 2  maxMinDisp.dat maxMinDisp_ref.dat`
if( $status != 0 ) then
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  echo "Test Failed, results are different ..."
  echo "Here is the difference report : "
  echo "$STEST0"
  cd ..
  exit 1
endif  

if( "$STEST0" == "") then
  printf "1\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
else
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  exit 1
endif


exit 0

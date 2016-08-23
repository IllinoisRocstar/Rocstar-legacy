#!/bin/tcsh

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt 

# Regression test for FSI obstacle (steady state) case

if( -d Vortex ) then
  echo "removing Vortex directory"
  rm -r Vortex
endif

mkdir Vortex
cd Vortex

cp $2/share/Testing/test_data/Vortex/* .

$3/SolverModuleDriver -com-mpi 8.0

if( ! -e case.ep ) then
  echo "No case.ep results file from running Elmer"
  exit 1
endif

#removing run time stamp from output file
sed '2d' case.ep > case.ep.tmp

printf "VortexRegressionTest:Works=" >> ${TmpOut}

set STEST=`diff case.ep.tmp case.ep_check`
if( $status != 0 ) then
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  exit 1
endif  

if( "$STEST" == "") then
  printf "1\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  rm -r Vortex
else
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  rm -r Vortex
  exit 1
endif


exit 0

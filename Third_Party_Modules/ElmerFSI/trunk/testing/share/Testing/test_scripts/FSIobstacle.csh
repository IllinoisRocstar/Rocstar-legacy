#!/bin/tcsh

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt 

# Regression test for FSI obstacle (steady state) case

if( -d FSIobstacle ) then
  echo "removing FSIobstacle directory"
  rm -r FSIobstacle
endif

mkdir FSIobstacle
cd FSIobstacle

cp $2/share/Testing/test_data/FSIobstacle/* .

$3/SolverModuleDriver -com-mpi 5.0 10.0 19.0

if( ! -e case.ep ) then
  echo "No case.ep results file from running Elmer"
  exit 1
endif

#removing run time stamp from output file
sed '2d' case.ep > case.ep.tmp

printf "FSIregressionTest:Works=" >> ${TmpOut}
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
  rm -r FSIobstacle
else
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  rm -r FSIobstacle
  exit 1
endif

exit 0

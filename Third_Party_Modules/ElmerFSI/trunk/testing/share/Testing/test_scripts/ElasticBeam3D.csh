#!/bin/tcsh

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt 

# Regression test for FSI obstacle (steady state) case

if( -d ElasticBeam3D ) then
  echo "removing ElasticBeam3D directory"
  rm -r ElasticBeam3D
endif

mkdir ElasticBeam3D
cd ElasticBeam3D

cp -r  $2/share/Testing/test_data/ElasticBeam3D/* .

$3/SolverModuleDriver -com-mpi 1.0

if( ! -e case.ep ) then
  echo "No case.ep results file from running Elmer!"
  cd ..
  exit 1
endif

#removing run time stamp from output file
sed '2d' case.ep > case.ep.tmp

printf "ElasticBeam3DRegressionTest:Works=" >> ${TmpOut}

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
  rm -r ElasticBeam3D
else
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  rm -r ElasticBeam3D
  exit 1
endif


exit 0

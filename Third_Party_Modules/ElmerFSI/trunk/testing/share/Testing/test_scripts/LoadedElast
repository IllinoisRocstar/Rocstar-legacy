#!/bin/tcsh

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt 

# Regression test for FSI obstacle (steady state) case

if( -d LoadedElasticBeamParallel ) then
  echo "removing LoadedElasticBeamParallel directory"
  /bin/rm -r LoadedElasticBeamParallel
endif

mkdir LoadedElasticBeamParallel
cd LoadedElasticBeamParallel

cp -r  $2/share/Testing/test_data/LoadedElasticBeamParallel/* .

mpirun -np 4 $3/SolverModuleDriverParallel -com-mpi 1.0

if( ! -e beam3d/case.ep.0 ) then
  echo "No case.ep results file from running Elmer!"
  cd ..
  exit 1
endif

#removing run time stamp from output file
sed '2d' beam3d/case.ep.0 > case.ep.0.tmp
sed '2d' beam3d/case.ep.1 > case.ep.1.tmp
sed '2d' beam3d/case.ep.2 > case.ep.2.tmp
sed '2d' beam3d/case.ep.3 > case.ep.3.tmp


printf "ParLoadedElasticBeam:Works=" >> ${TmpOut}

echo "DiffPath = $4"
set STEST0=`$4/diffdatafiles -p 2  case.ep.0.tmp PureElmerOutput/case.ep.0_check`
if( $status != 0 ) then
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  exit 1
endif  
set STEST1=`$4/diffdatafiles -p 2  case.ep.1.tmp PureElmerOutput/case.ep.1_check`
if( $status != 0 ) then
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  exit 1
endif  
set STEST2=`$4/diffdatafiles -p 2  case.ep.2.tmp PureElmerOutput/case.ep.2_check`
if( $status != 0 ) then
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  exit 1
endif  
set STEST3=`$4/diffdatafiles -p 2  case.ep.3.tmp PureElmerOutput/case.ep.3_check`
if( $status != 0 ) then
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  exit 1
endif  

echo "STEST0 = $STEST0"

if( "$STEST0" == "" && "$STEST1" == "" && "$STEST2" == "" && "$STEST3" == "") then
  printf "1\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  #/bin/rm -r LoadedElasticBeamParallel
else
  printf "0\n" >> ${TmpOut}
  cat ${TmpOut} >> ../${OutFile}
  cd ..
  #/bin/rm -r LoadedElasticBeamParallel
  exit 1
endif


exit 0

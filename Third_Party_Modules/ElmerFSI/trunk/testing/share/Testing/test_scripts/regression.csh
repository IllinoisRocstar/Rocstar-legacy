#!/bin/tcsh

#Enter necessary filename variables here
set OutFile=$1
set TmpOut=${OutFile}_tmp.txt 
set InputDir=FSIobstacle
set Outputs=(case.ep out.dat)
set OutputsCheck=(case.ep_check out.dat_check)
set TestName=FSIregressionTest:Works

#Remove old test InputDir if present
if( -d  ${InputDir}) then
  echo "removing ${InputDir} directory"
  rm -r ${InputDir}
endif

#Make InputDir directory to run test in
mkdir ${InputDir}
cd ${InputDir}

#Copy input data into InputDir
cp $2/share/Testing/test_data/${InputDir}/* .

#Run executable to generate output data
$3/SolverModuleDriver -com-mpi 5.0 10.0 19.0

#Make sure the necesary output was generated
foreach file (${Outputs})
  if( ! -e ${file} ) then
    echo "No ${file} results file from run!"
    exit 1
  endif
end

#variable for test passing
@ result = 1

#diff the new output file with the saved one to check
printf "${TestName}=" >> ${TmpOut}
@ i = 1
foreach file (${Outputs})
  set STEST=`$4/diffdatafiles ${file} $OutputsCheck[$i] -t 1.0e-10 -n`
  if( $status != 0 ) then
    printf "0\n" >> ${TmpOut}
    cat ${TmpOut} >> ../${OutFile}
    cd ..
    exit 1
  endif  
  if("$STEST" != "") then
    echo "${file} differs from $OutputsCheck[$i]"
    @ result = 0
  endif
  @ i += 1
end

#print test results to OutFile
printf "$result\n" >> ${TmpOut}
cat ${TmpOut} >> ../${OutFile}
cd ..
#if( result == 1 ) then
#  rm -r ${InputDir}
#endif

exit 0

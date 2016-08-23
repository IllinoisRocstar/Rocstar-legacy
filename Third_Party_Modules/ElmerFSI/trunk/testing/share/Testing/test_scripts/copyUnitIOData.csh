#!/bin/tcsh

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt

# Test the IO of the program output
# copy the input files to a location where I can get to them from the unit test
set allWorks=1
mkdir IO_Unit_Test
cp $2/share/Testing/test_data/ioTest/testInputMultiComponent.inp IO_Unit_Test/.
if($? != 0) then
  allWorks=0
endif

cp $2/share/Testing/test_data/ioTest/component1.inp IO_Unit_Test/.
if($? != 0) then
  allWorks=0
endif

cp $2/share/Testing/test_data/ioTest/component2.inp IO_Unit_Test/.
if($? != 0) then
  allWorks=0
endif


printf "IOCopy:Works=" >> ${TmpOut}
printf "$allWorks\n" >> ${TmpOut}
cat ${TmpOut} >> ${OutFile}
rm -f ${TmpOut}

exit 0

#!/bin/tcsh

set OutFile=$1
set TmpOut=${OutFile}_tmp.txt

# Test the IO of the program output
$3/batterySim -v 3 $2/share/Testing/test_data/ioTest/testInput.inp > IO_test.txt

set allWorks=1

foreach line ("`cat $2/share/Testing/test_data/ioTest/testInput.inp`")
  foreach input ($line)
    if ( $input == "#" ) then
      break
    endif
#    echo ${input}
    set varName=`echo $input  | cut -d = -f 1`
    set varVal=`echo $input  | cut -d = -f 2`
#    echo $varName
#    echo $varVal
    set GTEST=`grep ${varName} IO_test.txt`   
#    echo $GTEST
    set outVarVal=`echo $GTEST | cut -d = -f 2` 
#    echo $outVarVal
    set Works = `echo "$varVal $outVarVal" | awk '{if ($1 == $2) print 1; else print 0}'`
#    echo "Works = $Works"
    if( "$GTEST" == "") then
      @ Works = 0
#      echo "Not Found"
#    else
#      echo "Found"
    endif
    if($Works == 0) then
      @ allWorks = 0
    endif      
  end
end

#echo "allWorks = $allWorks"

printf "IOTest:Works=" >> ${TmpOut}
printf "$allWorks\n" >> ${TmpOut}

cat ${TmpOut} >> ${OutFile}
rm -f ${TmpOut}
rm -f IO_test.txt 
exit 0

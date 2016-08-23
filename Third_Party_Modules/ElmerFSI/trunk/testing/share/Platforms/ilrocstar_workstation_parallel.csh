#!/bin/tcsh
# runtest calls this script with the following arguments:
set RESULTSFILE = ${1}
set SRCDIR = ${2}
set BINDIR = ${3}
echo "Running parallel test 1"
echo "RESULTSFILE = "
echo ${RESULTSFILE}
echo "SRCDIR = "
echo ${SRCDIR}
echo "BINDIR = "
echo ${BINDIR}

rm -f tmpresults_1.txt
mpirun -np 2 ${BINDIR}/elmermoduledriver_parallel_test -s ${SRCDIR} -o tmpresults_1.txt
@ i = 1
while($i <= 240)
    @ i += 1
    if( -e tmpresults_1.txt ) then
        @ i += 241;
    else
        sleep 30;
    endif
end
cat tmpresults_1.txt
mv tmpresults_1.txt ${RESULTSFILE}
exit 0

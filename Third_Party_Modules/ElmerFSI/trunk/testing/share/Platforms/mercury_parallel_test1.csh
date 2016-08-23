#!/bin/tcsh

# runtest calls this script with the following arguments:
set RESULTSFILE = ${1}
set SRCDIR = ${2}
set BINDIR = ${3}

rm -f tmpresults_1.txt
cat <<EOF > ./elmermoduledriver_parallel_test_batch.csh
#!/bin/tcsh
#
#PBS -V
#PBS -l nodes=1:ppn=8
#PBS -l walltime=00:30:00 
#PBS -j oe
#PBS -o elmermoduledriver_parallel_test_batch_output
#PBS -A MPINFRA

cd \${PBS_O_WORKDIR}
mpirun -np 2 -machinefile \${PBS_NODEFILE} ${BINDIR}/elmermoduledriver_parallel_test -s ${SRCDIR} -o tmpresults_1.txt
exit 0
EOF
qsub elmermoduledriver_parallel_test_batch.csh
@ i = 1
while($i <= 720)
    @ i += 1
    if( -e tmpresults_1.txt ) then
        @ i += 721;
    else
        sleep 10;
    endif
end
sleep 20
cat tmpresults_1.txt >> ${RESULTSFILE}
#rm -f tmpresults_1.txt
rm -f ./elmermoduledriver_parallel_test_batch.csh
rm -f elmermoduledriver_parallel_test_batch_output
exit 0

#!/bin/bash
# @ job_name = testfitness-trace
# @ output = out_%j_testfitness-trace
# @ error = err_%j_testfitness-trace
# @ total_tasks = 1 
# @ cpus_per_task = 4
# @ wall_clock_limit = 0:15:00
## @ wall_clock_limit = 23:59:59


export OMP_NUM_THREADS=4
export EXTRAE_CONFIG_FILE=extrae.xml
export EXTRAE_HOME=/gpfs/apps/NVIDIA/CEPBATOOLS/extrae/2.2.1/64
source ${EXTRAE_HOME}/etc/extrae.sh

LD_PRELOAD=${EXTRAE_HOME}/lib/libomptrace.so 

./testfitnessChainOperationsCells_trace

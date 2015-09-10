#!/bin/bash
# @ initialdir = .
# @ output = trace.out
# @ error =  trace.err
# @ total_tasks = 16
# @ cpus_per_task = 1
# @ tasks_per_node = 1
# @ wall_clock_limit = 00:10:00

NPROCS=1
#NPOPUL=10000
NPOPUL=640
#PROBLEM=channel
PROBLEM=ellip1000
#PROBLEM=ellip200

NTEM=11
#NTEM=3
MTEM=11
#MTEM=3

TYPE=A

NIMG=1000
MIMG=1000

export OMP_NUM_THREADS=$NPROCS

srun ./trace.sh ./mainMPIOpenmp.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt ${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >>  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 2>&1  


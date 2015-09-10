#!/bin/bash
# @ job_name = mps-ga
# @ output = out_%j_mps-ga
# @ error = err_%j_mps-ga
# @ total_tasks = 1 
# @ cpus_per_task = 1
# @ wall_clock_limit = 1:05:00
## @ wall_clock_limit = 23:59:59


NPROCS=1
NPOPUL=1000
PROBLEM=channel

NTEM=9
MTEM=9

TYPE=A

NIMG=100
MIMG=100

export OMP_NUM_THREADS=$NPROCS

echo "/usr/bin/time ./mainSerial.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt salida_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt"

/usr/bin/time ./mainSerial.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt salida_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 2>&1  


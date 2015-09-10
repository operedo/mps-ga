#!/bin/bash
# @ job_name = mps-ga
# @ output = out_%j_mps-ga
# @ error = err_%j_mps-ga
# @ total_tasks = 4
# @ cpus_per_task = 12
# @ tasks_per_node 1
# @ gpus_per_node = 1
## @ wall_clock_limit = 00:50:00
# @ wall_clock_limit = 00:05:00

NPROCS=12
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

#echo "srun ./mainMPIOpenmp.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt salida_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt"
cat ../data/params/data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt >> evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 
/usr/bin/time srun ./mainMPIOpenmp.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt ${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >>  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 2>&1  
#/usr/bin/time ./mainOpenmp.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt salida_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 2>&1  

#cat ../data/data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt >> evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 
#/usr/bin/time ./mainAcc.exe data_${NTEM}x${MTEM}_${NIMG}x${MIMG}_${TYPE}_${PROBLEM}_${NPOPUL}.txt salida_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt histo_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt >>  evol_${PROBLEM}_${NTEM}x${MTEM}_${TYPE}_${NIMG}x${MIMG}_${SLURM_NPROCS}x${NPROCS}_${NPOPUL}_${SLURM_JOBID}.txt 2>&1  


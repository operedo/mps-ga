#!/bin/bash
# @ job_name = testSIMD
# @ output = out_%j_testSIMD
# @ error = err_%j_testSIMD
# @ total_tasks = 1
# @ cpus_per_task = 1
# @ wall_clock_limit = 0:05:00
## @ wall_clock_limit = 23:59:59


TAG="simd"
NPROCS=1
export OMP_NUM_THREADS=$NPROCS


for i in {1..30}
do
	echo "$i: OMP_NUM_THREADS=$NPROCS"
	echo "$i:" >> salida_${TAG}_${NPROCS}_${SLURM_JOBID}.txt 
	#ulimit -s unlimited
	srun ./testSIMDExternalCall.exe >> salida_${TAG}_${NPROCS}_${SLURM_JOBID}.txt 2>&1
done

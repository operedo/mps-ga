#!/bin/bash
# @ job_name = testfitness
# @ output = out_%j_testfitness
# @ error = err_%j_testfitness
# @ total_tasks = 1
# @ cpus_per_task = 12
## @ gpus_per_node = 1
# @ wall_clock_limit = 0:05:00
## @ wall_clock_limit = 23:59:59


#TAG="chainV7_auto-scalar"
#TAG="chainV2_auto_cse_openmpV2"
#TAG="chainV2_auto_cse_simd"
TAG="100x100_without-opt"
NPROCS=12

export OMP_NUM_THREADS=$NPROCS
#export OMP_SCHEDULE="dynamic,64"
export OMP_SCHEDULE="static"

for i in {1..100}
do
	echo "$i: OMP_NUM_THREADS=$NPROCS"
	echo "$i:" >> salida_${TAG}_${NPROCS}_${SLURM_JOBID}.txt 
	#ulimit -s unlimited
	#srun ./testfitnessChainOperationsCells.exe >> salida_${TAG}_${NPROCS}_${SLURM_JOBID}.txt 2>&1
	./testfitnessChainOperationsCells.exe >> salida_${TAG}_${NPROCS}_${SLURM_JOBID}.txt 2>&1
done

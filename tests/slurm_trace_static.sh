#!/bin/bash
# @ initialdir = .
# @ output = job_trace_%j.out
# @ error =  job_trace_%j.err
# @ total_tasks = 1
# @ cpus_per_task = 2
# @ tasks_per_node = 1
# @ tracing =  1
# @ wall_clock_limit = 00:15:00

export OMP_NUM_THREADS=2
#export OMP_SCHEDULE="dynamic"

#/opt/mpi/bullxmpi/1.1.11.1/bin/mpirun -mca mpi_yield_when_idle 0 ./trace_static.sh ./testfitnessChainOperationsCells_trace.exe
srun ./trace_static.sh ./testfitnessChainOperationsCells_trace 
	

#mv /gpfs/scratch/bsc21/bsc21021/traces/case.prv /gpfs/scratch/bsc21/bsc21021/traces/caseNP${SLURM_NPROCS}NT${OMP_NUM_THREADS}.prv
#mv /gpfs/scratch/bsc21/bsc21021/traces/case.pcf /gpfs/scratch/bsc21/bsc21021/traces/caseNP${SLURM_NPROCS}NT${OMP_NUM_THREADS}.pcf
#mv /gpfs/scratch/bsc21/bsc21021/traces/case.row /gpfs/scratch/bsc21/bsc21021/traces/caseNP${SLURM_NPROCS}NT${OMP_NUM_THREADS}.row

#rm /gpfs/scratch/bsc21/bsc21021/traces/TRACE* /gpfs/scratch/bsc21/bsc21021/traces/set-0/*


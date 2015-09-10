#!/bin/bash
# @ initialdir = .
# @ output = trace.out
# @ error =  trace.err
# @ total_tasks = 1
# @ cpus_per_task = 1
# @ tasks_per_node = 4
# @ wall_clock_limit = 00:10:00
# @ tracing = 1

srun ./trace_dyninst.sh ./testfitnessChainOperationsCells_trace


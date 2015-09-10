#!/bin/sh

export EXTRAE_HOME=/gpfs/apps/NVIDIA/CEPBATOOLS/extrae/2.2.1/64
export EXTRAE_CONFIG_FILE=./extrae.xml
export LD_PRELOAD=${EXTRAE_HOME}/lib/libompitrace.so
#export LD_PRELOAD=${EXTRAE_HOME}/lib/libmpitrace.so
#export LD_PRELOAD=${EXTRAE_HOME}/lib/libmpitracef.so

## Run the desired program
$*


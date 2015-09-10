#!/bin/sh

export OMP_NUM_THREADS=4
export EXTRAE_CONFIG_FILE=extrae.xml
export EXTRAE_HOME=/gpfs/apps/NVIDIA/CEPBATOOLS/extrae/2.2.1/64
source ${EXTRAE_HOME}/etc/extrae.sh

${EXTRAE_HOME}/bin/extrae -v ./pi

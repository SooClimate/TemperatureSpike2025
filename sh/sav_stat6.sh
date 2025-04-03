#!/bin/bash
#PBS -q cpu
#PBS -N stat6
#PBS -T intmpi
#PBS -b 2
#PBS -r n
#PBS -l elapstim_req=6:00:00
#PBS --custom cpusetnum-lhost=1
#PBS -v OMP_NUM_THREADS=2
#PBS -v I_MPI_PIN_DOMAIN=omp
#PBS -o stdout.%s.%j
#PBS -e stderr.%s.%j

module load R/4.0.3

cd /S/data00/G6008/d0997/program/r/script/TemperatureSpike2025


Rscript ./r/sav_stat_roll.r --var=rfc_cloud 
Rscript ./r/sav_stat_roll.r --var=rfc_volc 

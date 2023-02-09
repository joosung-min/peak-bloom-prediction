#!/bin/bash

#SBATCH --time=1-00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=2
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL

module load r/4.1.2
# Rscript test.r
Rscript B12_get_stations.r
#!/bin/bash

#SBATCH --time=06:00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=2g
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL
#SBATCH --job-name=A20_van

module load r/4.1.2

Rscript A2_closest_stations.r
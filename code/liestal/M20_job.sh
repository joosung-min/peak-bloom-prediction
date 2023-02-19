#!/bin/bash

#SBATCH --time=06:00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=2g
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL
#SBATCH --job-name=M20_liestal

module load r/4.1.2
Rscript M2_lgb_cv_liestal.r
# Rscript M3_lgb_final_liestal.r
# Rscript M2_lgb_cv_liestal.r
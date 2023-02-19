#!/bin/bash

#SBATCH --time=06:00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=2g
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL

module load r/4.1.2

# rm C11_gdd_model.r
# jupyter nbconvert --to script B21_pred.ipynb
Rscript M1_gdd_model.r
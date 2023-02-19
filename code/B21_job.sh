#!/bin/bash

#SBATCH --time=02:00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=2g
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL

module load r/4.1.2

rm B21_pred.r
jupyter nbconvert --to script B21_pred.ipynb
Rscript B21_pred.r
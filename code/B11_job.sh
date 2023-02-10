#!/bin/bash

#SBATCH --time=06:00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=4g
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL

module load r/4.1.2

rm B11_fit_lightgbm.r
jupyter nbconvert --to script B11_fit_lightgbm.ipynb
Rscript B11_fit_lightgbm.r
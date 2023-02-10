#!/bin/bash

#SBATCH --time=02:00:00
#SBATCH --account=def-lelliott
#SBATCH --cpus-per-task=4
#SBATCH --mail-user=<joosungm@sfu.ca>
#SBATCH --mail-type=ALL

module load r/4.1.2

rm -r A41_kyoto_gdd.r
jupyter nbconvert --to script A41_kyoto_gdd.ipynb
Rscript A41_kyoto_gdd.r

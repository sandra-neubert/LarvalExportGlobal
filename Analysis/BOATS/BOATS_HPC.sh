#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=15
#SBATCH --mem=150G
#SBATCH --job-name=Boats_LEx_d250_m1_oa_mpa0_1
#SBATCH --time=100:00:00
#SBATCH --partition=general
#SBATCH --account=account_string
#SBATCH -o slurm.output
#SBATCH -e slurm.error


module purge
module load matlab


matlab -nodisplay -nosplash -r "cd('sneubert-boats_v1/');run_boats"

#!/bin/bash

#!/bin/bash
#SBATCH --nodes=1-1
#SBATCH --ntasks=1
#SBATCH --mem=16g
#SBATCH --time=00:20:00
#SBATCH --array=0-499
#SBATCH -o /nas/longleaf/home/cmcrawf/slurmout/runcode_%J_%a.out
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cmcrawford@unc.edu

sh runcode.sh $SLURM_ARRAY_TASK_ID

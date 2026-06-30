#!/bin/bash

line=`expr $1 + 1`
input=`sed "${line}q;d" conditions.txt`

module add r
R CMD BATCH --no-save "--args $input" run.R /nas/longleaf/home/cmcrawf/slurmout/sim_$SLURM_ARRAY_TASK_ID.Rout

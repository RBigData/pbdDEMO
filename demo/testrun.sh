#!/bin/sh
for fil in `ls *.r`;do
    mpirun -np 4 Rscript $fil
done

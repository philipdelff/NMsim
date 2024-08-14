#!/bin/bash

#$ -wd /data/sandbox/trunk/analysis/NMsim/models/mod_lorlatinib_estimate

/opt/NONMEM/nm75/run/nmfe75 mod_lorlatinib_estimate.mod  mod_lorlatinib_estimate.lst  -parafile=mod_lorlatinib_estimate.pnm -maxlim=2

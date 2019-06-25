#!/bin/bash

DATADIR=/data

RAWDATAINPUTDIR=/eos/experiment/na62/data/offline/CITests
#RAWDATAFILE=na62raw_1476916073-03-006610-0008.dat
RAWDATAFILE=na62raw_1507352270-03-008215-0020.dat
#RAWDATAFILE=na62raw_1525735539-02-008647-0025.dat

if [ ! -f $DATADIR/$RAWDATAFILE ]; then
  # copying the raw data file from EOS
  mkdir -p $DATADIR
  kinit -kt /secret/na62om.keytab na62om@CERN.CH
  xrdcp root://eosna62/${RAWDATAINPUTDIR}/${RAWDATAFILE} $DATADIR
fi


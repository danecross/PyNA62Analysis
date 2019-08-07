#!/bin/bash

BUILDDIR=$CI_PROJECT_DIR
TESTDIR=$BUILDDIR/test_mc_reco_ana
RECODIR=$BUILDDIR/NA62Reconstruction
ANADIR=$BUILDDIR/NA62Analysis

DATADIR=/data
#RAWDATAFILE=na62raw_1476916073-03-006610-0008.dat
#NEVTSRAWDATAFILE=235705  # this is relative to the RAWDATAFILE above!
RAWDATAFILE=na62raw_1507352270-03-008215-0020.dat
NEVTSRAWDATAFILE=270914  # this is relative to the RAWDATAFILE above!
#RAWDATAFILE=na62raw_1525735539-02-008647-0025.dat
#NEVTSRAWDATAFILE=262615  # this is relative to the RAWDATAFILE above!
NEVTSTOBESKIPPED=$((NEVTSRAWDATAFILE-100))

run_and_check(){
  cmd=$1
  echo -e "\e[1;93m$cmd\e[0m"
  $cmd
  retcod=$?
  if [ $retcod -ne 0 ]; then
    exit $retcod
  fi
}

source "$ANADIR/scripts/env.sh"

export ANALYSISFW_PATH=$ANADIR
export ANALYSISFW_USERDIR=$ANADIR
export LD_LIBRARY_PATH=$ANALYSISFW_PATH/lib-${SYSTEMINSTALL}:$LD_LIBRARY_PATH

mkdir "$TESTDIR"
cp -r "$RECODIR/config" "$TESTDIR"

cd "$TESTDIR"

echo ">>> Running Calib1!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.calib1.root -e1 -c config/NA62Reconstruction.Calib1.conf -n 100 -j 11000"

echo ">>> Running Calib2!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.calib2.root -e1 -c config/NA62Reconstruction.Calib2.conf -n 100 -j 11000"

echo ">>> Running Calib3!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.calib3.root -e1 -c config/NA62Reconstruction.Calib3.conf -n 100 -j 11000"

echo ">>> Running Calib4!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.calib4.root -e1 -c config/NA62Reconstruction.Calib4.conf -n 100 -j 11000"

echo ">>> Running Swap!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.swap.root -e1 -c config/NA62Reconstruction.Swap.conf -j $NEVTSTOBESKIPPED"

echo ">>> Running PreProd!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.preprod.root -e1 -c config/NA62Reconstruction.preprod.conf -j $NEVTSTOBESKIPPED"

echo ">>> Running Prod!"
run_and_check "$RECODIR/bin-${SYSTEMINSTALL}/NA62Reco -i $DATADIR/$RAWDATAFILE -o raw.prod.root -c config/NA62Reconstruction.Prod.conf -j $NEVTSTOBESKIPPED -C /afs/cern.ch/work/n/na62prod/public/CDB/CITests"

echo ">>> Extract histos!"
run_and_check "$ANADIR/bin-${SYSTEMINSTALL}/RmObjFromRootF raw.prod.root raw.hist.root $ANADIR/Tools/RmObjFromRootF.default.list"

cp -r "$ANADIR/config" "$TESTDIR"

echo ">>> Running Filtering!"
run_and_check "$ANADIR/bin-${SYSTEMINSTALL}/Filter -i raw.prod.root -o reco.filter.root --filter --config config/Filter.conf -C /afs/cern.ch/work/n/na62prod/public/CDB/CITests -e0 --no-use-badburst"

echo ">>> Running ROOTFileMerger!"
echo "raw.prod.root" > rootfilemerger.list
echo "raw.hist.root" >> rootfilemerger.list
run_and_check "$ANADIR/bin-${SYSTEMINSTALL}/ROOTFileMerger rootfilemerger.list rootfilemerger.root"

echo ">>> Running PostProcessing!"
run_and_check "$ANADIR/bin-${SYSTEMINSTALL}/PostProcessing -i raw.prod.root -o reco.postproc.root -C /afs/cern.ch/work/n/na62prod/public/CDB/CITests -e0 --no-use-badburst"

run_and_check "$ANADIR/bin-${SYSTEMINSTALL}/PostProcessing -i reco.postproc.root -C /afs/cern.ch/work/n/na62prod/public/CDB/CITests --histo --no-use-badburst"

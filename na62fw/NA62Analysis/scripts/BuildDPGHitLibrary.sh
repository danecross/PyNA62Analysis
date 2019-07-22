#!/bin/bash

# These are the file lists of input MC events that will form the hit library.
# All the files contain the 'reco' level hits.
# The names below are just placeholders, you will have to put the relevant file lists by hand.
# Up-to-date lists can be found here https://twiki.cern.ch/twiki/bin/viewauth/NA62/MonteCarloSamples

# The commands below select the number of files that give a reasonable number of events for the library.
# All events in the halo and pion files must be used to get the proper normalisation

# Finally this script will print an 'hadd' command, which should be used to build the library.
# Note that the four types of beam halo are combined into a single 'Halo' library.

# 6 main kaon decays
km2List=lists/mc/km2.list
k2piList=lists/mc/k2pi.list
k3piList=lists/mc/k3pi.list
ke3List=lists/mc/ke3.list
km3List=lists/mc/km3.list
k3pi0List=lists/mc/k3pi0.list

# extended decay region for the 3 main kaon decays
km2edrList=lists/mc/km2.ext.list
k2piedrList=lists/mc/k2pi.ext.list
k3piedrList=lists/mc/k3pi.ext.list

# four types of beam halo (a single file, not a list)
haloFile=lists/mc/HaloReco.root

# beam pions (a single file, not a list)
pionFile=lists/mc/PionReco.root

echo "Building Km2 library (0)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $km2List -f 50 -o output.root -p"BuildDPGHitLibrary:Name=Km2"

echo " "
echo " "
echo " "
echo "Building K2pi library (1)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $k2piList -f 10 -o output.root -p"BuildDPGHitLibrary:Name=K2pi"

echo " "
echo " "
echo " "
echo "Building K3pi library (2)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $k3piList -f 10 -o output.root -p"BuildDPGHitLibrary:Name=K3pi"

echo " "
echo " "
echo " "
echo "Building Ke3 library (3)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $ke3List -f 10 -o output.root -p"BuildDPGHitLibrary:Name=Ke3"

echo " "
echo " "
echo " "
echo "Building Km3 library (4)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $km3List -f 4 -o output.root -p"BuildDPGHitLibrary:Name=Km3"

echo " "
echo " "
echo " "
echo "Building K3pi0 library (5)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $k3pi0List -f 3 -o output.root -p"BuildDPGHitLibrary:Name=K3pi0"

echo " "
echo " "
echo " "
echo "Building Km2 EDR library (6)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $km2edrList -f 100 -o output.root -p"BuildDPGHitLibrary:Name=Km2EDR"

echo " "
echo " "
echo " "
echo "Building K2pi EDR library (7)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $k2piedrList -f 40 -o output.root -p"BuildDPGHitLibrary:Name=K2piEDR"

echo " "
echo " "
echo " "
echo "Building K3pi EDR library (8)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -l $k3piedrList -f 20 -o output.root -p"BuildDPGHitLibrary:Name=K3piEDR"

echo " "
echo " "
echo " "
echo "Building Halo library (9)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -i $haloFile -o output.root -p"BuildDPGHitLibrary:Name=Halo"

echo " "
echo " "
echo " "
echo "Building Pim2 library (10)"
./bin-$SYSTEMINSTALL/BuildDPGHitLibrary -i $pionFile -o output.root -p"BuildDPGHitLibrary:Name=Pim2"

echo "Now hadd the outputs together with this command:"
echo "hadd -f DownstreamPileupGeneratorLibrary.root DownstreamPileupGeneratorLibraryKm2.root DownstreamPileupGeneratorLibraryK2pi.root DownstreamPileupGeneratorLibraryK3pi.root DownstreamPileupGeneratorLibraryKe3.root DownstreamPileupGeneratorLibraryKm3.root DownstreamPileupGeneratorLibraryK3pi0.root DownstreamPileupGeneratorLibraryKm2EDR.root DownstreamPileupGeneratorLibraryK2piEDR.root DownstreamPileupGeneratorLibraryK3piEDR.root DownstreamPileupGeneratorLibraryHalo.root DownstreamPileupGeneratorLibraryPim2.root"


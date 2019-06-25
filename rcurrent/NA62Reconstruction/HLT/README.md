# HLT algorithms

## How to fetch the HLT Algorithms

### Fetch them during the clone

    git clone --recursive

### Fetch them after the clone

    git submodule init
    git submodule update

## How to compile the HLT
### Enable the HLT algorithm in the env.sh

    cd NA62Reconstruction

Open *scripts/env.sh* and uncomment this line of code:  

    #export ONLINEHLTDEFINED=1

### Adjust the path of HLT config files
Open and edit *NA62Reconstruction/HLT/na62-trigger-algorithms/l1/ConfPath.h*

### Compile the Reconstruction

    source scripts/env.sh
    make -j16

## Execute

     ./bin-cc7/NA62Reco -i na62raw_1507352198-02-008215-0016.dat -n 20000 -c config/yourconfig.conf

In the config file you should request KTAG LAV STRAW
The following message will appear in the first output comments:

    [NA62Reco] Is equipped with HLT Algorithms!!!

## Miscellanea
### Unpack the HLT decoder

    g++ DecoderHandler.h -E -x c -P -C  -I  ../../../na62-farm-lib > DecoderHandler_preprocessed.h

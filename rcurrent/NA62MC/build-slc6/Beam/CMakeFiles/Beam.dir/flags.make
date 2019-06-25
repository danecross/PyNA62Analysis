# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# compile C with /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/gcc
# compile CXX with /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++
# compile Fortran with /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/gfortran
C_FLAGS = -O2 -fPIC -O3 -DNDEBUG -fPIC  

C_DEFINES = -DBeam_EXPORTS -DG4MULTITHREADED -DG4SLC6=1 -DG4VERBOSE -DG4_STORE_TRAJECTORY

C_INCLUDES = -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4 -isystem /cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include/boost-1_62 -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Beam/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Generator -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LoopTools214/src/LoopTools214/build 

CXX_FLAGS =  -W -Wall -pedantic -Wno-non-virtual-dtor -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-variadic-macros -Wshadow -pipe -pthread -ftls-model=global-dynamic -std=c++98   -pipe -m64 -fsigned-char -fPIC -pthread -std=c++14 -W -Wall -ansi -pedantic -Wno-non-virtual-dtor -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-shadow -Wno-vla -pipe -std=c++14  -pipe -m64 -fsigned-char -fPIC -pthread -std=c++14 -O2 -DNDEBUG -O3 -DNDEBUG -fPIC  

CXX_DEFINES = -DBeam_EXPORTS -DG4MULTITHREADED -DG4SLC6=1 -DG4VERBOSE -DG4_STORE_TRAJECTORY

CXX_INCLUDES = -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4 -isystem /cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui -isystem /cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include/boost-1_62 -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Beam/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Generator -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LoopTools214/src/LoopTools214/build 

Fortran_FLAGS = -O2 -fPIC -fno-automatic -fno-backslash -fno-second-underscore -std=legacy -O3 -fPIC  

Fortran_DEFINES = -DBeam_EXPORTS -DG4MULTITHREADED -DG4SLC6=1 -DG4VERBOSE -DG4_STORE_TRAJECTORY

Fortran_INCLUDES = -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4 -I/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include/boost-1_62 -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV0/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV1/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV2/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/RICH/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Spectrometer/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Beam/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Generator -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LoopTools214/src/LoopTools214/build 


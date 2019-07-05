# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

# compile CXX with /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++
CXX_FLAGS = -W -Wall -pedantic -Wno-non-virtual-dtor -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-variadic-macros -Wshadow -pipe -DG4USE_STD11 -pthread -ftls-model=global-dynamic -std=c++1z -W -Wall -ansi -pedantic -Wno-non-virtual-dtor -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-shadow -Wno-vla -pipe -std=c++14  -pipe -m64 -fsigned-char -pthread -std=c++1z -O3 -DNDEBUG -fno-trapping-math -ftree-vectorize -fno-math-errno -fPIC  

CXX_DEFINES = -DG4GEOM_USE_USOLIDS -DG4LIB_USE_GDML=1 -DG4MULTITHREADED -DG4VERBOSE -DG4_STORE_TRAJECTORY -DMCBase_EXPORTS -DVECCORE_ENABLE_VC -DVECGEOM_INPLACE_TRANSFORMATIONS -DVECGEOM_NO_SPECIALIZATION -DVECGEOM_QUADRILATERALS_VC -DVECGEOM_ROOT -DVECGEOM_USE_INDEXEDNAVSTATES -DVECGEOM_VC

CXX_INCLUDES = -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/NA62/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/LAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/LKr/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/MUV0/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/MUV1/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/MUV2/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/MUV3/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/RICH/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/SAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/SAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/FullReco/Spectrometer/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/NA62/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/LAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/LKr/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/MUV0/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/MUV1/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/MUV2/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/MUV3/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/RICH/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/SAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/SAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/Persistency/SlimReco/Spectrometer/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/clhep/2.4.1.0-2c56f/x86_64-centos7-gcc7-opt/lib/CLHEP-2.4.1.0/../../include -isystem /cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/include/Geant4 -isystem /cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-ced0e/x86_64-centos7-gcc7-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/veccore/0.4.2-ff84f/x86_64-centos7-gcc7-opt/lib/cmake/VecCore/../../../include -isystem /cvmfs/sft.cern.ch/lcg/releases/Vc/1.3.2-7fbe0/x86_64-centos7-gcc7-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/include -isystem /cvmfs/sft.cern.ch/lcg/releases/VecGeom/v1.1.0-22e48/x86_64-centos7-gcc7-opt/lib/cmake/VecGeom/../../../include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/include/boost -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/xrootd/4.8.4/x86_64-centos7-gcc7-opt/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Cedar/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/CHANTI/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/CHOD/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/NewCHOD/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/GigaTracker/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/HAC/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/IRC/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/LAV/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/LKr/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/MUV0/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/MUV1/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/MUV2/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/MUV3/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/RICH/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/SAC/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/MCBase/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Beam/include -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Generator -I/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/LoopTools214/src/LoopTools214/build 


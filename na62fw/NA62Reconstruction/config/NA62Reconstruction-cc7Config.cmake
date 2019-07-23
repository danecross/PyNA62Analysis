# - Config file for the NA62Reconstruction package
# It defines the following variables
#  NA62RECO_FOUND - True
# Paths
#  NA62RECO_INCLUDE_DIRS      - include directories for NA62Reconstruction
#  NA62RECO_LIBRARY_DIRS      - library directories for NA62Reconstruction
# Lists
#  NA62RECO_LIBRARIES         - reco libraries to link against
#  NA62RECO_LIBRARIES_STATIC  - reco static libraries to link against
 
 
set(NA62RECO_FOUND 1)
# Compute paths
get_filename_component(NA62RECO_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
set(NA62RECO_INCLUDE_DIRS ";/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/Cedar/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/CHANTI/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/CHOD/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/NewCHOD/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/GigaTracker/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/HAC/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/IRC/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/LAV/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/LKr/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/MUV0/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/MUV1/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/MUV2/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/MUV3/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/RICH/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/SAC/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/SAV/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/Spectrometer/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/include;/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/RecoBase/include")
set(NA62RECO_LIBRARY_DIRS "/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62Reconstruction/lib-cc7")
 
# These are IMPORTED targets created by FooBarTargets.cmake
set(NA62RECO_LIBRARIES ";RecoCedar;RecoCHANTI;RecoCHOD;RecoNewCHOD;RecoGigaTracker;RecoHAC;RecoIRC;RecoLAV;RecoLKr;RecoMUV0;RecoMUV1;RecoMUV2;RecoMUV3;RecoRICH;RecoSAC;RecoSAV;RecoSpectrometer;RecoBase;RecoService")
set(NA62RECO_LIBRARIES_STATIC ";RecoCedar-static;RecoCHANTI-static;RecoCHOD-static;RecoNewCHOD-static;RecoGigaTracker-static;RecoHAC-static;RecoIRC-static;RecoLAV-static;RecoLKr-static;RecoMUV0-static;RecoMUV1-static;RecoMUV2-static;RecoMUV3-static;RecoRICH-static;RecoSAC-static;RecoSAV-static;RecoSpectrometer-static;RecoBase-static;RecoService-static")

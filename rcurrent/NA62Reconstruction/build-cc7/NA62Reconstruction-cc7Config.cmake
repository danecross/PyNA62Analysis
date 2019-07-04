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
set(NA62RECO_INCLUDE_DIRS ";/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/Cedar/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/CHANTI/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/CHOD/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/NewCHOD/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/GigaTracker/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/HAC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/IRC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/LAV/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/LKr/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/MUV0/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/MUV1/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/MUV2/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/MUV3/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/RICH/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/SAC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/SAV/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/Spectrometer/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/RecoBase/include")
set(NA62RECO_LIBRARY_DIRS "/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/lib-cc7")
 
# These are IMPORTED targets created by FooBarTargets.cmake
set(NA62RECO_LIBRARIES ";Cedar;CHANTI;CHOD;NewCHOD;GigaTracker;HAC;IRC;LAV;LKr;MUV0;MUV1;MUV2;MUV3;RICH;SAC;SAV;Spectrometer;RecoBase")
set(NA62RECO_LIBRARIES_STATIC ";Cedar-static;CHANTI-static;CHOD-static;NewCHOD-static;GigaTracker-static;HAC-static;IRC-static;LAV-static;LKr-static;MUV0-static;MUV1-static;MUV2-static;MUV3-static;RICH-static;SAC-static;SAV-static;Spectrometer-static;RecoBase-static")

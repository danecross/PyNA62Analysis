# - Config file for the NA62MC package
# It defines the following variables
# Paths
#  NA62MC_PERS_INCLUDE_DIRS     - include directories for NA62MC Persistency
#  NA62MC_CONFIG_DIR            - configuration files path (cmake, conf, ...)
#  NA62MC_PERS_LIBRARY_DIRS     - library directories for NA62MC Persistency
# Lists
#  NA62MC_PERS_LIBRARIES        - persistency libraries to link against
#  NA62MC_PERS_LIBRARIES-STATIC - persistency static libraries to link against
#  NA62MC_DETECTORS             - List of subdetectors
 
# Compute paths
get_filename_component(NA62MC_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
set(NA62MC_PERS_INCLUDE_DIRS ";/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/Cedar;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/CHANTI;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/CHOD;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/NewCHOD;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/GigaTracker;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/HAC;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/IRC;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/LAV;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/LKr;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV0;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV1;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV2;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV3;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/RICH;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/SAC;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/Spectrometer;/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers")
set(NA62MC_CONFIG_DIR "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/config")
set(NA62MC_PERS_LIBRARY_DIRS "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/lib-slc6/Persistency")
 
#TODO publish list of subdet
set(NA62MC_DETECTORS "Cedar;CHANTI;CHOD;NewCHOD;GigaTracker;HAC;IRC;LAV;LKr;MUV0;MUV1;MUV2;MUV3;RICH;SAC;Spectrometer")
 
# These are IMPORTED targets created by FooBarTargets.cmake
set(NA62MC_PERS_LIBRARIES NA62Persistency ";CedarPersistency;CHANTIPersistency;CHODPersistency;NewCHODPersistency;GigaTrackerPersistency;HACPersistency;IRCPersistency;LAVPersistency;LKrPersistency;MUV0Persistency;MUV1Persistency;MUV2Persistency;MUV3Persistency;RICHPersistency;SACPersistency;SpectrometerPersistency")
set(NA62MC_PERS_LIBRARIES_STATIC NA62Persistency-static ";CedarPersistency-static;CHANTIPersistency-static;CHODPersistency-static;NewCHODPersistency-static;GigaTrackerPersistency-static;HACPersistency-static;IRCPersistency-static;LAVPersistency-static;LKrPersistency-static;MUV0Persistency-static;MUV1Persistency-static;MUV2Persistency-static;MUV3Persistency-static;RICHPersistency-static;SACPersistency-static;SpectrometerPersistency-static")

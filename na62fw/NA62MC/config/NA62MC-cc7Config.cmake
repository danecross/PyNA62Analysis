# - Config file for the NA62MC package
# It defines the following variables
# Paths
#  NA62MC_CONFIG_DIR            - configuration files path (cmake, conf, ...)
# Lists
#  NA62MC_DETECTORS             - List of subdetectors
 
# Compute paths
get_filename_component(NA62MC_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
set(NA62MC_CONFIG_DIR "/afs/cern.ch/user/d/dacross/SummerProject/na62fw/NA62MC/config")
 
#TODO publish list of subdet
set(NA62MC_DETECTORS "Cedar;CHANTI;CHOD;NewCHOD;GigaTracker;HAC;IRC;LAV;LKr;MUV0;MUV1;MUV2;MUV3;RICH;SAC;Spectrometer")

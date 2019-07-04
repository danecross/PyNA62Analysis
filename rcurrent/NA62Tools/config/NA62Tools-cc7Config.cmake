# - Config file for the NA62TOOLS package
# It defines the following variables
#  NA62TOOLS_FOUND - True
# Paths
#  NA62TOOLS_PERS_INCLUDE_DIRS     - include directories for NA62TOOLS Persistency
#  NA62TOOLS_SLIM_INCLUDE_DIRS     - include directories for NA62TOOLS Slim Persistency
#  NA62TOOLS_INCLUDE_DIRS          - include directories for NA62TOOLS
#  NA62TOOLS_CONFIG_DIR            - configuration files path (cmake, conf, ...)
#  NA62TOOLS_LIBRARY_DIRS          - library directories for NA62TOOLS
# Lists
#  NA62TOOLS_PERS_LIBRARIES        - persistency libraries to link against
#  NA62TOOLS_PERS_LIBRARIES-STATIC - persistency static libraries to link against
#  NA62TOOLS_SLIM_LIBRARIES        - slim persistency libraries to link against
#  NA62TOOLS_SLIM_LIBRARIES-STATIC - slim persistency static libraries to link against
#  NA62TOOLS_LIBRARIES             - NA62Tools libraries to link against
#  NA62TOOLS_LIBRARIES-STATIC      - NA62Tools static libraries to link against
 
set(NA62TOOLS_FOUND 1)
# Compute paths
get_filename_component(NA62TOOLS_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
set(NA62TOOLS_INCLUDE_DIRS ";/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include")
set(NA62TOOLS_CONFIG_DIR "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/config")
set(NA62TOOLS_LIBRARY_DIRS "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/Persistency;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/SlimPersistency")
set(NA62TOOLS_LIBRARIES NA62Tools)
set(NA62TOOLS_LIBRARIES_STATIC NA62Tools-static)

set(NA62TOOLS_PERS_INCLUDE_DIRS "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/Cedar/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/CHANTI/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/CHOD/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/GigaTracker/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/HAC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/IRC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LAV/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/LKr/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV0/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV2/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV3/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NewCHOD/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/RICH/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAV/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/Spectrometer/include")
set(NA62TOOLS_PERS_LIBRARIES "NA62Persistency;CedarPersistency;CHANTIPersistency;CHODPersistency;GigaTrackerPersistency;HACPersistency;IRCPersistency;LAVPersistency;LKrPersistency;MUV0Persistency;MUV1Persistency;MUV2Persistency;MUV3Persistency;NewCHODPersistency;RICHPersistency;SACPersistency;SAVPersistency;SpectrometerPersistency")
set(NA62TOOLS_PERS_LIBRARIES_STATIC "NA62Persistency-static;CedarPersistency-static;CHANTIPersistency-static;CHODPersistency-static;GigaTrackerPersistency-static;HACPersistency-static;IRCPersistency-static;LAVPersistency-static;LKrPersistency-static;MUV0Persistency-static;MUV1Persistency-static;MUV2Persistency-static;MUV3Persistency-static;NewCHODPersistency-static;RICHPersistency-static;SACPersistency-static;SAVPersistency-static;SpectrometerPersistency-static")

set(NA62TOOLS_SLIM_INCLUDE_DIRS "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NA62/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Cedar/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/CHANTI/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/CHOD/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/GigaTracker/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/HAC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/IRC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LKr/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV0/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV1/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV2/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/MUV3/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NewCHOD/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/RICH/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/SAC/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/SAV/include;/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Spectrometer/include")
set(NA62TOOLS_SLIM_LIBRARIES "NA62SlimPersistency;CedarSlimPersistency;CHANTISlimPersistency;CHODSlimPersistency;GigaTrackerSlimPersistency;HACSlimPersistency;IRCSlimPersistency;LAVSlimPersistency;LKrSlimPersistency;MUV0SlimPersistency;MUV1SlimPersistency;MUV2SlimPersistency;MUV3SlimPersistency;NewCHODSlimPersistency;RICHSlimPersistency;SACSlimPersistency;SAVSlimPersistency;SpectrometerSlimPersistency")
set(NA62TOOLS_SLIM_LIBRARIES_STATIC "NA62SlimPersistency-static;CedarSlimPersistency-static;CHANTISlimPersistency-static;CHODSlimPersistency-static;GigaTrackerSlimPersistency-static;HACSlimPersistency-static;IRCSlimPersistency-static;LAVSlimPersistency-static;LKrSlimPersistency-static;MUV0SlimPersistency-static;MUV1SlimPersistency-static;MUV2SlimPersistency-static;MUV3SlimPersistency-static;NewCHODSlimPersistency-static;RICHSlimPersistency-static;SACSlimPersistency-static;SAVSlimPersistency-static;SpectrometerSlimPersistency-static")

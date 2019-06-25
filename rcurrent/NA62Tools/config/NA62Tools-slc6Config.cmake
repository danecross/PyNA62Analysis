# - Config file for the NA62TOOLS package
# It defines the following variables
# Paths
#  NA62TOOLS_PERS_INCLUDE_DIRS     - include directories for NA62TOOLS Persistency
#  NA62TOOLS_CONFIG_DIR            - configuration files path (cmake, conf, ...)
#  NA62TOOLS_PERS_LIBRARY_DIRS     - library directories for NA62TOOLS Persistency
# Lists
#  NA62TOOLS_PERS_LIBRARIES        - persistency libraries to link against
#  NA62TOOLS_PERS_LIBRARIES-STATIC - persistency static libraries to link against
#  NA62TOOLS_DETECTORS             - List of subdetectors
 
# Compute paths
get_filename_component(NA62TOOLS_CMAKE_DIR "${CMAKE_CURRENT_LIST_FILE}" PATH)
set(NA62TOOLS_INCLUDE_DIRS ";/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include")
set(NA62TOOLS_CONFIG_DIR "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/config")
set(NA62TOOLS_LIBRARY_DIRS "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/lib-slc6")

# These are IMPORTED targets created by FooBarTargets.cmake
set(NA62TOOLS_LIBRARIES NA62Tools)
set(NA62TOOLS_LIBRARIES_STATIC NA62Tools-static)

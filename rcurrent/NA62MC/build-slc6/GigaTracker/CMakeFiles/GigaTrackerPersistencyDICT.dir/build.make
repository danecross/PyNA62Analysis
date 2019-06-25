# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /cvmfs/sft.cern.ch/lcg/releases/CMake/3.5.2-a9b03/x86_64-slc6-gcc49-opt/bin/cmake

# The command to remove a file.
RM = /cvmfs/sft.cern.ch/lcg/releases/CMake/3.5.2-a9b03/x86_64-slc6-gcc49-opt/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6

# Utility rule file for GigaTrackerPersistencyDICT.

# Include the progress variables for this target.
include GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/progress.make

GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT: GigaTracker/GigaTrackerPersistencyDICT.cxx


GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/GigaTrackerPersistencyLinkDef.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TDigiGigaTrackerError.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerDigiEvent.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerHit.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TRecoGigaTrackerCandidate.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TRecoGigaTrackerHit.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/GigaTrackerChannelID.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TDigiGigaTrackerError.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerDigi.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerDigiEvent.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerEvent.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerHit.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TGigaTrackerSpecialTriggerEvent.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TRecoGigaTrackerCandidate.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TRecoGigaTrackerEvent.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/include/TRecoGigaTrackerHit.hh
GigaTracker/GigaTrackerPersistencyDICT.cxx: ../GigaTracker/Persistency/GigaTrackerPersistencyLinkDef.hh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating GigaTrackerPersistencyDICT.cxx, libGigaTrackerPersistency_rdict.pcm, libGigaTrackerPersistency.rootmap"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/bin/rootcling -f GigaTrackerPersistencyDICT.cxx -s /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker/libGigaTrackerPersistency.so -rml libGigaTrackerPersistency.so -rmf /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker/libGigaTrackerPersistency.rootmap -inlineInputHeader -DG4_STORE_TRAJECTORY -DG4VERBOSE -DG4MULTITHREADED -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4 -I/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include/boost-1_62 -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/GigaTrackerChannelID.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TDigiGigaTrackerError.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TGigaTrackerDigi.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TGigaTrackerDigiEvent.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TGigaTrackerEvent.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TGigaTrackerHit.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TGigaTrackerSpecialTriggerEvent.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TRecoGigaTrackerCandidate.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TRecoGigaTrackerEvent.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include/TRecoGigaTrackerHit.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/GigaTrackerPersistencyLinkDef.hh

GigaTracker/libGigaTrackerPersistency_rdict.pcm: GigaTracker/GigaTrackerPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate GigaTracker/libGigaTrackerPersistency_rdict.pcm

GigaTracker/libGigaTrackerPersistency.rootmap: GigaTracker/GigaTrackerPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate GigaTracker/libGigaTrackerPersistency.rootmap

GigaTrackerPersistencyDICT: GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT
GigaTrackerPersistencyDICT: GigaTracker/GigaTrackerPersistencyDICT.cxx
GigaTrackerPersistencyDICT: GigaTracker/libGigaTrackerPersistency_rdict.pcm
GigaTrackerPersistencyDICT: GigaTracker/libGigaTrackerPersistency.rootmap
GigaTrackerPersistencyDICT: GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/build.make

.PHONY : GigaTrackerPersistencyDICT

# Rule to build all files generated by this target.
GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/build: GigaTrackerPersistencyDICT

.PHONY : GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/build

GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker && $(CMAKE_COMMAND) -P CMakeFiles/GigaTrackerPersistencyDICT.dir/cmake_clean.cmake
.PHONY : GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/clean

GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : GigaTracker/CMakeFiles/GigaTrackerPersistencyDICT.dir/depend

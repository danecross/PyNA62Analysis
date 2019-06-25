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

# Utility rule file for IRCPersistencyDICT.

# Include the progress variables for this target.
include IRC/CMakeFiles/IRCPersistencyDICT.dir/progress.make

IRC/CMakeFiles/IRCPersistencyDICT: IRC/IRCPersistencyDICT.cxx


IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/IRCPersistencyLinkDef.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TIRCDigi.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TIRCHit.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TRecoIRCEvent.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/IRCChannelID.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TIRCDigi.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TIRCEvent.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TIRCHit.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TRecoIRCCandidate.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TRecoIRCEvent.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/include/TRecoIRCHit.hh
IRC/IRCPersistencyDICT.cxx: ../IRC/Persistency/IRCPersistencyLinkDef.hh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating IRCPersistencyDICT.cxx, libIRCPersistency_rdict.pcm, libIRCPersistency.rootmap"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/bin/rootcling -f IRCPersistencyDICT.cxx -s /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC/libIRCPersistency.so -rml libIRCPersistency.so -rmf /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC/libIRCPersistency.rootmap -inlineInputHeader -DG4_STORE_TRAJECTORY -DG4VERBOSE -DG4MULTITHREADED -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4 -I/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include/boost-1_62 -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Cedar/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHANTI/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/CHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/GigaTracker/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MCBase/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/IRCChannelID.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/TIRCDigi.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/TIRCEvent.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/TIRCHit.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/TRecoIRCCandidate.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/TRecoIRCEvent.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/include/TRecoIRCHit.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC/Persistency/IRCPersistencyLinkDef.hh

IRC/libIRCPersistency_rdict.pcm: IRC/IRCPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate IRC/libIRCPersistency_rdict.pcm

IRC/libIRCPersistency.rootmap: IRC/IRCPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate IRC/libIRCPersistency.rootmap

IRCPersistencyDICT: IRC/CMakeFiles/IRCPersistencyDICT
IRCPersistencyDICT: IRC/IRCPersistencyDICT.cxx
IRCPersistencyDICT: IRC/libIRCPersistency_rdict.pcm
IRCPersistencyDICT: IRC/libIRCPersistency.rootmap
IRCPersistencyDICT: IRC/CMakeFiles/IRCPersistencyDICT.dir/build.make

.PHONY : IRCPersistencyDICT

# Rule to build all files generated by this target.
IRC/CMakeFiles/IRCPersistencyDICT.dir/build: IRCPersistencyDICT

.PHONY : IRC/CMakeFiles/IRCPersistencyDICT.dir/build

IRC/CMakeFiles/IRCPersistencyDICT.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC && $(CMAKE_COMMAND) -P CMakeFiles/IRCPersistencyDICT.dir/cmake_clean.cmake
.PHONY : IRC/CMakeFiles/IRCPersistencyDICT.dir/clean

IRC/CMakeFiles/IRCPersistencyDICT.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/IRC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC/CMakeFiles/IRCPersistencyDICT.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : IRC/CMakeFiles/IRCPersistencyDICT.dir/depend


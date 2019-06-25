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

# Include any dependencies generated for this target.
include LAV/CMakeFiles/LAVPersistency-static.dir/depend.make

# Include the progress variables for this target.
include LAV/CMakeFiles/LAVPersistency-static.dir/progress.make

# Include the compile flags for this target's objects.
include LAV/CMakeFiles/LAVPersistency-static.dir/flags.make

# Object files for target LAVPersistency-static
LAVPersistency__static_OBJECTS =

# External object files for target LAVPersistency-static
LAVPersistency__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/LAVChannelID.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TLAVDigi.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TLAVEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TLAVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVCandidate.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVDigi.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjPersistencyLib.dir/LAVPersistencyDICT.cxx.o"

LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/LAVChannelID.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TLAVDigi.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TLAVEvent.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TLAVHit.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVCandidate.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVDigi.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVEvent.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/Persistency/src/TRecoLAVHit.cc.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVObjPersistencyLib.dir/LAVPersistencyDICT.cxx.o
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVPersistency-static.dir/build.make
LAV/libLAVPersistency-static.a: LAV/CMakeFiles/LAVPersistency-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libLAVPersistency-static.a"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV && $(CMAKE_COMMAND) -P CMakeFiles/LAVPersistency-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/LAVPersistency-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
LAV/CMakeFiles/LAVPersistency-static.dir/build: LAV/libLAVPersistency-static.a

.PHONY : LAV/CMakeFiles/LAVPersistency-static.dir/build

LAV/CMakeFiles/LAVPersistency-static.dir/requires:

.PHONY : LAV/CMakeFiles/LAVPersistency-static.dir/requires

LAV/CMakeFiles/LAVPersistency-static.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV && $(CMAKE_COMMAND) -P CMakeFiles/LAVPersistency-static.dir/cmake_clean.cmake
.PHONY : LAV/CMakeFiles/LAVPersistency-static.dir/clean

LAV/CMakeFiles/LAVPersistency-static.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVPersistency-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : LAV/CMakeFiles/LAVPersistency-static.dir/depend


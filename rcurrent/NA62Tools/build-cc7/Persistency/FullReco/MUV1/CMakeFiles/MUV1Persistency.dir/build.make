# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.11

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
CMAKE_COMMAND = /cvmfs/sft.cern.ch/lcg/releases/CMake/3.11.1-daf3a/x86_64-centos7-gcc7-opt/bin/cmake

# The command to remove a file.
RM = /cvmfs/sft.cern.ch/lcg/releases/CMake/3.11.1-daf3a/x86_64-centos7-gcc7-opt/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62Tools

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7

# Include any dependencies generated for this target.
include Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/depend.make

# Include the progress variables for this target.
include Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/flags.make

# Object files for target MUV1Persistency
MUV1Persistency_OBJECTS =

# External object files for target MUV1Persistency
MUV1Persistency_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/MUV1ChannelID.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TMUV1Digi.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TMUV1Event.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TMUV1Hit.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TRecoMUV1Candidate.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TRecoMUV1Event.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TRecoMUV1Hit.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/MUV1PersistencyDICT.cxx.o"

Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/MUV1ChannelID.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TMUV1Digi.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TMUV1Event.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TMUV1Hit.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TRecoMUV1Candidate.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TRecoMUV1Event.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/src/TRecoMUV1Hit.cc.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1ObjPersistencyLib.dir/MUV1PersistencyDICT.cxx.o
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/build.make
Persistency/FullReco/MUV1/libMUV1Persistency.so: Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libMUV1Persistency.so"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1 && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MUV1Persistency.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/build: Persistency/FullReco/MUV1/libMUV1Persistency.so

.PHONY : Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/build

Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1 && $(CMAKE_COMMAND) -P CMakeFiles/MUV1Persistency.dir/cmake_clean.cmake
.PHONY : Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/clean

Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Tools /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/MUV1 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/FullReco/MUV1/CMakeFiles/MUV1Persistency.dir/depend


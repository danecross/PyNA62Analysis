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
include Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/depend.make

# Include the progress variables for this target.
include Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/flags.make

# Object files for target CedarSlimPersistency
CedarSlimPersistency_OBJECTS =

# External object files for target CedarSlimPersistency
CedarSlimPersistency_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/src/TSlimRecoCedarCandidate.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/src/TSlimRecoCedarEvent.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/src/TSlimRecoCedarHit.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/CedarSlimPersistencyDICT.cxx.o"

Persistency/SlimReco/Cedar/libCedarSlimPersistency.so: Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/src/TSlimRecoCedarCandidate.cc.o
Persistency/SlimReco/Cedar/libCedarSlimPersistency.so: Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/src/TSlimRecoCedarEvent.cc.o
Persistency/SlimReco/Cedar/libCedarSlimPersistency.so: Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/src/TSlimRecoCedarHit.cc.o
Persistency/SlimReco/Cedar/libCedarSlimPersistency.so: Persistency/SlimReco/Cedar/CMakeFiles/CedarObjSlimPersistencyLib.dir/CedarSlimPersistencyDICT.cxx.o
Persistency/SlimReco/Cedar/libCedarSlimPersistency.so: Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/build.make
Persistency/SlimReco/Cedar/libCedarSlimPersistency.so: Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libCedarSlimPersistency.so"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/CedarSlimPersistency.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/build: Persistency/SlimReco/Cedar/libCedarSlimPersistency.so

.PHONY : Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/build

Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar && $(CMAKE_COMMAND) -P CMakeFiles/CedarSlimPersistency.dir/cmake_clean.cmake
.PHONY : Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/clean

Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Tools /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/Cedar /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/SlimReco/Cedar/CMakeFiles/CedarSlimPersistency.dir/depend


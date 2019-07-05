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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7

# Include any dependencies generated for this target.
include Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/depend.make

# Include the progress variables for this target.
include Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/flags.make

# Object files for target SAVSlimPersistency
SAVSlimPersistency_OBJECTS =

# External object files for target SAVSlimPersistency
SAVSlimPersistency_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/src/TSlimRecoSAVCandidate.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/src/TSlimRecoSAVEvent.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/src/TSlimRecoSAVHit.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/SAVSlimPersistencyDICT.cxx.o"

Persistency/SlimReco/SAV/libSAVSlimPersistency.so: Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/src/TSlimRecoSAVCandidate.cc.o
Persistency/SlimReco/SAV/libSAVSlimPersistency.so: Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/src/TSlimRecoSAVEvent.cc.o
Persistency/SlimReco/SAV/libSAVSlimPersistency.so: Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/src/TSlimRecoSAVHit.cc.o
Persistency/SlimReco/SAV/libSAVSlimPersistency.so: Persistency/SlimReco/SAV/CMakeFiles/SAVObjSlimPersistencyLib.dir/SAVSlimPersistencyDICT.cxx.o
Persistency/SlimReco/SAV/libSAVSlimPersistency.so: Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/build.make
Persistency/SlimReco/SAV/libSAVSlimPersistency.so: Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libSAVSlimPersistency.so"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/SAVSlimPersistency.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/build: Persistency/SlimReco/SAV/libSAVSlimPersistency.so

.PHONY : Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/build

Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV && $(CMAKE_COMMAND) -P CMakeFiles/SAVSlimPersistency.dir/cmake_clean.cmake
.PHONY : Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/clean

Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/SAV /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/SlimReco/SAV/CMakeFiles/SAVSlimPersistency.dir/depend


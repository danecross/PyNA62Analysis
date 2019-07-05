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
include Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/depend.make

# Include the progress variables for this target.
include Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/flags.make

# Object files for target MUV3SlimPersistency
MUV3SlimPersistency_OBJECTS =

# External object files for target MUV3SlimPersistency
MUV3SlimPersistency_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/src/TSlimRecoMUV3Candidate.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/src/TSlimRecoMUV3Event.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/src/TSlimRecoMUV3Hit.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/MUV3SlimPersistencyDICT.cxx.o"

Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so: Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/src/TSlimRecoMUV3Candidate.cc.o
Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so: Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/src/TSlimRecoMUV3Event.cc.o
Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so: Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/src/TSlimRecoMUV3Hit.cc.o
Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so: Persistency/SlimReco/MUV3/CMakeFiles/MUV3ObjSlimPersistencyLib.dir/MUV3SlimPersistencyDICT.cxx.o
Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so: Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/build.make
Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so: Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libMUV3SlimPersistency.so"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3 && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MUV3SlimPersistency.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/build: Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so

.PHONY : Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/build

Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3 && $(CMAKE_COMMAND) -P CMakeFiles/MUV3SlimPersistency.dir/cmake_clean.cmake
.PHONY : Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/clean

Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/SlimReco/MUV3/CMakeFiles/MUV3SlimPersistency.dir/depend


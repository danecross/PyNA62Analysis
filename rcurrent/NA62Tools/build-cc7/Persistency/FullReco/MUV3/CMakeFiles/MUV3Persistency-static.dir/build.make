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
include Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/depend.make

# Include the progress variables for this target.
include Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/flags.make

# Object files for target MUV3Persistency-static
MUV3Persistency__static_OBJECTS =

# External object files for target MUV3Persistency-static
MUV3Persistency__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/MUV3ChannelID.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TMUV3Digi.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TMUV3Event.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TMUV3Hit.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TRecoMUV3Candidate.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TRecoMUV3Event.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TRecoMUV3Hit.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/MUV3PersistencyDICT.cxx.o"

Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/MUV3ChannelID.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TMUV3Digi.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TMUV3Event.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TMUV3Hit.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TRecoMUV3Candidate.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TRecoMUV3Event.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/src/TRecoMUV3Hit.cc.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3ObjPersistencyLib.dir/MUV3PersistencyDICT.cxx.o
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/build.make
Persistency/FullReco/MUV3/libMUV3Persistency-static.a: Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libMUV3Persistency-static.a"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3 && $(CMAKE_COMMAND) -P CMakeFiles/MUV3Persistency-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3 && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MUV3Persistency-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/build: Persistency/FullReco/MUV3/libMUV3Persistency-static.a

.PHONY : Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/build

Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3 && $(CMAKE_COMMAND) -P CMakeFiles/MUV3Persistency-static.dir/cmake_clean.cmake
.PHONY : Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/clean

Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/FullReco/MUV3 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/FullReco/MUV3/CMakeFiles/MUV3Persistency-static.dir/depend

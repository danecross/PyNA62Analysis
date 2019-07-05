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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7

# Include any dependencies generated for this target.
include MUV0/CMakeFiles/MUV0-static.dir/depend.make

# Include the progress variables for this target.
include MUV0/CMakeFiles/MUV0-static.dir/progress.make

# Include the compile flags for this target's objects.
include MUV0/CMakeFiles/MUV0-static.dir/flags.make

# Object files for target MUV0-static
MUV0__static_OBJECTS =

# External object files for target MUV0-static
MUV0__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Channel.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Digitizer.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Geometry.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0OnlineMonitor.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0RawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0RawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Reconstruction.cc.o"

MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Channel.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Digitizer.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Geometry.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0OnlineMonitor.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0RawDecoder.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0RawEncoder.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0ObjLib.dir/src/MUV0Reconstruction.cc.o
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0-static.dir/build.make
MUV0/libMUV0-static.a: MUV0/CMakeFiles/MUV0-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libMUV0-static.a"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0 && $(CMAKE_COMMAND) -P CMakeFiles/MUV0-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0 && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MUV0-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
MUV0/CMakeFiles/MUV0-static.dir/build: MUV0/libMUV0-static.a

.PHONY : MUV0/CMakeFiles/MUV0-static.dir/build

MUV0/CMakeFiles/MUV0-static.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0 && $(CMAKE_COMMAND) -P CMakeFiles/MUV0-static.dir/cmake_clean.cmake
.PHONY : MUV0/CMakeFiles/MUV0-static.dir/clean

MUV0/CMakeFiles/MUV0-static.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/MUV0 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/MUV0/CMakeFiles/MUV0-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : MUV0/CMakeFiles/MUV0-static.dir/depend


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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7

# Include any dependencies generated for this target.
include MUV2/CMakeFiles/MUV2.dir/depend.make

# Include the progress variables for this target.
include MUV2/CMakeFiles/MUV2.dir/progress.make

# Include the compile flags for this target's objects.
include MUV2/CMakeFiles/MUV2.dir/flags.make

# Object files for target MUV2
MUV2_OBJECTS =

# External object files for target MUV2
MUV2_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2Digitizer.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2Geometry.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2HitsCluster.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2OnlineMonitor.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2RawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2RawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2Reconstruction.cc.o"

MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2Digitizer.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2Geometry.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2HitsCluster.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2OnlineMonitor.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2RawDecoder.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2RawEncoder.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2ObjLib.dir/src/MUV2Reconstruction.cc.o
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2.dir/build.make
MUV2/libMUV2.so: MUV2/CMakeFiles/MUV2.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libMUV2.so"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2 && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/MUV2.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
MUV2/CMakeFiles/MUV2.dir/build: MUV2/libMUV2.so

.PHONY : MUV2/CMakeFiles/MUV2.dir/build

MUV2/CMakeFiles/MUV2.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2 && $(CMAKE_COMMAND) -P CMakeFiles/MUV2.dir/cmake_clean.cmake
.PHONY : MUV2/CMakeFiles/MUV2.dir/clean

MUV2/CMakeFiles/MUV2.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/MUV2 /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2 /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/MUV2/CMakeFiles/MUV2.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : MUV2/CMakeFiles/MUV2.dir/depend


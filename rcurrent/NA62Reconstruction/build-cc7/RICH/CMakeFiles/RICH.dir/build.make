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
include RICH/CMakeFiles/RICH.dir/depend.make

# Include the progress variables for this target.
include RICH/CMakeFiles/RICH.dir/progress.make

# Include the compile flags for this target's objects.
include RICH/CMakeFiles/RICH.dir/flags.make

# Object files for target RICH
RICH_OBJECTS =

# External object files for target RICH
RICH_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICHObjLib.dir/src/RICHChannel.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICHObjLib.dir/src/RICHDigitizer.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICHObjLib.dir/src/RICHOnlineMonitor.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICHObjLib.dir/src/RICHRawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICHObjLib.dir/src/RICHRawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICHObjLib.dir/src/RICHReconstruction.cc.o"

RICH/libRICH.so: RICH/CMakeFiles/RICHObjLib.dir/src/RICHChannel.cc.o
RICH/libRICH.so: RICH/CMakeFiles/RICHObjLib.dir/src/RICHDigitizer.cc.o
RICH/libRICH.so: RICH/CMakeFiles/RICHObjLib.dir/src/RICHOnlineMonitor.cc.o
RICH/libRICH.so: RICH/CMakeFiles/RICHObjLib.dir/src/RICHRawDecoder.cc.o
RICH/libRICH.so: RICH/CMakeFiles/RICHObjLib.dir/src/RICHRawEncoder.cc.o
RICH/libRICH.so: RICH/CMakeFiles/RICHObjLib.dir/src/RICHReconstruction.cc.o
RICH/libRICH.so: RICH/CMakeFiles/RICH.dir/build.make
RICH/libRICH.so: RICH/CMakeFiles/RICH.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libRICH.so"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/RICH.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
RICH/CMakeFiles/RICH.dir/build: RICH/libRICH.so

.PHONY : RICH/CMakeFiles/RICH.dir/build

RICH/CMakeFiles/RICH.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH && $(CMAKE_COMMAND) -P CMakeFiles/RICH.dir/cmake_clean.cmake
.PHONY : RICH/CMakeFiles/RICH.dir/clean

RICH/CMakeFiles/RICH.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/RICH /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/RICH/CMakeFiles/RICH.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : RICH/CMakeFiles/RICH.dir/depend


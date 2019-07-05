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
include HAC/CMakeFiles/HAC-static.dir/depend.make

# Include the progress variables for this target.
include HAC/CMakeFiles/HAC-static.dir/progress.make

# Include the compile flags for this target's objects.
include HAC/CMakeFiles/HAC-static.dir/flags.make

# Object files for target HAC-static
HAC__static_OBJECTS =

# External object files for target HAC-static
HAC__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACChannel.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACDigitizer.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACGeometry.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACOnlineMonitor.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACRawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACRawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HACObjLib.dir/src/HACReconstruction.cc.o"

HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACChannel.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACDigitizer.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACGeometry.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACOnlineMonitor.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACRawDecoder.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACRawEncoder.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HACObjLib.dir/src/HACReconstruction.cc.o
HAC/libHAC-static.a: HAC/CMakeFiles/HAC-static.dir/build.make
HAC/libHAC-static.a: HAC/CMakeFiles/HAC-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libHAC-static.a"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC && $(CMAKE_COMMAND) -P CMakeFiles/HAC-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/HAC-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
HAC/CMakeFiles/HAC-static.dir/build: HAC/libHAC-static.a

.PHONY : HAC/CMakeFiles/HAC-static.dir/build

HAC/CMakeFiles/HAC-static.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC && $(CMAKE_COMMAND) -P CMakeFiles/HAC-static.dir/cmake_clean.cmake
.PHONY : HAC/CMakeFiles/HAC-static.dir/clean

HAC/CMakeFiles/HAC-static.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/HAC /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/HAC/CMakeFiles/HAC-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : HAC/CMakeFiles/HAC-static.dir/depend


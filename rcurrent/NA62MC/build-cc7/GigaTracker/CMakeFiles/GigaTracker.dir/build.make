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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7

# Include any dependencies generated for this target.
include GigaTracker/CMakeFiles/GigaTracker.dir/depend.make

# Include the progress variables for this target.
include GigaTracker/CMakeFiles/GigaTracker.dir/progress.make

# Include the compile flags for this target's objects.
include GigaTracker/CMakeFiles/GigaTracker.dir/flags.make

# Object files for target GigaTracker
GigaTracker_OBJECTS =

# External object files for target GigaTracker
GigaTracker_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerBumpBondingParameterisation.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerBumpBonds.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerChip.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerCollimator.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerCoolingPlate.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerDetector.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerGeometryParameters.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerHit.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerMCBMagnet.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerMDXMagnet.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerMaterialParameters.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerPCBModule.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerPixelParameterisation.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerRootIO.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerSD.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerScraperField.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerScraperMagnet.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerSensor.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerSensorAssembly.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerStation.cc.o"

GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerBumpBondingParameterisation.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerBumpBonds.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerChip.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerCollimator.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerCoolingPlate.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerDetector.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerGeometryParameters.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerHit.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerMCBMagnet.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerMDXMagnet.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerMaterialParameters.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerPCBModule.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerPixelParameterisation.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerRootIO.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerSD.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerScraperField.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerScraperMagnet.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerSensor.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerSensorAssembly.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTrackerObjLib.dir/src/GigaTrackerStation.cc.o
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTracker.dir/build.make
GigaTracker/libGigaTracker.so: GigaTracker/CMakeFiles/GigaTracker.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libGigaTracker.so"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/GigaTracker.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
GigaTracker/CMakeFiles/GigaTracker.dir/build: GigaTracker/libGigaTracker.so

.PHONY : GigaTracker/CMakeFiles/GigaTracker.dir/build

GigaTracker/CMakeFiles/GigaTracker.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker && $(CMAKE_COMMAND) -P CMakeFiles/GigaTracker.dir/cmake_clean.cmake
.PHONY : GigaTracker/CMakeFiles/GigaTracker.dir/clean

GigaTracker/CMakeFiles/GigaTracker.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/GigaTracker /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/CMakeFiles/GigaTracker.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : GigaTracker/CMakeFiles/GigaTracker.dir/depend

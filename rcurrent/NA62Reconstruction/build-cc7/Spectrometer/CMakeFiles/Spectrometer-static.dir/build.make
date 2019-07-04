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
include Spectrometer/CMakeFiles/Spectrometer-static.dir/depend.make

# Include the progress variables for this target.
include Spectrometer/CMakeFiles/Spectrometer-static.dir/progress.make

# Include the compile flags for this target's objects.
include Spectrometer/CMakeFiles/Spectrometer-static.dir/flags.make

# Object files for target Spectrometer-static
Spectrometer__static_OBJECTS =

# External object files for target Spectrometer-static
Spectrometer__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/ChamberHitCollector.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Cluster.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Combination.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Intersection.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SRBRawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SRBRawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDigiManager.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDigitizer.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometry.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerOnlineMonitor.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerParameters.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerReconstruction.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/StrawHitCollector.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/StrawResponse.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/T0Jumps.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Track.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/TrackCollector.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/ViewHitCollector.cc.o"

Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/ChamberHitCollector.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Cluster.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Combination.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Intersection.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SRBRawDecoder.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SRBRawEncoder.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDigiManager.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDigitizer.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometry.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerOnlineMonitor.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerParameters.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRawDecoder.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRawEncoder.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerReconstruction.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/StrawHitCollector.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/StrawResponse.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/T0Jumps.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Track.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/TrackCollector.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/ViewHitCollector.cc.o
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/Spectrometer-static.dir/build.make
Spectrometer/libSpectrometer-static.a: Spectrometer/CMakeFiles/Spectrometer-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libSpectrometer-static.a"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer && $(CMAKE_COMMAND) -P CMakeFiles/Spectrometer-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/Spectrometer-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Spectrometer/CMakeFiles/Spectrometer-static.dir/build: Spectrometer/libSpectrometer-static.a

.PHONY : Spectrometer/CMakeFiles/Spectrometer-static.dir/build

Spectrometer/CMakeFiles/Spectrometer-static.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer && $(CMAKE_COMMAND) -P CMakeFiles/Spectrometer-static.dir/cmake_clean.cmake
.PHONY : Spectrometer/CMakeFiles/Spectrometer-static.dir/clean

Spectrometer/CMakeFiles/Spectrometer-static.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/Spectrometer /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer /afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/build-cc7/Spectrometer/CMakeFiles/Spectrometer-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Spectrometer/CMakeFiles/Spectrometer-static.dir/depend


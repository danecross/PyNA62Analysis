# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

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
CMAKE_COMMAND = /cvmfs/sft.cern.ch/lcg/releases/CMake/3.5.2-a9b03/x86_64-slc6-gcc49-opt/bin/cmake

# The command to remove a file.
RM = /cvmfs/sft.cern.ch/lcg/releases/CMake/3.5.2-a9b03/x86_64-slc6-gcc49-opt/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6

# Include any dependencies generated for this target.
include LAV/CMakeFiles/LAV.dir/depend.make

# Include the progress variables for this target.
include LAV/CMakeFiles/LAV.dir/progress.make

# Include the compile flags for this target's objects.
include LAV/CMakeFiles/LAV.dir/flags.make

# Object files for target LAV
LAV_OBJECTS =

# External object files for target LAV
LAV_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVAccurateBlock.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVBanana.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVDetector.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVDetectorMessenger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVGeometryParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVMaterialParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVOptTrack.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVPbGlBlock.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVPhotoMultiplier.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVRootIO.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVSD.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVSampleMatrix.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAVObjLib.dir/src/LAVVPbGlBlock.cc.o"

LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVAccurateBlock.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVBanana.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVDetector.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVDetectorMessenger.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVGeometryParameters.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVHit.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVMaterialParameters.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVOptTrack.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVPbGlBlock.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVPhotoMultiplier.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVRootIO.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVSD.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVSampleMatrix.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAVObjLib.dir/src/LAVVPbGlBlock.cc.o
LAV/libLAV.so: LAV/CMakeFiles/LAV.dir/build.make
LAV/libLAV.so: LAV/CMakeFiles/LAV.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libLAV.so"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/LAV.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
LAV/CMakeFiles/LAV.dir/build: LAV/libLAV.so

.PHONY : LAV/CMakeFiles/LAV.dir/build

LAV/CMakeFiles/LAV.dir/requires:

.PHONY : LAV/CMakeFiles/LAV.dir/requires

LAV/CMakeFiles/LAV.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV && $(CMAKE_COMMAND) -P CMakeFiles/LAV.dir/cmake_clean.cmake
.PHONY : LAV/CMakeFiles/LAV.dir/clean

LAV/CMakeFiles/LAV.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LAV /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/CMakeFiles/LAV.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : LAV/CMakeFiles/LAV.dir/depend


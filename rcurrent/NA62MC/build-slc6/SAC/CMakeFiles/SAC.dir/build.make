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
include SAC/CMakeFiles/SAC.dir/depend.make

# Include the progress variables for this target.
include SAC/CMakeFiles/SAC.dir/progress.make

# Include the compile flags for this target's objects.
include SAC/CMakeFiles/SAC.dir/flags.make

# Object files for target SAC
SAC_OBJECTS =

# External object files for target SAC
SAC_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o"

SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o
SAC/libSAC.so: SAC/CMakeFiles/SAC.dir/build.make
SAC/libSAC.so: SAC/CMakeFiles/SAC.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libSAC.so"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/SAC.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
SAC/CMakeFiles/SAC.dir/build: SAC/libSAC.so

.PHONY : SAC/CMakeFiles/SAC.dir/build

SAC/CMakeFiles/SAC.dir/requires:

.PHONY : SAC/CMakeFiles/SAC.dir/requires

SAC/CMakeFiles/SAC.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && $(CMAKE_COMMAND) -P CMakeFiles/SAC.dir/cmake_clean.cmake
.PHONY : SAC/CMakeFiles/SAC.dir/clean

SAC/CMakeFiles/SAC.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SAC.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : SAC/CMakeFiles/SAC.dir/depend


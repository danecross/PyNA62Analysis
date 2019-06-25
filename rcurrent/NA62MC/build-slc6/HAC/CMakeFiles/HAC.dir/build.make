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
include HAC/CMakeFiles/HAC.dir/depend.make

# Include the progress variables for this target.
include HAC/CMakeFiles/HAC.dir/progress.make

# Include the compile flags for this target's objects.
include HAC/CMakeFiles/HAC.dir/flags.make

# Object files for target HAC
HAC_OBJECTS =

# External object files for target HAC
HAC_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACAbsorberLayer.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACDetector.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACGeometryParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACMagnet.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACMaterialParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACModule.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACRootIO.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACSD.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HACObjLib.dir/src/HACScintillatorLayer.cc.o"

HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACAbsorberLayer.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACDetector.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACGeometryParameters.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACHit.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACMagnet.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACMaterialParameters.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACModule.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACRootIO.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACSD.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HACObjLib.dir/src/HACScintillatorLayer.cc.o
HAC/libHAC.so: HAC/CMakeFiles/HAC.dir/build.make
HAC/libHAC.so: HAC/CMakeFiles/HAC.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libHAC.so"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/HAC.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
HAC/CMakeFiles/HAC.dir/build: HAC/libHAC.so

.PHONY : HAC/CMakeFiles/HAC.dir/build

HAC/CMakeFiles/HAC.dir/requires:

.PHONY : HAC/CMakeFiles/HAC.dir/requires

HAC/CMakeFiles/HAC.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC && $(CMAKE_COMMAND) -P CMakeFiles/HAC.dir/cmake_clean.cmake
.PHONY : HAC/CMakeFiles/HAC.dir/clean

HAC/CMakeFiles/HAC.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/CMakeFiles/HAC.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : HAC/CMakeFiles/HAC.dir/depend


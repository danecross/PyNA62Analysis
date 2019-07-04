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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62MC

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7

# Include any dependencies generated for this target.
include SAC/CMakeFiles/SAC-static.dir/depend.make

# Include the progress variables for this target.
include SAC/CMakeFiles/SAC-static.dir/progress.make

# Include the compile flags for this target's objects.
include SAC/CMakeFiles/SAC-static.dir/flags.make

# Object files for target SAC-static
SAC__static_OBJECTS =

# External object files for target SAC-static
SAC__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o"

SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o
SAC/libSAC-static.a: SAC/CMakeFiles/SAC-static.dir/build.make
SAC/libSAC-static.a: SAC/CMakeFiles/SAC-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libSAC-static.a"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC && $(CMAKE_COMMAND) -P CMakeFiles/SAC-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/SAC-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
SAC/CMakeFiles/SAC-static.dir/build: SAC/libSAC-static.a

.PHONY : SAC/CMakeFiles/SAC-static.dir/build

SAC/CMakeFiles/SAC-static.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC && $(CMAKE_COMMAND) -P CMakeFiles/SAC-static.dir/cmake_clean.cmake
.PHONY : SAC/CMakeFiles/SAC-static.dir/clean

SAC/CMakeFiles/SAC-static.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62MC /afs/cern.ch/user/d/dacross/na62fw/NA62MC/SAC /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC /afs/cern.ch/user/d/dacross/na62fw/NA62MC/build-cc7/SAC/CMakeFiles/SAC-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : SAC/CMakeFiles/SAC-static.dir/depend


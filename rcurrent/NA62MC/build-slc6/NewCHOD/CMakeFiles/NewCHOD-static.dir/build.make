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
include NewCHOD/CMakeFiles/NewCHOD-static.dir/depend.make

# Include the progress variables for this target.
include NewCHOD/CMakeFiles/NewCHOD-static.dir/progress.make

# Include the compile flags for this target's objects.
include NewCHOD/CMakeFiles/NewCHOD-static.dir/flags.make

# Object files for target NewCHOD-static
NewCHOD__static_OBJECTS =

# External object files for target NewCHOD-static
NewCHOD__static_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODDetector.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODGeometryParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODMaterialParameters.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODRootIO.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODSD.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODScintillatorCounter.cc.o"

NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODDetector.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODGeometryParameters.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODHit.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODMaterialParameters.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODRootIO.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODSD.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHODObjLib.dir/src/NewCHODScintillatorCounter.cc.o
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHOD-static.dir/build.make
NewCHOD/libNewCHOD-static.a: NewCHOD/CMakeFiles/NewCHOD-static.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX static library libNewCHOD-static.a"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD && $(CMAKE_COMMAND) -P CMakeFiles/NewCHOD-static.dir/cmake_clean_target.cmake
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/NewCHOD-static.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
NewCHOD/CMakeFiles/NewCHOD-static.dir/build: NewCHOD/libNewCHOD-static.a

.PHONY : NewCHOD/CMakeFiles/NewCHOD-static.dir/build

NewCHOD/CMakeFiles/NewCHOD-static.dir/requires:

.PHONY : NewCHOD/CMakeFiles/NewCHOD-static.dir/requires

NewCHOD/CMakeFiles/NewCHOD-static.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD && $(CMAKE_COMMAND) -P CMakeFiles/NewCHOD-static.dir/cmake_clean.cmake
.PHONY : NewCHOD/CMakeFiles/NewCHOD-static.dir/clean

NewCHOD/CMakeFiles/NewCHOD-static.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/NewCHOD /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/CMakeFiles/NewCHOD-static.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : NewCHOD/CMakeFiles/NewCHOD-static.dir/depend


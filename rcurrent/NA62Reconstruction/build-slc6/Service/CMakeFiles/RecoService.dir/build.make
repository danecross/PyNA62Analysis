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
CMAKE_SOURCE_DIR = /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6

# Include any dependencies generated for this target.
include Service/CMakeFiles/RecoService.dir/depend.make

# Include the progress variables for this target.
include Service/CMakeFiles/RecoService.dir/progress.make

# Include the compile flags for this target's objects.
include Service/CMakeFiles/RecoService.dir/flags.make

# Object files for target RecoService
RecoService_OBJECTS =

# External object files for target RecoService
RecoService_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service/CMakeFiles/ServiceObjLib.dir/src/TH1Sparse.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service/CMakeFiles/ServiceObjLib.dir/src/TH2Sparse.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service/CMakeFiles/ServiceObjLib.dir/src/Tools.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service/CMakeFiles/ServiceObjLib.dir/src/q.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service/CMakeFiles/ServiceObjLib.dir/src/wzero.cc.o"

Service/libRecoService.so: Service/CMakeFiles/ServiceObjLib.dir/src/TH1Sparse.cc.o
Service/libRecoService.so: Service/CMakeFiles/ServiceObjLib.dir/src/TH2Sparse.cc.o
Service/libRecoService.so: Service/CMakeFiles/ServiceObjLib.dir/src/Tools.cc.o
Service/libRecoService.so: Service/CMakeFiles/ServiceObjLib.dir/src/q.cc.o
Service/libRecoService.so: Service/CMakeFiles/ServiceObjLib.dir/src/wzero.cc.o
Service/libRecoService.so: Service/CMakeFiles/RecoService.dir/build.make
Service/libRecoService.so: Service/CMakeFiles/RecoService.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libRecoService.so"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/RecoService.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Service/CMakeFiles/RecoService.dir/build: Service/libRecoService.so

.PHONY : Service/CMakeFiles/RecoService.dir/build

Service/CMakeFiles/RecoService.dir/requires:

.PHONY : Service/CMakeFiles/RecoService.dir/requires

Service/CMakeFiles/RecoService.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service && $(CMAKE_COMMAND) -P CMakeFiles/RecoService.dir/cmake_clean.cmake
.PHONY : Service/CMakeFiles/RecoService.dir/clean

Service/CMakeFiles/RecoService.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Service /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/Service/CMakeFiles/RecoService.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Service/CMakeFiles/RecoService.dir/depend


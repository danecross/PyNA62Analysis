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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62Tools

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7

# Include any dependencies generated for this target.
include Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/depend.make

# Include the progress variables for this target.
include Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/flags.make

# Object files for target CHANTISlimPersistency
CHANTISlimPersistency_OBJECTS =

# External object files for target CHANTISlimPersistency
CHANTISlimPersistency_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/src/TSlimRecoCHANTICandidate.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/src/TSlimRecoCHANTIEvent.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/src/TSlimRecoCHANTIHit.cc.o" \
"/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/CHANTISlimPersistencyDICT.cxx.o"

Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so: Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/src/TSlimRecoCHANTICandidate.cc.o
Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so: Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/src/TSlimRecoCHANTIEvent.cc.o
Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so: Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/src/TSlimRecoCHANTIHit.cc.o
Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so: Persistency/SlimReco/CHANTI/CMakeFiles/CHANTIObjSlimPersistencyLib.dir/CHANTISlimPersistencyDICT.cxx.o
Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so: Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/build.make
Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so: Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libCHANTISlimPersistency.so"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/CHANTISlimPersistency.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/build: Persistency/SlimReco/CHANTI/libCHANTISlimPersistency.so

.PHONY : Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/build

Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI && $(CMAKE_COMMAND) -P CMakeFiles/CHANTISlimPersistency.dir/cmake_clean.cmake
.PHONY : Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/clean

Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Tools /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/CHANTI /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/SlimReco/CHANTI/CMakeFiles/CHANTISlimPersistency.dir/depend


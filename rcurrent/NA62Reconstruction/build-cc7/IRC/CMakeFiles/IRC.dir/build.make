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
include IRC/CMakeFiles/IRC.dir/depend.make

# Include the progress variables for this target.
include IRC/CMakeFiles/IRC.dir/progress.make

# Include the compile flags for this target's objects.
include IRC/CMakeFiles/IRC.dir/flags.make

# Object files for target IRC
IRC_OBJECTS =

# External object files for target IRC
IRC_EXTERNAL_OBJECTS = \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCChannel.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCDigitizer.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCGeometry.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCOnlineMonitor.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCRawDecoder.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCRawEncoder.cc.o" \
"/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRCObjLib.dir/src/IRCReconstruction.cc.o"

IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCChannel.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCDigitizer.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCGeometry.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCOnlineMonitor.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCRawDecoder.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCRawEncoder.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRCObjLib.dir/src/IRCReconstruction.cc.o
IRC/libIRC.so: IRC/CMakeFiles/IRC.dir/build.make
IRC/libIRC.so: IRC/CMakeFiles/IRC.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libIRC.so"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/IRC.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
IRC/CMakeFiles/IRC.dir/build: IRC/libIRC.so

.PHONY : IRC/CMakeFiles/IRC.dir/build

IRC/CMakeFiles/IRC.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC && $(CMAKE_COMMAND) -P CMakeFiles/IRC.dir/cmake_clean.cmake
.PHONY : IRC/CMakeFiles/IRC.dir/clean

IRC/CMakeFiles/IRC.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/IRC /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/IRC/CMakeFiles/IRC.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : IRC/CMakeFiles/IRC.dir/depend


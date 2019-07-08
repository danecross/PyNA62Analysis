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
include SAV/CMakeFiles/SAVObjLib.dir/depend.make

# Include the progress variables for this target.
include SAV/CMakeFiles/SAVObjLib.dir/progress.make

# Include the compile flags for this target's objects.
include SAV/CMakeFiles/SAVObjLib.dir/flags.make

SAV/CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.o: SAV/CMakeFiles/SAVObjLib.dir/flags.make
SAV/CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.o: ../SAV/src/SAVDigitizer.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object SAV/CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVDigitizer.cc

SAV/CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVDigitizer.cc > CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.i

SAV/CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVDigitizer.cc -o CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.s

SAV/CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.o: SAV/CMakeFiles/SAVObjLib.dir/flags.make
SAV/CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.o: ../SAV/src/SAVOnlineMonitor.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object SAV/CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVOnlineMonitor.cc

SAV/CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVOnlineMonitor.cc > CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.i

SAV/CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVOnlineMonitor.cc -o CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.s

SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.o: SAV/CMakeFiles/SAVObjLib.dir/flags.make
SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.o: ../SAV/src/SAVRawDecoder.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVRawDecoder.cc

SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVRawDecoder.cc > CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.i

SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVRawDecoder.cc -o CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.s

SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.o: SAV/CMakeFiles/SAVObjLib.dir/flags.make
SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.o: ../SAV/src/SAVRawEncoder.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVRawEncoder.cc

SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVRawEncoder.cc > CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.i

SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVRawEncoder.cc -o CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.s

SAV/CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.o: SAV/CMakeFiles/SAVObjLib.dir/flags.make
SAV/CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.o: ../SAV/src/SAVReconstruction.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object SAV/CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVReconstruction.cc

SAV/CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVReconstruction.cc > CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.i

SAV/CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV/src/SAVReconstruction.cc -o CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.s

SAVObjLib: SAV/CMakeFiles/SAVObjLib.dir/src/SAVDigitizer.cc.o
SAVObjLib: SAV/CMakeFiles/SAVObjLib.dir/src/SAVOnlineMonitor.cc.o
SAVObjLib: SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawDecoder.cc.o
SAVObjLib: SAV/CMakeFiles/SAVObjLib.dir/src/SAVRawEncoder.cc.o
SAVObjLib: SAV/CMakeFiles/SAVObjLib.dir/src/SAVReconstruction.cc.o
SAVObjLib: SAV/CMakeFiles/SAVObjLib.dir/build.make

.PHONY : SAVObjLib

# Rule to build all files generated by this target.
SAV/CMakeFiles/SAVObjLib.dir/build: SAVObjLib

.PHONY : SAV/CMakeFiles/SAVObjLib.dir/build

SAV/CMakeFiles/SAVObjLib.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV && $(CMAKE_COMMAND) -P CMakeFiles/SAVObjLib.dir/cmake_clean.cmake
.PHONY : SAV/CMakeFiles/SAVObjLib.dir/clean

SAV/CMakeFiles/SAVObjLib.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/SAV /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Reconstruction/build-cc7/SAV/CMakeFiles/SAVObjLib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : SAV/CMakeFiles/SAVObjLib.dir/depend

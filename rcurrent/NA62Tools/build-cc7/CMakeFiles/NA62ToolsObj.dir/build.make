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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7

# Include any dependencies generated for this target.
include CMakeFiles/NA62ToolsObj.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/NA62ToolsObj.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/NA62ToolsObj.dir/flags.make

CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.o: ../src/BlueTubeMagneticFieldMap.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/BlueTubeMagneticFieldMap.cc

CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/BlueTubeMagneticFieldMap.cc > CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.i

CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/BlueTubeMagneticFieldMap.cc -o CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.s

CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.o: ../src/BlueTubeTracker.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/BlueTubeTracker.cc

CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/BlueTubeTracker.cc > CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.i

CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/BlueTubeTracker.cc -o CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.s

CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.o: ../src/CHANTIGeometry.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/CHANTIGeometry.cc

CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/CHANTIGeometry.cc > CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.i

CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/CHANTIGeometry.cc -o CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.s

CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.o: ../src/FringeMagneticFieldMap.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/FringeMagneticFieldMap.cc

CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/FringeMagneticFieldMap.cc > CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.i

CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/FringeMagneticFieldMap.cc -o CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.s

CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.o: ../src/GigaTrackerParameterTools.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/GigaTrackerParameterTools.cc

CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/GigaTrackerParameterTools.cc > CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.i

CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/GigaTrackerParameterTools.cc -o CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.s

CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.o: ../src/GitRevision.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/GitRevision.cc

CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/GitRevision.cc > CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.i

CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/GitRevision.cc -o CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.s

CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.o: ../src/MNP33MagneticFieldMap.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/MNP33MagneticFieldMap.cc

CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/MNP33MagneticFieldMap.cc > CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.i

CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/MNP33MagneticFieldMap.cc -o CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.s

CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.o: ../src/MUV3Geometry.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/MUV3Geometry.cc

CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/MUV3Geometry.cc > CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.i

CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/MUV3Geometry.cc -o CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.s

CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.o: ../src/NA62ConditionsService.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62ConditionsService.cc

CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62ConditionsService.cc > CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.i

CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62ConditionsService.cc -o CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.s

CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.o: ../src/NA62Global.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62Global.cc

CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62Global.cc > CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.i

CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62Global.cc -o CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.s

CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.o: ../src/NA62Utilities.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_11) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62Utilities.cc

CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62Utilities.cc > CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.i

CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NA62Utilities.cc -o CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.s

CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.o: ../src/NewCHODGeometry.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_12) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NewCHODGeometry.cc

CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NewCHODGeometry.cc > CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.i

CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/NewCHODGeometry.cc -o CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.s

CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.o: ../src/PersistencyChanger.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_13) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/PersistencyChanger.cc

CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/PersistencyChanger.cc > CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.i

CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/PersistencyChanger.cc -o CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.s

CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.o: CMakeFiles/NA62ToolsObj.dir/flags.make
CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.o: ../src/StringInterpreter.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_14) "Building CXX object CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/StringInterpreter.cc

CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/StringInterpreter.cc > CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.i

CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/src/StringInterpreter.cc -o CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.s

NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/BlueTubeMagneticFieldMap.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/BlueTubeTracker.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/CHANTIGeometry.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/FringeMagneticFieldMap.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/GigaTrackerParameterTools.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/GitRevision.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/MNP33MagneticFieldMap.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/MUV3Geometry.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/NA62ConditionsService.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/NA62Global.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/NA62Utilities.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/NewCHODGeometry.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/PersistencyChanger.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/src/StringInterpreter.cc.o
NA62ToolsObj: CMakeFiles/NA62ToolsObj.dir/build.make

.PHONY : NA62ToolsObj

# Rule to build all files generated by this target.
CMakeFiles/NA62ToolsObj.dir/build: NA62ToolsObj

.PHONY : CMakeFiles/NA62ToolsObj.dir/build

CMakeFiles/NA62ToolsObj.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/NA62ToolsObj.dir/cmake_clean.cmake
.PHONY : CMakeFiles/NA62ToolsObj.dir/clean

CMakeFiles/NA62ToolsObj.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/CMakeFiles/NA62ToolsObj.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/NA62ToolsObj.dir/depend


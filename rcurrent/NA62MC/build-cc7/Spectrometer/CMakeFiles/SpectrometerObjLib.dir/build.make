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
CMAKE_SOURCE_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7

# Include any dependencies generated for this target.
include Spectrometer/CMakeFiles/SpectrometerObjLib.dir/depend.make

# Include the progress variables for this target.
include Spectrometer/CMakeFiles/SpectrometerObjLib.dir/progress.make

# Include the compile flags for this target's objects.
include Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.o: ../Spectrometer/src/Chamber.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/Chamber.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/Chamber.cc > CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/Chamber.cc -o CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.o: ../Spectrometer/src/HalfView.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/HalfView.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/HalfView.cc > CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/HalfView.cc -o CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.o: ../Spectrometer/src/SpectrometerDetector.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerDetector.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerDetector.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerDetector.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.o: ../Spectrometer/src/SpectrometerGeometryParameters.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerGeometryParameters.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerGeometryParameters.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerGeometryParameters.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.o: ../Spectrometer/src/SpectrometerHit.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerHit.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerHit.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerHit.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.o: ../Spectrometer/src/SpectrometerMagnet.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerMagnet.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerMagnet.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerMagnet.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.o: ../Spectrometer/src/SpectrometerMaterialParameters.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerMaterialParameters.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerMaterialParameters.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerMaterialParameters.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.o: ../Spectrometer/src/SpectrometerRootIO.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerRootIO.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerRootIO.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerRootIO.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.o: ../Spectrometer/src/SpectrometerSD.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerSD.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerSD.cc > CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/SpectrometerSD.cc -o CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o: ../Spectrometer/src/Straw.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/Straw.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/Straw.cc > CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/Straw.cc -o CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.s

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/View.cc.o: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/flags.make
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/View.cc.o: ../Spectrometer/src/View.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_11) "Building CXX object Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/View.cc.o"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SpectrometerObjLib.dir/src/View.cc.o -c /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/View.cc

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/View.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SpectrometerObjLib.dir/src/View.cc.i"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/View.cc > CMakeFiles/SpectrometerObjLib.dir/src/View.cc.i

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/View.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SpectrometerObjLib.dir/src/View.cc.s"
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer/src/View.cc -o CMakeFiles/SpectrometerObjLib.dir/src/View.cc.s

SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Chamber.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/HalfView.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerDetector.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerGeometryParameters.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerHit.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMagnet.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerMaterialParameters.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerRootIO.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/SpectrometerSD.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/Straw.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/src/View.cc.o
SpectrometerObjLib: Spectrometer/CMakeFiles/SpectrometerObjLib.dir/build.make

.PHONY : SpectrometerObjLib

# Rule to build all files generated by this target.
Spectrometer/CMakeFiles/SpectrometerObjLib.dir/build: SpectrometerObjLib

.PHONY : Spectrometer/CMakeFiles/SpectrometerObjLib.dir/build

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/clean:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer && $(CMAKE_COMMAND) -P CMakeFiles/SpectrometerObjLib.dir/cmake_clean.cmake
.PHONY : Spectrometer/CMakeFiles/SpectrometerObjLib.dir/clean

Spectrometer/CMakeFiles/SpectrometerObjLib.dir/depend:
	cd /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/Spectrometer /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7 /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer/CMakeFiles/SpectrometerObjLib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Spectrometer/CMakeFiles/SpectrometerObjLib.dir/depend

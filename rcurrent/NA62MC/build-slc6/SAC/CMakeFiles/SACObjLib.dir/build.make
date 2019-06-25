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
include SAC/CMakeFiles/SACObjLib.dir/depend.make

# Include the progress variables for this target.
include SAC/CMakeFiles/SACObjLib.dir/progress.make

# Include the compile flags for this target's objects.
include SAC/CMakeFiles/SACObjLib.dir/flags.make

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o: ../SAC/src/SACAbsorberLayer.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACAbsorberLayer.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACAbsorberLayer.cc > CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACAbsorberLayer.cc -o CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o: ../SAC/src/SACAbsorberScintillator.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACAbsorberScintillator.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACAbsorberScintillator.cc > CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACAbsorberScintillator.cc -o CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o: ../SAC/src/SACDetector.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACDetector.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACDetector.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACDetector.cc > CMakeFiles/SACObjLib.dir/src/SACDetector.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACDetector.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACDetector.cc -o CMakeFiles/SACObjLib.dir/src/SACDetector.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o: ../SAC/src/SACGeometryParameters.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACGeometryParameters.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACGeometryParameters.cc > CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACGeometryParameters.cc -o CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o: ../SAC/src/SACHit.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACHit.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACHit.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACHit.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACHit.cc > CMakeFiles/SACObjLib.dir/src/SACHit.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACHit.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACHit.cc -o CMakeFiles/SACObjLib.dir/src/SACHit.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o: ../SAC/src/SACMaterialParameters.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACMaterialParameters.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACMaterialParameters.cc > CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACMaterialParameters.cc -o CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o: ../SAC/src/SACRootIO.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACRootIO.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACRootIO.cc > CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACRootIO.cc -o CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o: ../SAC/src/SACSD.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACSD.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACSD.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACSD.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACSD.cc > CMakeFiles/SACObjLib.dir/src/SACSD.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACSD.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACSD.cc -o CMakeFiles/SACObjLib.dir/src/SACSD.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o: ../SAC/src/SACScintillatorLayer.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACScintillatorLayer.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACScintillatorLayer.cc > CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACScintillatorLayer.cc -o CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o


SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o: SAC/CMakeFiles/SACObjLib.dir/flags.make
SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o: ../SAC/src/SACSegment.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Building CXX object SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACSegment.cc

SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjLib.dir/src/SACSegment.cc.i"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACSegment.cc > CMakeFiles/SACObjLib.dir/src/SACSegment.cc.i

SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjLib.dir/src/SACSegment.cc.s"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC/src/SACSegment.cc -o CMakeFiles/SACObjLib.dir/src/SACSegment.cc.s

SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.requires:

.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.requires

SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.provides: SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.requires
	$(MAKE) -f SAC/CMakeFiles/SACObjLib.dir/build.make SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.provides.build
.PHONY : SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.provides

SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.provides.build: SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o


SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o
SACObjLib: SAC/CMakeFiles/SACObjLib.dir/build.make

.PHONY : SACObjLib

# Rule to build all files generated by this target.
SAC/CMakeFiles/SACObjLib.dir/build: SACObjLib

.PHONY : SAC/CMakeFiles/SACObjLib.dir/build

SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberLayer.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACAbsorberScintillator.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACDetector.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACGeometryParameters.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACHit.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACMaterialParameters.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACRootIO.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACSD.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACScintillatorLayer.cc.o.requires
SAC/CMakeFiles/SACObjLib.dir/requires: SAC/CMakeFiles/SACObjLib.dir/src/SACSegment.cc.o.requires

.PHONY : SAC/CMakeFiles/SACObjLib.dir/requires

SAC/CMakeFiles/SACObjLib.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC && $(CMAKE_COMMAND) -P CMakeFiles/SACObjLib.dir/cmake_clean.cmake
.PHONY : SAC/CMakeFiles/SACObjLib.dir/clean

SAC/CMakeFiles/SACObjLib.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/SAC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/CMakeFiles/SACObjLib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : SAC/CMakeFiles/SACObjLib.dir/depend


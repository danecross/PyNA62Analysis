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
include CMakeFiles/NA62Reco.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/NA62Reco.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/NA62Reco.dir/flags.make

CMakeFiles/NA62Reco.dir/NA62Reco.cc.o: CMakeFiles/NA62Reco.dir/flags.make
CMakeFiles/NA62Reco.dir/NA62Reco.cc.o: ../NA62Reco.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/NA62Reco.dir/NA62Reco.cc.o"
	/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++   $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/NA62Reco.dir/NA62Reco.cc.o -c /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/NA62Reco.cc

CMakeFiles/NA62Reco.dir/NA62Reco.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/NA62Reco.dir/NA62Reco.cc.i"
	/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/NA62Reco.cc > CMakeFiles/NA62Reco.dir/NA62Reco.cc.i

CMakeFiles/NA62Reco.dir/NA62Reco.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/NA62Reco.dir/NA62Reco.cc.s"
	/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/NA62Reco.cc -o CMakeFiles/NA62Reco.dir/NA62Reco.cc.s

CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.requires:

.PHONY : CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.requires

CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.provides: CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.requires
	$(MAKE) -f CMakeFiles/NA62Reco.dir/build.make CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.provides.build
.PHONY : CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.provides

CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.provides.build: CMakeFiles/NA62Reco.dir/NA62Reco.cc.o


# Object files for target NA62Reco
NA62Reco_OBJECTS = \
"CMakeFiles/NA62Reco.dir/NA62Reco.cc.o"

# External object files for target NA62Reco
NA62Reco_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles/MainObjLib.dir/src/NA62OnlineMonitor.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles/MainObjLib.dir/src/NA62Reconstruction.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles/MainObjLib.dir/src/Timer.cc.o"

NA62Reco: CMakeFiles/NA62Reco.dir/NA62Reco.cc.o
NA62Reco: CMakeFiles/MainObjLib.dir/src/NA62OnlineMonitor.cc.o
NA62Reco: CMakeFiles/MainObjLib.dir/src/NA62Reconstruction.cc.o
NA62Reco: CMakeFiles/MainObjLib.dir/src/Timer.cc.o
NA62Reco: CMakeFiles/NA62Reco.dir/build.make
NA62Reco: Cedar/libCedar-static.a
NA62Reco: CHANTI/libCHANTI-static.a
NA62Reco: CHOD/libCHOD-static.a
NA62Reco: NewCHOD/libNewCHOD-static.a
NA62Reco: GigaTracker/libGigaTracker-static.a
NA62Reco: HAC/libHAC-static.a
NA62Reco: IRC/libIRC-static.a
NA62Reco: LAV/libLAV-static.a
NA62Reco: LKr/libLKr-static.a
NA62Reco: MUV0/libMUV0-static.a
NA62Reco: MUV1/libMUV1-static.a
NA62Reco: MUV2/libMUV2-static.a
NA62Reco: MUV3/libMUV3-static.a
NA62Reco: RICH/libRICH-static.a
NA62Reco: SAC/libSAC-static.a
NA62Reco: SAV/libSAV-static.a
NA62Reco: Spectrometer/libSpectrometer-static.a
NA62Reco: RecoBase/libRecoBase-static.a
NA62Reco: EventDisplay/libEventDisplay-static.a
NA62Reco: Service/libRecoService-static.a
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libCore.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libRIO.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libNet.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libHist.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGraf.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGraf3d.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGpad.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libTree.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libRint.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libPostscript.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMatrix.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libPhysics.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMathCore.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libThread.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMultiProc.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libTMVA.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libEve.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libRGL.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libFTGL.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGed.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libSpectrum.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMinuit.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMLP.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libXMLIO.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGeom.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libEG.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGui.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libTreePlayer.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGraf3d.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libTree.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libNet.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGpad.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libGraf.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libRIO.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libThread.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libPhysics.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libHist.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMatrix.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libMathCore.so
NA62Reco: /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib/libCore.so
NA62Reco: CMakeFiles/NA62Reco.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable NA62Reco"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/NA62Reco.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/NA62Reco.dir/build: NA62Reco

.PHONY : CMakeFiles/NA62Reco.dir/build

CMakeFiles/NA62Reco.dir/requires: CMakeFiles/NA62Reco.dir/NA62Reco.cc.o.requires

.PHONY : CMakeFiles/NA62Reco.dir/requires

CMakeFiles/NA62Reco.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/NA62Reco.dir/cmake_clean.cmake
.PHONY : CMakeFiles/NA62Reco.dir/clean

CMakeFiles/NA62Reco.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles/NA62Reco.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/NA62Reco.dir/depend


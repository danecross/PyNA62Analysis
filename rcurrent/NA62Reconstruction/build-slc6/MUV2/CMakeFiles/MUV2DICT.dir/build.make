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

# Utility rule file for MUV2DICT.

# Include the progress variables for this target.
include MUV2/CMakeFiles/MUV2DICT.dir/progress.make

MUV2/CMakeFiles/MUV2DICT: MUV2/MUV2DICT.cxx


MUV2/MUV2DICT.cxx: ../MUV2/MUV2LinkDef.hh
MUV2/MUV2DICT.cxx: ../MUV2/include/MUV2HitsCluster.hh
MUV2/MUV2DICT.cxx: ../MUV2/MUV2LinkDef.hh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating MUV2DICT.cxx, libMUV2_rdict.pcm, libMUV2.rootmap"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV2 && /cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/bin/rootcling -f MUV2DICT.cxx -s /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV2/libMUV2.so -rml libMUV2.so -rmf /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV2/libMUV2.rootmap -inlineInputHeader -DG4_STORE_TRAJECTORY -DG4VERBOSE -DG4UI_USE -DG4VIS_USE -DG4MULTITHREADED -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/Cedar -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/CHANTI -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/CHOD -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/NewCHOD -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/GigaTracker -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/HAC -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/IRC -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/LAV -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/LKr -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV0 -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV1 -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV2 -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/MUV3 -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/RICH -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/SAC -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers/Spectrometer -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency-headers -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/include/Geant4 -I/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtCore -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtGui -I/cvmfs/sft.cern.ch/lcg/releases/qt/4.8.7-0b84e/x86_64-slc6-gcc49-opt/include/QtOpenGL -I/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Service/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/Cedar/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/CHANTI/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/CHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/NewCHOD/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/GigaTracker/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/HAC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/IRC/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/LAV/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/LKr/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV0/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV1/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV2/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/include -I/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/RecoBase/include /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV2/include/MUV2HitsCluster.hh /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV2/MUV2LinkDef.hh

MUV2/libMUV2_rdict.pcm: MUV2/MUV2DICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate MUV2/libMUV2_rdict.pcm

MUV2/libMUV2.rootmap: MUV2/MUV2DICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate MUV2/libMUV2.rootmap

MUV2DICT: MUV2/CMakeFiles/MUV2DICT
MUV2DICT: MUV2/MUV2DICT.cxx
MUV2DICT: MUV2/libMUV2_rdict.pcm
MUV2DICT: MUV2/libMUV2.rootmap
MUV2DICT: MUV2/CMakeFiles/MUV2DICT.dir/build.make

.PHONY : MUV2DICT

# Rule to build all files generated by this target.
MUV2/CMakeFiles/MUV2DICT.dir/build: MUV2DICT

.PHONY : MUV2/CMakeFiles/MUV2DICT.dir/build

MUV2/CMakeFiles/MUV2DICT.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV2 && $(CMAKE_COMMAND) -P CMakeFiles/MUV2DICT.dir/cmake_clean.cmake
.PHONY : MUV2/CMakeFiles/MUV2DICT.dir/clean

MUV2/CMakeFiles/MUV2DICT.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/MUV2 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV2 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Reconstruction/build-slc6/MUV2/CMakeFiles/MUV2DICT.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : MUV2/CMakeFiles/MUV2DICT.dir/depend


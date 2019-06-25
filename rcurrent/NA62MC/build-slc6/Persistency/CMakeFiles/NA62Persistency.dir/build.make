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
include Persistency/CMakeFiles/NA62Persistency.dir/depend.make

# Include the progress variables for this target.
include Persistency/CMakeFiles/NA62Persistency.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/CMakeFiles/NA62Persistency.dir/flags.make

# Object files for target NA62Persistency
NA62Persistency_OBJECTS =

# External object files for target NA62Persistency
NA62Persistency_EXTERNAL_OBJECTS = \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/AnalysisInfo.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/AnalyzerIdentifier.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/BeamData.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/BeamSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/DetectorParameter.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/Event.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/EventBoundary.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/EventHeader.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/FADCEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/FADCVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/GenePart.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/HLTEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/KinePart.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/L0TPData.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/L0TPSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/L1TPData.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/L1TPSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/L2EBData.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/L2EBSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/MCInfo.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/NA62Global.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/RecoInfo.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/Rndm.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/Stream.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/StringInterpreter.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDCError.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDCEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDCVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDetectorVEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDetectorVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDigiVCandidate.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDigiVError.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TDigiVEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TEventInfo.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TPrimSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TPrimitive.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TRecoVEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TRecoVCandidate.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TRecoVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TSpecialTriggerEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TTDCBSpecialTrigger.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TTimeCluster.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TVCandidate.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TVChannelID.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TVDigi.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TVEvent.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/TVHit.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/BlueTubeMagneticFieldMap.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/FringeMagneticFieldMap.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/src/MNP33MagneticFieldMap.cc.o" \
"/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/PersObjLib.dir/NA62PersistencyDICT.cxx.o"

Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/AnalysisInfo.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/AnalyzerIdentifier.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/BeamData.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/BeamSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/DetectorParameter.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/Event.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/EventBoundary.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/EventHeader.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/FADCEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/FADCVHit.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/GenePart.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/HLTEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/KinePart.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/L0TPData.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/L0TPSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/L1TPData.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/L1TPSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/L2EBData.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/L2EBSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/MCInfo.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/NA62Global.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/RecoInfo.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/Rndm.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/Stream.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/StringInterpreter.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDCError.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDCEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDCVHit.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDetectorVEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDetectorVHit.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDigiVCandidate.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDigiVError.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TDigiVEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TEventInfo.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TPrimSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TPrimitive.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TRecoVEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TRecoVCandidate.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TRecoVHit.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TSpecialTriggerEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TTDCBSpecialTrigger.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TTimeCluster.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TVCandidate.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TVChannelID.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TVDigi.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TVEvent.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/TVHit.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/BlueTubeMagneticFieldMap.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/FringeMagneticFieldMap.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/src/MNP33MagneticFieldMap.cc.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/PersObjLib.dir/NA62PersistencyDICT.cxx.o
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/NA62Persistency.dir/build.make
Persistency/libNA62Persistency.so: Persistency/CMakeFiles/NA62Persistency.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Linking CXX shared library libNA62Persistency.so"
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/NA62Persistency.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
Persistency/CMakeFiles/NA62Persistency.dir/build: Persistency/libNA62Persistency.so

.PHONY : Persistency/CMakeFiles/NA62Persistency.dir/build

Persistency/CMakeFiles/NA62Persistency.dir/requires:

.PHONY : Persistency/CMakeFiles/NA62Persistency.dir/requires

Persistency/CMakeFiles/NA62Persistency.dir/clean:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency && $(CMAKE_COMMAND) -P CMakeFiles/NA62Persistency.dir/cmake_clean.cmake
.PHONY : Persistency/CMakeFiles/NA62Persistency.dir/clean

Persistency/CMakeFiles/NA62Persistency.dir/depend:
	cd /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6 /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/CMakeFiles/NA62Persistency.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/CMakeFiles/NA62Persistency.dir/depend


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
include Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/depend.make

# Include the progress variables for this target.
include Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/progress.make

# Include the compile flags for this target's objects.
include Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make

Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/SACPersistencyLinkDef.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/SACChannelID.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TRecoSACCandidate.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TRecoSACEvent.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TRecoSACHit.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TSACDigi.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TSACEvent.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TSACHit.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/SACChannelID.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TRecoSACCandidate.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TRecoSACEvent.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TRecoSACHit.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TSACDigi.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TSACEvent.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/include/TSACHit.hh
Persistency/FullReco/SAC/SACPersistencyDICT.cxx: ../Persistency/FullReco/SAC/SACPersistencyLinkDef.hh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating SACPersistencyDICT.cxx, libSACPersistency_rdict.pcm, libSACPersistency.rootmap"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/CMake/3.11.1-daf3a/x86_64-centos7-gcc7-opt/bin/cmake -E env LD_LIBRARY_PATH=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib:/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/lib-cc7:/afs/cern.ch/user/d/dacross/na62fw/NA62MC/lib-cc7/:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/fftw3/3.3.4/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/GSL/2.5/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/views/LCG_95/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/views/LCG_95/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/xrootd/4.8.4/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/tbb/2019_U1/x86_64-centos7-gcc7-opt/lib:/lib:/lib:/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/:/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/Persistency:/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/SlimPersistency:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/lib:/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/lib64:/cvmfs/sft.cern.ch/lcg/releases/binutils/2.28-a983d/x86_64-centos7/lib:/cvmfs/sft.cern.ch/lcg/releases/clhep/2.4.1.0-2c56f/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-ced0e/x86_64-centos7-gcc7-opt/lib /cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/bin/rootcling -v2 -f SACPersistencyDICT.cxx -s /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/libSACPersistency.so -rml libSACPersistency.so -rmf /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/libSACPersistency.rootmap -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/NA62/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include -inlineInputHeader /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/SACChannelID.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/TRecoSACCandidate.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/TRecoSACEvent.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/TRecoSACHit.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/TSACDigi.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/TSACEvent.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/include/TSACHit.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/SACPersistencyLinkDef.hh

Persistency/FullReco/SAC/libSACPersistency_rdict.pcm: Persistency/FullReco/SAC/SACPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate Persistency/FullReco/SAC/libSACPersistency_rdict.pcm

Persistency/FullReco/SAC/libSACPersistency.rootmap: Persistency/FullReco/SAC/SACPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate Persistency/FullReco/SAC/libSACPersistency.rootmap

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.o: ../Persistency/FullReco/SAC/src/SACChannelID.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/SACChannelID.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/SACChannelID.cc > CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/SACChannelID.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.o: ../Persistency/FullReco/SAC/src/TRecoSACCandidate.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACCandidate.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACCandidate.cc > CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACCandidate.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.o: ../Persistency/FullReco/SAC/src/TRecoSACEvent.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACEvent.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACEvent.cc > CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACEvent.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.o: ../Persistency/FullReco/SAC/src/TRecoSACHit.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACHit.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACHit.cc > CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TRecoSACHit.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.o: ../Persistency/FullReco/SAC/src/TSACDigi.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACDigi.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACDigi.cc > CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACDigi.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.o: ../Persistency/FullReco/SAC/src/TSACEvent.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACEvent.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACEvent.cc > CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACEvent.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.o: ../Persistency/FullReco/SAC/src/TSACHit.cc
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACHit.cc

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACHit.cc > CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC/src/TSACHit.cc -o CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.s

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.o: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/flags.make
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.o: Persistency/FullReco/SAC/SACPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building CXX object Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.o"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.o -c /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/SACPersistencyDICT.cxx

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.i"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/SACPersistencyDICT.cxx > CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.i

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.s"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && /cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/bin/g++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/SACPersistencyDICT.cxx -o CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.s

SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/SACChannelID.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACCandidate.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACEvent.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TRecoSACHit.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACDigi.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACEvent.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/src/TSACHit.cc.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/SACPersistencyDICT.cxx.o
SACObjPersistencyLib: Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/build.make

.PHONY : SACObjPersistencyLib

# Rule to build all files generated by this target.
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/build: SACObjPersistencyLib

.PHONY : Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/build

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC && $(CMAKE_COMMAND) -P CMakeFiles/SACObjPersistencyLib.dir/cmake_clean.cmake
.PHONY : Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/clean

Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/depend: Persistency/FullReco/SAC/SACPersistencyDICT.cxx
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/depend: Persistency/FullReco/SAC/libSACPersistency_rdict.pcm
Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/depend: Persistency/FullReco/SAC/libSACPersistency.rootmap
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Tools /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco/SAC /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/FullReco/SAC/CMakeFiles/SACObjPersistencyLib.dir/depend


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

# Utility rule file for LAVSlimPersistencyDICT.

# Include the progress variables for this target.
include Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/progress.make

Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx
Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/libLAVSlimPersistency_rdict.pcm
Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/libLAVSlimPersistency.rootmap


Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/LAVSlimPersistencyLinkDef.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/include/TSlimRecoLAVCandidate.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/include/TSlimRecoLAVEvent.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/include/TSlimRecoLAVHit.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/include/TSlimRecoLAVCandidate.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/include/TSlimRecoLAVEvent.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/include/TSlimRecoLAVHit.hh
Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx: ../Persistency/SlimReco/LAV/LAVSlimPersistencyLinkDef.hh
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Generating LAVSlimPersistencyDICT.cxx, libLAVSlimPersistency_rdict.pcm, libLAVSlimPersistency.rootmap"
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/LAV && /cvmfs/sft.cern.ch/lcg/releases/CMake/3.11.1-daf3a/x86_64-centos7-gcc7-opt/bin/cmake -E env LD_LIBRARY_PATH=/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib:/afs/cern.ch/user/d/dacross/na62fw/NA62Reconstruction/lib-cc7:/afs/cern.ch/user/d/dacross/na62fw/NA62MC/lib-cc7/:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/fftw3/3.3.4/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/GSL/2.5/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/views/LCG_95/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/views/LCG_95/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/xrootd/4.8.4/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/tbb/2019_U1/x86_64-centos7-gcc7-opt/lib:/lib:/lib:/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/:/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/Persistency:/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/lib-cc7/SlimPersistency:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/lib:/cvmfs/sft.cern.ch/lcg/releases/gcc/7.3.0-cb1ee/x86_64-centos7/lib64:/cvmfs/sft.cern.ch/lcg/releases/binutils/2.28-a983d/x86_64-centos7/lib:/cvmfs/sft.cern.ch/lcg/releases/clhep/2.4.1.0-2c56f/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-ced0e/x86_64-centos7-gcc7-opt/lib /cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/bin/rootcling -v2 -f LAVSlimPersistencyDICT.cxx -s /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/LAV/libLAVSlimPersistency.so -rml libLAVSlimPersistency.so -rmf /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/LAV/libLAVSlimPersistency.rootmap -I/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/../FullReco/NA62/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/NA62/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/include -I/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/../../FullReco/LAV/include -inlineInputHeader /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/include/TSlimRecoLAVCandidate.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/include/TSlimRecoLAVEvent.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/include/TSlimRecoLAVHit.hh /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV/LAVSlimPersistencyLinkDef.hh

Persistency/SlimReco/LAV/libLAVSlimPersistency_rdict.pcm: Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate Persistency/SlimReco/LAV/libLAVSlimPersistency_rdict.pcm

Persistency/SlimReco/LAV/libLAVSlimPersistency.rootmap: Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx
	@$(CMAKE_COMMAND) -E touch_nocreate Persistency/SlimReco/LAV/libLAVSlimPersistency.rootmap

LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT
LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/LAVSlimPersistencyDICT.cxx
LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/libLAVSlimPersistency_rdict.pcm
LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/libLAVSlimPersistency.rootmap
LAVSlimPersistencyDICT: Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/build.make

.PHONY : LAVSlimPersistencyDICT

# Rule to build all files generated by this target.
Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/build: LAVSlimPersistencyDICT

.PHONY : Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/build

Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/clean:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/LAV && $(CMAKE_COMMAND) -P CMakeFiles/LAVSlimPersistencyDICT.dir/cmake_clean.cmake
.PHONY : Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/clean

Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/depend:
	cd /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /afs/cern.ch/user/d/dacross/na62fw/NA62Tools /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/SlimReco/LAV /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7 /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/LAV /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : Persistency/SlimReco/LAV/CMakeFiles/LAVSlimPersistencyDICT.dir/depend


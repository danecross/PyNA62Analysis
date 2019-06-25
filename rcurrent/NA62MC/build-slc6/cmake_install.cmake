# Install script for directory: /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE FILE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LoopTools214/src/LoopTools214/build/libooptools.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin-slc6" TYPE EXECUTABLE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NA62MC")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC"
         OLD_RPATH "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/lib-slc6:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/lib64:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/lib:/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Geant4/10.01.p02/x86_64-slc6-gcc49-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-f6432/x86_64-slc6-gcc49-opt/lib:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-slc6/NA62MC")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/config" TYPE FILE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NA62MC-slc6Config.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Cedar/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CHANTI/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/CHOD/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/NewCHOD/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/GigaTracker/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/IRC/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LAV/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV0/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV1/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV2/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV3/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/RICH/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/SAC/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Spectrometer/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Generator/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MCBase/cmake_install.cmake")
  include("/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Beam/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")

# Install script for directory: /afs/cern.ch/user/d/dacross/na62fw/NA62Tools/Persistency/FullReco

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/afs/cern.ch/user/d/dacross/na62fw/NA62Tools")
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

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/NA62/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/Cedar/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/CHANTI/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/CHOD/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/GigaTracker/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/HAC/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/IRC/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/LAV/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/LKr/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV0/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV1/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV2/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/MUV3/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/NewCHOD/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/RICH/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAC/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/SAV/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/na62fw/NA62Tools/build-cc7/Persistency/FullReco/Spectrometer/cmake_install.cmake")

endif()


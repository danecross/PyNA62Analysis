# Install script for directory: /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/Persistency/SlimReco/MUV3

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools")
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xslimlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency" TYPE FILE FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/libMUV3SlimPersistency_rdict.pcm")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xslimlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency" TYPE FILE FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/libMUV3SlimPersistency.rootmap")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xslimlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/libMUV3SlimPersistency.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/cvmfs/sft.cern.ch/lcg/releases/binutils/2.28-a983d/x86_64-centos7/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency/libMUV3SlimPersistency.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xslimlibx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-cc7/SlimPersistency" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62Tools/build-cc7/Persistency/SlimReco/MUV3/libMUV3SlimPersistency-static.a")
endif()

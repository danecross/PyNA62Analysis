# Install script for directory: /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3

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
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libMUV3.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libMUV3.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libMUV3.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV3/libMUV3.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libMUV3.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libMUV3.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libMUV3.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV3/libMUV3-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE FILE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV3/libMUV3Persistency_rdict.pcm")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libMUV3Persistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libMUV3Persistency.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libMUV3Persistency.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV3/libMUV3Persistency.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libMUV3Persistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libMUV3Persistency.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libMUV3Persistency.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/MUV3/libMUV3Persistency-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/Persistency-headers/MUV3" TYPE FILE FILES
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/MUV3ChannelID.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/TMUV3Digi.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/TMUV3Event.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/TMUV3Hit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/TRecoMUV3Candidate.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/TRecoMUV3Event.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/MUV3/Persistency/include/TRecoMUV3Hit.hh"
    )
endif()


# Install script for directory: /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC

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
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libHAC.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libHAC.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libHAC.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/libHAC.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libHAC.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libHAC.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libHAC.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/libHAC-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE FILE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/libHACPersistency_rdict.pcm")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libHACPersistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libHACPersistency.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libHACPersistency.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/libHACPersistency.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libHACPersistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libHACPersistency.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libHACPersistency.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/HAC/libHACPersistency-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/Persistency-headers/HAC" TYPE FILE FILES
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/HACChannelID.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/THACDigi.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/THACEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/THACHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/TRecoHACCandidate.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/TRecoHACEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/HAC/Persistency/include/TRecoHACHit.hh"
    )
endif()


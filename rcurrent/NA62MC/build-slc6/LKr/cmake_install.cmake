# Install script for directory: /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr

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
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/libLKr.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so"
         OLD_RPATH "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62Tools/lib-slc6:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/ROOT/6.08.00/x86_64-slc6-gcc49-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/lib64:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/gcc/4.9.3/x86_64-slc6/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/Boost/1.62.0/x86_64-slc6-gcc49-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/sqlite/3110100/x86_64-slc6-gcc49-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_86/xrootd/4.4.1/x86_64-slc6-gcc49-opt/lib:/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/libLKr.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/libLKr-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE FILE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/libLKrPersistency_rdict.pcm")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libLKrPersistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libLKrPersistency.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libLKrPersistency.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/libLKrPersistency.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libLKrPersistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libLKrPersistency.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libLKrPersistency.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/LKr/libLKrPersistency-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/Persistency-headers/LKr" TYPE FILE FILES
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/LKrChannelID.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TDigiLKrEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrDigi.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TLKrMicroCellHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TRecoLKrCandidate.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TRecoLKrEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/LKr/Persistency/include/TRecoLKrHit.hh"
    )
endif()


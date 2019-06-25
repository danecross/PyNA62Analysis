# Install script for directory: /afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency

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

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libNA62Persistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libNA62Persistency.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libNA62Persistency.so"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE SHARED_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/libNA62Persistency.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libNA62Persistency.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libNA62Persistency.so")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency/libNA62Persistency.so")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE STATIC_LIBRARY FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/libNA62Persistency-static.a")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-slc6/Persistency" TYPE FILE FILES "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/build-slc6/Persistency/libNA62Persistency_rdict.pcm")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "persistencylib")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/Persistency-headers" TYPE FILE FILES
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/AnalysisInfo.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/AnalyzerIdentifier.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/BeamData.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/BeamSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/DetectorParameter.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/Event.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/EventBoundary.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/EventHeader.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/FADCEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/FADCVHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/GenePart.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/HLTEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/KinePart.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/L0TPData.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/L0TPSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/L1TPData.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/L1TPSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/L2EBData.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/L2EBSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/MCInfo.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/RecoInfo.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/Rndm.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/Stream.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/StringInterpreter.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDCError.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDCEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDCVHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDetectorVEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDetectorVHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDigiVCandidate.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDigiVError.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TDigiVEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TEventInfo.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TPrimSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TPrimitive.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TRecoVCandidate.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TRecoVEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TRecoVHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TSpecialTriggerEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TTDCBSpecialTrigger.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TTimeCluster.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TVCandidate.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TVChannelID.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TVDigi.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TVEvent.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/TVHit.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/BlueTubeMagneticFieldMap.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/FringeMagneticFieldMap.hh"
    "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/Persistency/include/MNP33MagneticFieldMap.hh"
    )
endif()


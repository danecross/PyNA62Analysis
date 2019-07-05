# Install script for directory: /afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC")
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

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib-cc7" TYPE FILE FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/LoopTools214/src/LoopTools214/build/libooptools.a")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC"
         RPATH "")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin-cc7" TYPE EXECUTABLE FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/NA62MC")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC"
         OLD_RPATH "/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/lib-cc7:/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/lib-cc7/Persistency:/afs/cern.ch/na62/offline/NA62FW/dev/r2779/NA62Tools/lib-cc7/SlimPersistency:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/ROOT/6.16.00/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Boost/1.69.0/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/sqlite/3210000/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/xrootd/4.8.4/x86_64-centos7-gcc7-opt/lib:/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC:/cvmfs/sft.cern.ch/lcg/releases/LCG_95/Geant4/10.05/x86_64-centos7-gcc7-opt/lib64:/cvmfs/sft.cern.ch/lcg/releases/XercesC/3.1.3-ced0e/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/ROOT/6.16.00-42022/x86_64-centos7-gcc7-opt/lib:/cvmfs/sft.cern.ch/lcg/releases/VecGeom/v1.1.0-22e48/x86_64-centos7-gcc7-opt/lib/cmake/VecGeom/../..:/cvmfs/sft.cern.ch/lcg/releases/clhep/2.4.1.0-2c56f/x86_64-centos7-gcc7-opt/lib:"
         NEW_RPATH "")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/cvmfs/sft.cern.ch/lcg/releases/binutils/2.28-a983d/x86_64-centos7/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin-cc7/NA62MC")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/config" TYPE FILE FILES "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/NA62MC-cc7Config.cmake")
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Cedar/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CHANTI/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/CHOD/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/NewCHOD/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/GigaTracker/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/HAC/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/IRC/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/LAV/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/LKr/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/MUV0/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/MUV1/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/MUV2/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/MUV3/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/RICH/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/SAC/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Spectrometer/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Generator/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/MCBase/cmake_install.cmake")
  include("/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/Beam/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/afs/cern.ch/user/d/dacross/SummerProject/rcurrent/NA62MC/build-cc7/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")

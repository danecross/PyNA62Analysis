cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project (NA62MC C CXX Fortran)

#Get and configure NA62Tools
find_package(NA62Tools-$ENV{SYSTEMINSTALL} REQUIRED)
include_directories(${NA62TOOLS_INCLUDE_DIRS})
include_directories(${NA62TOOLS_PERS_INCLUDE_DIRS})
link_directories(${NA62TOOLS_LIBRARY_DIRS})

include(${NA62TOOLS_CONFIG_DIR}/common.cmake)

if(SHARED_LIB)
    set( LIBTYPE SHARED )
else()
    set( LIBTYPE STATIC )
endif()

if(${LIBTYPE} STREQUAL STATIC)
    message("-- Using static libraries")
    set( LIBTYPEPOSTFIX "-static")
    set( NA62TOOLS_LIBS_USED ${NA62TOOLS_LIBRARIES_STATIC})
    set( NA62TOOLS_LIBS_PERS_USED ${NA62TOOLS_PERS_LIBRARIES_STATIC})
    set( NA62TOOLS_LIBS_SLIM_USED ${NA62TOOLS_SLIM_LIBRARIES_STATIC})
else()
    message("-- Using shared libraries")
    set( LIBTYPEPOSTFIX "")
    set( NA62TOOLS_LIBS_USED ${NA62TOOLS_LIBRARIES})
    set( NA62TOOLS_LIBS_PERS_USED ${NA62TOOLS_PERS_LIBRARIES})
    set( NA62TOOLS_LIBS_SLIM_USED ${NA62TOOLS_SLIM_LIBRARIES})
endif()

#Set build type to release as default
set(ACCEPTED_BUILD_TYPES DEBUG RELEASE RELWITHDEBINFO MINSIZEREL)
if(NOT CMAKE_BUILD_TYPE)
    message(STATUS "Setting build type to 'Release' as none was specified.")
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build." FORCE)
endif()
string(TOUPPER ${CMAKE_BUILD_TYPE} UPPER_BUILD_TYPE)
list(FIND ACCEPTED_BUILD_TYPES ${UPPER_BUILD_TYPE} IS_BUILD_TYPE_ACCEPTED)
if(${IS_BUILD_TYPE_ACCEPTED} EQUAL -1)
    message(FATAL_ERROR "CMAKE_BUILD_TYPE of type ${CMAKE_BUILD_TYPE} is not accepted. Available values are Debug, Release, RelWithDebInfo, MinSizeRel")
else()
    message(STATUS "Compilation type: " ${CMAKE_BUILD_TYPE})
endif()

#Get and configure ROOT
list(APPEND CMAKE_PREFIX_PATH $ENV{ROOTSYS})
find_package(ROOT REQUIRED)
if( ${ROOT_FOUND} )
    include(${ROOT_USE_FILE})
    message (STATUS "Found ROOT ${ROOT_VERSION} at ${ROOT_DIR}")
else()
    message (FATAL_ERROR "ROOT not found")
endif()
include_directories(${ROOT_INCLUDE_DIR})
link_directories(${ROOT_LIBRARY_DIR})

#Get and configure GEANT4
option(WITH_GEANT4_UIVIS "Build example with Geant4 UI and Vis drivers" OFF)
add_definitions(-DG4LIB_USE_GDML=1)
if(WITH_GEANT4_UIVIS)
    message (STATUS "Geant4: graphics enabled")
    find_package(Geant4 REQUIRED ui_all vis_all)
else()
    message (STATUS "Geant4: graphics disabled")
    set(G4UI_NONE 1)
    set(G4VIS_NONE 1)
    find_package(Geant4 REQUIRED)
    list(REMOVE_ITEM Geant4_LIBRARIES G4interfaces)
endif()
if( ${Geant4_FOUND} )
    include(${Geant4_USE_FILE})
    message (STATUS "Found Geant4 ${Geant4_VERSION} at ${Geant4_DIR}")
else()
    message (FATAL_ERROR "ROOT not found")
endif()

if($ENV{SYSTEMINSTALL} STREQUAL "slc6")
    # define for old Geant4 (10.01)
    add_definitions(-DG4SLC6=1)
endif()

# select boost libraries to be used
set(BOOST_SUFFIX $ENV{BOOSTCOMP}$ENV{BOOSTVER})
set(BOOST_LIB boost_thread${BOOST_SUFFIX} boost_wave${BOOST_SUFFIX} boost_serialization${BOOST_SUFFIX} boost_filesystem${BOOST_SUFFIX} boost_system${BOOST_SUFFIX})

# Use correct gcc libraries
link_directories($ENV{NA62FW_STDLIBSPATH}/lib64)
link_directories($ENV{NA62FW_STDLIBSPATH}/lib)

#Configure other external libraries
include_directories($ENV{BOOST}/include/boost$ENV{BOOSTVER}) #old versions
include_directories($ENV{BOOST}/include)
include_directories($ENV{SQLITE}/include)
include_directories($ENV{XROOTD}/include)
#include_directories($ENV{LOOPTOOLS}/include)
link_directories($ENV{BOOST}/lib)
link_directories($ENV{SQLITE}/lib)
link_directories($ENV{XROOTD}/lib)
#link_directories($ENV{LOOPTOOLS}/lib64)
link_directories(${CMAKE_SOURCE_DIR})

#Define install directories
if("${CMAKE_INSTALL_PREFIX}" STREQUAL "/usr/local")
    #Overwrite the default
    set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})
endif()

if(CMAKE_COMPILER_IS_GNUCXX)
    #Get GCC/G++ version to use correct c++11 flag
    GETGCCVERSION(maj min)
    if( "${maj}.${min}" VERSION_LESS "4.7" )
        SET(C++11_FLAG "-std=c++0x")
    elseif( "${maj}.${min}" VERSION_LESS "4.9" )
        SET(C++11_FLAG "-std=c++1y")
    else()
        SET(C++11_FLAG "-std=c++14")
    endif()
    set(WARNING_FLAGS "-W -Wall -ansi -pedantic -Wno-non-virtual-dtor -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-shadow -Wno-vla")
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${WARNING_FLAGS} -pipe ${C++11_FLAG} ${ROOT_CXX_FLAGS}")
    set(CMAKE_C_FLAGS "${WARNING_FLAGS} -pipe")
endif()

#Get the NA62MC sources
file(GLOB sources src/*.cc)
file(GLOB headers include/*.hh)

#Create executable
add_executable(NA62MC NA62MC.cc ${sources})

#List all subdetectors
SET(subdetlist Cedar CHANTI CHOD NewCHOD GigaTracker HAC IRC LAV LKr MUV0 MUV1 MUV2 MUV3 RICH SAC Spectrometer)

foreach(detector ${subdetlist})
    #For each subdetector, add the subdirectory, specify the include and persistency include dirs
    #and link the libraries
    add_subdirectory(${detector})
    include_directories(${detector}/include)
    target_link_libraries(NA62MC MC${detector}${LIBTYPEPOSTFIX})
endforeach()

#Specify other NA62MC include directories
include_directories(include)
include_directories(MCBase/include)
include_directories(Beam/include)
include_directories(Generator)

#Add subdirectories to the build
include(ExternalProject)
include(ExternalLibs/looptools.cmake)
add_subdirectory(Generator)
add_subdirectory(MCBase)
add_subdirectory(Beam)
add_dependencies(CMC LoopTools214)
add_dependencies(CMC-static LoopTools214)

#Link with NA62MC libraries
target_link_libraries(NA62MC MCBase${LIBTYPEPOSTFIX})
target_link_libraries(NA62MC NA62Persistency${LIBTYPEPOSTFIX})
target_link_libraries(NA62MC Beam${LIBTYPEPOSTFIX})
target_link_libraries(NA62MC CMC${LIBTYPEPOSTFIX})

#Link with external libraries
target_link_libraries(NA62MC ${NA62TOOLS_LIBS_USED})
target_link_libraries(NA62MC ${NA62TOOLS_LIBS_SLIM_USED})
target_link_libraries(NA62MC ${NA62TOOLS_LIBS_PERS_USED})
target_link_libraries(NA62MC ${Geant4_LIBRARIES} G4global G3toG4)
target_link_libraries(NA62MC ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve)
target_link_libraries(NA62MC ${BOOST_LIB} lzma sqlite3 ooptools gfortran)

#Install executable
install(TARGETS NA62MC DESTINATION bin-$ENV{SYSTEMINSTALL})

#Export package for external use
export(PACKAGE NA62MC)
set(CONF_CONFIG_DIR "${CMAKE_INSTALL_PREFIX}/config")
configure_file(NA62MCConfig.cmake.in
  "${PROJECT_BINARY_DIR}/NA62MC-$ENV{SYSTEMINSTALL}Config.cmake" @ONLY)
install(FILES ${PROJECT_BINARY_DIR}/NA62MC-$ENV{SYSTEMINSTALL}Config.cmake DESTINATION config)

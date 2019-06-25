cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project (NA62Reconstruction C CXX Fortran)

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
else()
    message("-- Using shared libraries")
    set( LIBTYPEPOSTFIX "")
    set( NA62TOOLS_LIBS_USED ${NA62TOOLS_LIBRARIES})
    set( NA62TOOLS_LIBS_PERS_USED ${NA62TOOLS_PERS_LIBRARIES})
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
    message(FATAL_ERROR "CMAKE_BUILD_TYPE of type ${CMAKE_BUILD_TYPE} is not accepted. Available values are Debug,Release,RelWithDebInfo,MinSizeRel")
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

#Get and configure GEANT4 (needed just for the G4SystemOfUnits.hh)
find_package(Geant4 REQUIRED)
include(${Geant4_USE_FILE})

#Get and configure DIM
if(DEFINED ENV{DIMDEFINED})
    if($ENV{DIMDEFINED} EQUAL 1)
        add_definitions(-DDIM)
        if(DEFINED ENV{DIMDIR})
            link_directories($ENV{DIMDIR}/linux)
            include_directories($ENV{DIMDIR}/dim)
        endif()
        message (STATUS "Using DIM")
    endif()
endif()

#Get and configure sqlite3
include_directories($ENV{SQLITE}/include)
link_directories($ENV{SQLITE}/lib)

#XrdPosix lib for access to raw files via xrootd
link_directories($ENV{XROOTD}/lib64)

# Use correct gcc libraries
link_directories($ENV{NA62FW_STDLIBSPATH}/lib64)
link_directories($ENV{NA62FW_STDLIBSPATH}/lib)

link_directories($ENV{BOOST}/lib)

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
#   set(WARNING_FLAGS "-W -Wall -ansi -pedantic -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-shadow")
    set(WARNING_FLAGS "-W -Wall -ansi -pedantic -Wno-non-virtual-dtor -Wno-long-long -Wwrite-strings -Wpointer-arith -Woverloaded-virtual -Wno-shadow -Wno-vla")
    set(CMAKE_POSITION_INDEPENDENT_CODE ON)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${WARNING_FLAGS} -pipe ${C++11_FLAG} ${ROOT_CXX_FLAGS}")
    set(CMAKE_C_FLAGS "${WARNING_FLAGS} -pipe")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${WARNING_FLAGS}")
endif()

#Get the NA62Reco sources
file(GLOB sources src/*.cc)
file(GLOB headers include/*.hh)

#Create executable
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(MainObjLib OBJECT ${sources})
set_property(TARGET ${MainObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries
add_executable(NA62Reco NA62Reco.cc $<TARGET_OBJECTS:MainObjLib>)
add_executable(NA62EventDisplay NA62EventDisplay.cc $<TARGET_OBJECTS:MainObjLib>)
add_executable(NA62OnlineMonitor NA62OnlineMonitor.cc $<TARGET_OBJECTS:MainObjLib>)

add_custom_target(tools ${CMAKE_COMMAND} -DCMAKE_INSTALL_COMPONENT=toolsexe -P ${CMAKE_BINARY_DIR}/cmake_install.cmake)

include_directories(Service/include)

#List all subdetectors
SET(subdetlist Cedar CHANTI CHOD NewCHOD GigaTracker HAC IRC LAV LKr MUV0 MUV1 MUV2 MUV3 RICH SAC SAV Spectrometer)

#Add subdirectories to the build
add_subdirectory(RecoBase)
add_subdirectory(Service)
add_subdirectory(EventDisplay)
add_subdirectory(Tools)

set(CONF_INCLUDE_DIRS "")
set(CONF_SUBDET "")
set(CONF_SUBDET_STATIC "")

foreach(detector ${subdetlist})
    #For each subdetector, add the subdirectory, specify the include dir and link the libraries
    add_subdirectory(${detector})
    include_directories(${detector}/include)
    target_link_libraries(NA62Reco ${detector}${LIBTYPEPOSTFIX})
    target_link_libraries(NA62EventDisplay ${detector}${LIBTYPEPOSTFIX})
    target_link_libraries(NA62OnlineMonitor ${detector}${LIBTYPEPOSTFIX})
    set(CONF_INCLUDE_DIRS "${CONF_INCLUDE_DIRS}" "${CMAKE_INSTALL_PREFIX}/${detector}/include")
    set(CONF_SUBDET "${CONF_SUBDET}" "${detector}")
    set(CONF_SUBDET_STATIC "${CONF_SUBDET_STATIC}" "${detector}-static")
endforeach()

#Configure HLT libraries
set(BOOST_SUFFIX $ENV{BOOSTCOMP}$ENV{BOOSTVER})
set(BOOST_LIB boost_thread${BOOST_SUFFIX} boost_wave${BOOST_SUFFIX} boost_serialization${BOOST_SUFFIX} boost_filesystem${BOOST_SUFFIX} boost_system${BOOST_SUFFIX} boost_timer${BOOST_SUFFIX})
set(TBB_LIBDIR $ENV{TBB}/lib)
set(TBB_INCLUDE $ENV{TBB}/include)

if(DEFINED ENV{ONLINEHLTDEFINED})
    if($ENV{ONLINEHLTDEFINED} EQUAL 1)
        include_directories($ENV{BOOST}/include/boost$ENV{BOOSTVER}) #old versions
        include_directories($ENV{BOOST}/include)
        include_directories(${TBB_INCLUDE})
        include_directories("HLT/na62-farm-lib/")
        include_directories("HLT/na62-trigger-algorithms/")
        add_subdirectory(HLT)
        add_definitions(-DONLINEHLT)
        target_link_libraries(NA62Reco offline-na62-farm-lib)
        target_link_libraries(NA62Reco offline-na62-trigger-algorithms)
        target_link_libraries(NA62Reco ${BOOST_LIB})
        target_link_libraries(NA62EventDisplay offline-na62-farm-lib)
        target_link_libraries(NA62EventDisplay offline-na62-trigger-algorithms)
        target_link_libraries(NA62EventDisplay ${BOOST_LIB})
        target_link_libraries(NA62OnlineMonitor offline-na62-farm-lib)
        target_link_libraries(NA62OnlineMonitor offline-na62-trigger-algorithms)
        target_link_libraries(NA62OnlineMonitor ${BOOST_LIB})
    endif()
endif()

set(CONF_SUBDET "${CONF_SUBDET}" "RecoBase")
set(CONF_SUBDET_STATIC "${CONF_SUBDET_STATIC}" "RecoBase-static")

#Specify other NA62Reco include directories
include_directories(include)
include_directories(RecoBase/include)
include_directories(EventDisplay/include)

#Link with NA62Reco libraries
target_link_libraries(NA62Reco RecoBase${LIBTYPEPOSTFIX} EventDisplay${LIBTYPEPOSTFIX} RecoService${LIBTYPEPOSTFIX})
target_link_libraries(NA62EventDisplay RecoBase${LIBTYPEPOSTFIX} EventDisplay${LIBTYPEPOSTFIX} RecoService${LIBTYPEPOSTFIX})
target_link_libraries(NA62OnlineMonitor RecoBase${LIBTYPEPOSTFIX} EventDisplay${LIBTYPEPOSTFIX} RecoService${LIBTYPEPOSTFIX})

#Link with external libraries
target_link_libraries(NA62Reco ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve RGL FTGL Gui Ged TreePlayer Spectrum)
target_link_libraries(NA62Reco sqlite3 XrdPosixPreload XrdPosix)
target_link_libraries(NA62Reco ${NA62TOOLS_LIBS_PERS_USED})
target_link_libraries(NA62Reco ${NA62TOOLS_LIBS_USED})
target_link_libraries(NA62EventDisplay ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve RGL FTGL Gui Ged TreePlayer Spectrum)
target_link_libraries(NA62EventDisplay sqlite3 XrdPosixPreload XrdPosix)
target_link_libraries(NA62EventDisplay ${NA62TOOLS_LIBS_PERS_USED})
target_link_libraries(NA62EventDisplay ${NA62TOOLS_LIBS_USED})
target_link_libraries(NA62OnlineMonitor ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve RGL FTGL Gui Ged TreePlayer Spectrum)
target_link_libraries(NA62OnlineMonitor sqlite3 XrdPosixPreload XrdPosix)
target_link_libraries(NA62OnlineMonitor ${NA62TOOLS_LIBS_PERS_USED})
target_link_libraries(NA62OnlineMonitor ${NA62TOOLS_LIBS_USED})

#Get and configure DIM
if(DEFINED ENV{DIMDEFINED})
    if($ENV{DIMDEFINED} EQUAL 1)
        target_link_libraries(NA62Reco dim)
        target_link_libraries(NA62EventDisplay dim)
        target_link_libraries(NA62OnlineMonitor dim)
    endif()
endif()

#Install executable
install(TARGETS NA62Reco DESTINATION bin-$ENV{SYSTEMINSTALL})
install(TARGETS NA62EventDisplay DESTINATION bin-$ENV{SYSTEMINSTALL})
install(TARGETS NA62OnlineMonitor DESTINATION bin-$ENV{SYSTEMINSTALL})

#Export package for external use
export(PACKAGE NA62Reconstruction)
set(CONF_INCLUDE_DIRS "${CONF_INCLUDE_DIRS}" "${CMAKE_INSTALL_PREFIX}/include" "${CMAKE_INSTALL_PREFIX}/RecoBase/include")
set(CONF_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib-$ENV{SYSTEMINSTALL}")
configure_file(NA62ReconstructionConfig.cmake.in
  "${PROJECT_BINARY_DIR}/NA62Reconstruction-$ENV{SYSTEMINSTALL}Config.cmake" @ONLY)
install(FILES ${PROJECT_BINARY_DIR}/NA62Reconstruction-$ENV{SYSTEMINSTALL}Config.cmake DESTINATION config)

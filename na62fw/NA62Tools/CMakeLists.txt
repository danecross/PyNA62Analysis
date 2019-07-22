cmake_minimum_required(VERSION 2.6 FATAL_ERROR)

project (NA62Tools C CXX)

####################
### Preparation work
###   Get external packages
###   Setup gcc
###   Setup git
####################
include(config/common.cmake)

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
    message (STATUS "Found ROOT ${ROOT_VERSION}")
else()
    message (FATAL_ERROR "ROOT not found")
endif()

# Use correct gcc libraries
link_directories($ENV{NA62FW_STDLIBSPATH}/lib64)
link_directories($ENV{NA62FW_STDLIBSPATH}/lib)

#Get revision number
include(${CMAKE_SOURCE_DIR}/config/get_revision.cmake)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config/GitRevision.cc.in
  ${CMAKE_CURRENT_SOURCE_DIR}/src/GitRevision.cc)

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


####################
### Start the real work
###   Compile Persistency
###   Compile NA62Tools
###   Package and install everything
####################

#Specify other NA62Tools include directories
include_directories(include)

# Persistency
add_subdirectory(Persistency)
include_directories(${NA62TOOLS_SLIM_INCLUDE_DIRS})
include_directories(${NA62TOOLS_PERS_INCLUDE_DIRS})

## Define and compile main NA62Tools library
#Get the NA62Tools sources
file(GLOB sources src/*.cc)
file(GLOB headers include/*.hh)

# Create library
add_library(NA62ToolsObj OBJECT ${sources})
set_property(TARGET NA62ToolsObj PROPERTY POSITION_INDEPENDENT_CODE 1)
add_library(NA62ToolsMain SHARED $<TARGET_OBJECTS:NA62ToolsObj>)
add_library(NA62ToolsMain-static STATIC $<TARGET_OBJECTS:NA62ToolsObj>)

# Install library
install(TARGETS NA62ToolsMain NA62ToolsMain-static DESTINATION lib-$ENV{SYSTEMINSTALL})

#Export package for external use
export(PACKAGE NA62Tools)
# NA62TOOLS variables
set(CONF_INCLUDE_DIRS "${CONF_INCLUDE_DIRS}" "${CMAKE_INSTALL_PREFIX}/include")
set(CONF_LIB_DIR "${CMAKE_INSTALL_PREFIX}/lib-$ENV{SYSTEMINSTALL}" "${CMAKE_INSTALL_PREFIX}/lib-$ENV{SYSTEMINSTALL}/Persistency" "${CMAKE_INSTALL_PREFIX}/lib-$ENV{SYSTEMINSTALL}/SlimPersistency")
set(CONF_CONFIG_DIR "${CMAKE_INSTALL_PREFIX}/config")
# RecoPersistency variables
set(CONF_PERS_INCLUDE_DIRS "${NA62TOOLS_PERS_INCLUDE_DIRS}")
set(CONF_SUBDET_PERS "${NA62TOOLS_PERS_LIBRARIES}")
set(CONF_SUBDET_PERS_STATIC "${NA62TOOLS_PERS_LIBRARIES_STATIC}")
# SlimPersistency variables
set(CONF_SLIM_INCLUDE_DIRS "${NA62TOOLS_SLIM_INCLUDE_DIRS}")
set(CONF_SUBDET_SLIM "${NA62TOOLS_SLIM_LIBRARIES}")
set(CONF_SUBDET_SLIM_STATIC "${NA62TOOLS_SLIM_LIBRARIES_STATIC}")
# Write and install configuration
configure_file(NA62ToolsConfig.cmake.in
  "${PROJECT_BINARY_DIR}/NA62Tools-$ENV{SYSTEMINSTALL}Config.cmake" @ONLY)
install(FILES ${PROJECT_BINARY_DIR}/NA62Tools-$ENV{SYSTEMINSTALL}Config.cmake DESTINATION config)


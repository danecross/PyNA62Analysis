set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../lib${INSTALLEXTENSION} )
set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../lib${INSTALLEXTENSION} )
set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/../${BINDIR})
set( CMAKE_INSTALL_LIBDIR ${CMAKE_BINARY_DIR}/../lib${INSTALLEXTENSION} )

if(SHARED_LIB)
    set( LIBTYPE SHARED )
else()
    set( LIBTYPE STATIC )
endif()

#require ROOT
list(APPEND CMAKE_PREFIX_PATH $ENV{ROOTSYS})
find_package(ROOT REQUIRED)
if( ${ROOT_FOUND} )
    include(${ROOT_USE_FILE})
    message (STATUS "Found ROOT ${ROOT_VERSION}")
    message (STATUS "   ${ROOT_DIR}")
else()
    message (FATAL_ERROR "ROOT not found")
endif()

if("${INSTALLEXTENSION}" STREQUAL "-cc7")
	list(APPEND CMAKE_PREFIX_PATH $ENV{QTDIR})
	find_package(Qt5 COMPONENTS Core)
	if( ${Qt5_FOUND} )
		message (STATUS "Found Qt5 ${Qt5_VERSION}")
		message (STATUS "   ${Qt5_DIR}")
		link_directories($ENV{QTDIR}/lib)
	else()
		message (WARNING "Qt5 not found")
	endif()
endif() 

#Get and configure GEANT4
if(NA62_NOGEANT4)
else()
    option(WITH_GEANT4_UIVIS "Build example with Geant4 UI and Vis drivers" ON)
    if(WITH_GEANT4_UIVIS)
        find_package(Geant4 REQUIRED ui_all vis_all)
    else()
        find_package(Geant4 REQUIRED)
    endif()

    if( ${Geant4_FOUND} )
        include(${Geant4_USE_FILE})
        message (STATUS "Found Geant4 ${Geant4_VERSION}")
	    message (STATUS "   ${Geant4_DIR}")
        message (STATUS "   With visualisation option: ${WITH_GEANT4_UIVIS}")
    else()
        message (FATAL_ERROR "Geant4 not found")
    endif()
endif()

# select boost libraries to be used
set(BOOST_SUFFIX $ENV{BOOSTCOMP}$ENV{BOOSTVER})
message(STATUS "Using BOOST at ${BOOST_SUFFIX}") 
set(BOOST_LIB boost_program_options${BOOST_SUFFIX})

# Use correct gcc libraries
message(STATUS "Using standard libraries: $ENV{NA62FW_STDLIBSPATH}/lib64")
message(STATUS "Using standard libraries: $ENV{NA62FW_STDLIBSPATH}/lib")
link_directories($ENV{NA62FW_STDLIBSPATH}/lib64)
link_directories($ENV{NA62FW_STDLIBSPATH}/lib)

message(STATUS "Adding LCG libraries: $ENV{LCGDIR}/lib64")
message(STATUS "Adding LCG libraries: $ENV{LCGDIR}/lib")
#link_directories($ENV{LCGDIR}/lib64)
#link_directories($ENV{LCGDIR}/lib)

#Configure other external libraries
include_directories($ENV{BOOST}/include/boost$ENV{BOOSTVER}) #old versions
include_directories($ENV{BOOST}/include)
link_directories($ENV{BOOST}/lib)
include_directories($ENV{SQLITE}/include)
link_directories($ENV{SQLITE}/lib)

#Get and configure NA62Tools
find_package(NA62Tools-$ENV{SYSTEMINSTALL} REQUIRED)

#Get NA62RECO
find_package(NA62Reconstruction-$ENV{SYSTEMINSTALL})
# It defines the following variables
# Paths
#  NA62RECO_INCLUDE_DIRS      - include directories for NA62Reconstruction
#  NA62RECO_LIBRARY_DIRS      - library directories for NA62Reconstruction
# Lists
#  NA62RECO_LIBRARIES         - reco libraries to link against
#  NA62RECO_LIBRARIES_STATIC  - reco static libraries to link against

if( ${NA62TOOLS_FOUND} )
	message(STATUS "Found NA62Tools: ${NA62TOOLS_CMAKE_DIR}")
else()
	message(FATAL_ERROR "Unable to find NA62Tools")
endif()

if( ${NA62RECO_FOUND} )
	message(STATUS "Found NA62Reco: ${NA62RECO_CMAKE_DIR}")
else()
	message(FATAL_ERROR "Unable to find NA62Reco")
endif()

if(${LIBTYPE} STREQUAL STATIC)
    message("-- Using static libraries")
    set( LIBTYPEPOSTFIX "-static")
    set( NA62TOOLS_LIBS_USED ${NA62TOOLS_LIBRARIES_STATIC})
    set( NA62TOOLS_LIBS_PERS_USED ${NA62TOOLS_PERS_LIBRARIES_STATIC})
    set( NA62TOOLS_LIBS_SLIM_USED ${NA62TOOLS_SLIM_LIBRARIES_STATIC})
    set( NA62RECO_LIBS_USED ${NA62RECO_LIBRARIES_STATIC})
else()
    message("-- Using shared libraries")
    set( LIBTYPEPOSTFIX "")
    set( NA62TOOLS_LIBS_USED ${NA62TOOLS_LIBRARIES})
    set( NA62TOOLS_LIBS_PERS_USED ${NA62TOOLS_PERS_LIBRARIES})
    set( NA62TOOLS_LIBS_SLIM_USED ${NA62TOOLS_SLIM_LIBRARIES})
    set( NA62RECO_LIBS_USED ${NA62RECO_LIBRARIES})
endif()

include(${NA62TOOLS_CONFIG_DIR}/common.cmake)

if(CMAKE_COMPILER_IS_GNUCXX)
    #Get GCC/G++ version to use correct c++ std flag
    GETGCCVERSION(maj min)
    if( "${maj}.${min}" VERSION_LESS "4.7" )
        SET(C++STD_FLAG "-std=c++0x")
    elseif( "${maj}.${min}" VERSION_LESS "4.9" )
        SET(C++STD_FLAG "-std=c++1y")
    else()
        SET(C++STD_FLAG "-std=c++14")
    endif()

    # Test c++11 unordered_map feature availability
    set(OLD_CXX_FLAGS ${CMAKE_CXX_FLAGS})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${C++STD_FLAG}")
    if(${NA62USE_USER})
        try_compile(TEST_UN_MAP $ENV{ANALYSISFW_USERDIR}/build-$ENV{SYSTEMINSTALL}/test ${NA62ANALYSIS}/scripts/test_unordered_map.cpp)
    else ()
        try_compile(TEST_UN_MAP ${NA62ANALYSIS}/build-$ENV{SYSTEMINSTALL}/test ${NA62ANALYSIS}/scripts/test_unordered_map.cpp)
    endif()
    if(TEST_UN_MAP)
        message (STATUS "Testing C++11 compatibility -- works")
    else()
        message (STATUS "Testing C++11 compatibility -- failed")
    endif()
    set(CMAKE_CXX_FLAGS ${OLD_CXX_FLAGS})
    
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ROOT_CXX_FLAGS}")
    # Choose warning flags
    if(FULL_WARNING)
        message(STATUS "Using Flag: FULL_WARNING")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pedantic-errors -Wall -Wno-unknown-pragmas -Wextra -Wwrite-strings -Woverloaded-virtual -fno-nonansi-builtins -fno-gnu-keywords -fstrict-aliasing -Wno-long-long")
    else()
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unknown-pragmas -Wno-long-long")
    endif()
    
    # Choose debug flags
    if(NA62_DEBUG)
        message(STATUS "Using Flag: NA62_DEBUG")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g")
    else()
                set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
    endif()
    
    # Choose c++11 flag
    if(TEST_UN_MAP)
        message(STATUS "Using Flag: NA62_C11=1")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNA62_C11=1")
    endif()

    if(OLD_SPECIALTRIGGER)
        message(STATUS "Using Flag: OLD_SPECIALTRIGGER")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DOLD_SPECIALTRIGGER=1")
    endif()

	message(STATUS "CXX Flags: ${CMAKE_CXX_FLAGS}")
endif()

# NA62Tools and NA62Reconstruction include directories
include_directories(${NA62TOOLS_INCLUDE_DIRS})
include_directories(${NA62TOOLS_PERS_INCLUDE_DIRS})
include_directories(${NA62TOOLS_SLIM_INCLUDE_DIRS})
include_directories(${NA62RECO_INCLUDE_DIRS})

add_custom_target(NA62Core)
add_custom_target(NA62Tools)
add_custom_target(NA62Analyzers)
add_dependencies(NA62Analyzers NA62Core NA62Tools)

#FW include directories
include_directories(${NA62ANALYSIS}/include)
include_directories(${NA62ANALYSIS}/ToolsLib/include)
include_directories(${NA62ANALYSIS}/Algorithms/include)
include_directories(${NA62ANALYSIS}/Examples/include)

SUBDIRLIST(${NA62ANALYSIS}/Analyzers NA62ANALYSIS_ANALYZER_DIRS)
SET(NA62ANALYSIS_ANALYZER_INCLUDES "")
FOREACH(dir ${NA62ANALYSIS_ANALYZER_DIRS})
    SET(NA62ANALYSIS_ANALYZER_INCLUDES ${NA62ANALYSIS_ANALYZER_INCLUDES} ${dir}/include)
ENDFOREACH(dir)

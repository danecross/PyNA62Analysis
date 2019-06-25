file(GLOB add_sources src/*.[cf])

set(CMAKE_C_FLAGS "-O2 -fPIC")
set(CMAKE_Fortran_FLAGS "-cpp -g -O2 -fPIC -fno-automatic -fno-backslash -fno-second-underscore -c")

remove_definitions(-Woverloaded-virtual -std=c++11 -fsigned-char -Wno-non-virtual-dtor -Woverloaded-virtual)

#Include directories 
#For SubDirectory
include_directories(include)
#Needs to be specified on a subdirectory basis because of clash with 
#files with the same name in RecoBase/include and SubDetector/include
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${CMAKE_SOURCE_DIR}/RecoBase/include)

#Get all the sources
file(GLOB sources src/*.cc)

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(LKrObjLib OBJECT ${sources} ${add_sources})
set_property(TARGET ${LKrObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(LKr SHARED $<TARGET_OBJECTS:LKrObjLib>)
add_library(LKr-static STATIC $<TARGET_OBJECTS:LKrObjLib>)

#Install libraries and headers
install(TARGETS LKr LKr-static EXPORT NA62RecoTargets DESTINATION lib-$ENV{SYSTEMINSTALL})

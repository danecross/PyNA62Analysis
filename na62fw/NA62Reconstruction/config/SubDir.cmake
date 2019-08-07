#Include directories 
#For SubDirectory
include_directories(include)
#Needs to be specified on a subdirectory basis because of clash with 
#files with the same name in RecoBase/include and SubDetector/include
include_directories(${CMAKE_SOURCE_DIR}/include)
include_directories(${CMAKE_SOURCE_DIR}/RecoBase/include)

#Get all the sources
file(GLOB sources src/*.cc)

#Find out in which directory we are
get_filename_component(SubDet ${CMAKE_CURRENT_SOURCE_DIR} NAME)

if(NOT DEFINED add_sources)
    set(add_sources "")
endif()

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(${SubDet}ObjLib OBJECT ${sources} ${add_sources})
set_property(TARGET ${${SubDet}ObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(Reco${SubDet} SHARED $<TARGET_OBJECTS:${SubDet}ObjLib>)
add_library(Reco${SubDet}-static STATIC $<TARGET_OBJECTS:${SubDet}ObjLib>)

#Install libraries and headers
install(TARGETS Reco${SubDet} Reco${SubDet}-static EXPORT NA62RecoTargets DESTINATION lib-$ENV{SYSTEMINSTALL})

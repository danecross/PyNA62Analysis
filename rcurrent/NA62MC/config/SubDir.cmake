#Include directories 
#For SubDirectory
include_directories(include)
#For MCBase, NA62MC and NA62Persistency
include_directories(${CMAKE_SOURCE_DIR}/MCBase/include)
include_directories(${CMAKE_SOURCE_DIR}/include)

#Get all the sources
file(GLOB sources src/*.cc)

#Find out in which directory we are
get_filename_component(SubDet ${CMAKE_CURRENT_SOURCE_DIR} NAME)

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(${SubDet}ObjLib OBJECT ${sources})
set_property(TARGET ${${SubDet}ObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(${SubDet} SHARED $<TARGET_OBJECTS:${SubDet}ObjLib>)
add_library(${SubDet}-static STATIC $<TARGET_OBJECTS:${SubDet}ObjLib>)


#Install libraries and headers
install(TARGETS ${SubDet} ${SubDet}-static DESTINATION lib-$ENV{SYSTEMINSTALL})

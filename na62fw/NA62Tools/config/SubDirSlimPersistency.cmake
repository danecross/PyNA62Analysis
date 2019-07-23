#Include directories 
#For SubDirectory
include_directories(include)

#Get all the sources
file(GLOB sources src/*.cc)
file(GLOB headers include/*.hh)

#Find out in which directory we are
get_filename_component(SubDet ${CMAKE_CURRENT_SOURCE_DIR} NAME)

include_directories(../../FullReco/${SubDet}/include)

#Generate ROOT dictionary
ROOT_GENERATE_DICTIONARY(${SubDet}SlimPersistencyDICT ${headers} LINKDEF ${SubDet}SlimPersistencyLinkDef.hh MODULE ${SubDet}SlimPersistency OPTIONS -inlineInputHeader)# -noIncludePaths)

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(${SubDet}ObjSlimPersistencyLib OBJECT ${sources} ${SubDet}SlimPersistencyDICT.cxx)
set_property(TARGET ${${SubDet}ObjSlimPersistencyLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(${SubDet}SlimPersistency SHARED $<TARGET_OBJECTS:${SubDet}ObjSlimPersistencyLib>)
add_library(${SubDet}SlimPersistency-static STATIC $<TARGET_OBJECTS:${SubDet}ObjSlimPersistencyLib>)

add_dependencies(slimpersistency ${SubDet}SlimPersistency ${SubDet}SlimPersistency-static)

#Install libraries and headers
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${SubDet}SlimPersistency_rdict.pcm DESTINATION lib-$ENV{SYSTEMINSTALL}/SlimPersistency COMPONENT slimlib)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${SubDet}SlimPersistency.rootmap DESTINATION lib-$ENV{SYSTEMINSTALL}/SlimPersistency COMPONENT slimlib)
install(TARGETS ${SubDet}SlimPersistency ${SubDet}SlimPersistency-static EXPORT NA62SlimPersistencyTargets DESTINATION lib-$ENV{SYSTEMINSTALL}/SlimPersistency COMPONENT slimlib)

SET(SLIM_PERSISTENCY_INCLUDE_DIR ${SLIM_PERSISTENCY_INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/include PARENT_SCOPE)
SET(SLIM_PERSISTENCY_LIBRARIES ${SLIM_PERSISTENCY_LIBRARIES} ${SubDet}SlimPersistency PARENT_SCOPE)
SET(SLIM_PERSISTENCY_LIBRARIES_STATIC ${SLIM_PERSISTENCY_LIBRARIES_STATIC} ${SubDet}SlimPersistency-static PARENT_SCOPE)

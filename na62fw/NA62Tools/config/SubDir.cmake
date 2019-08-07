#Include directories 
#For SubDirectory
include_directories(include)

#Get all the sources
file(GLOB sources src/*.cc)
file(GLOB headers include/*.hh)

if(${ROOT_VERSION} STREQUAL "6.18.00" OR ${ROOT_VERSION} VERSION_GREATER "6.18.00")
        # properly handle headers for ROOT versions >= 6.18
        KEEPFILENAMES_LIST(headers)
endif()

#Find out in which directory we are
get_filename_component(SubDet ${CMAKE_CURRENT_SOURCE_DIR} NAME)

#Generate ROOT dictionary
ROOT_GENERATE_DICTIONARY(${SubDet}PersistencyDICT ${headers} OPTIONS -inlineInputHeader -Iinclude LINKDEF ${SubDet}PersistencyLinkDef.hh MODULE ${SubDet}Persistency)# -noIncludePaths)

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(${SubDet}ObjPersistencyLib OBJECT ${sources} ${SubDet}PersistencyDICT.cxx)
set_property(TARGET ${${SubDet}ObjPersistencyLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(${SubDet}Persistency SHARED $<TARGET_OBJECTS:${SubDet}ObjPersistencyLib>)
add_library(${SubDet}Persistency-static STATIC $<TARGET_OBJECTS:${SubDet}ObjPersistencyLib>)  

add_dependencies(persistency ${SubDet}Persistency ${SubDet}Persistency-static)

#Install libraries and headers
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${SubDet}Persistency_rdict.pcm DESTINATION lib-$ENV{SYSTEMINSTALL}/Persistency COMPONENT persistencylib)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/lib${SubDet}Persistency.rootmap DESTINATION lib-$ENV{SYSTEMINSTALL}/Persistency COMPONENT persistencylib)
install(TARGETS ${SubDet}Persistency ${SubDet}Persistency-static EXPORT NA62PersistencyTargets DESTINATION lib-$ENV{SYSTEMINSTALL}/Persistency COMPONENT persistencylib)

SET(PERSISTENCY_INCLUDE_DIR ${PERSISTENCY_INCLUDE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/include PARENT_SCOPE)
SET(PERSISTENCY_LIBRARIES ${PERSISTENCY_LIBRARIES} ${SubDet}Persistency PARENT_SCOPE)
SET(PERSISTENCY_LIBRARIES_STATIC ${PERSISTENCY_LIBRARIES_STATIC} ${SubDet}Persistency-static PARENT_SCOPE)

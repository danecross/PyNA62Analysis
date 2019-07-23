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

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${SubDet}LinkDef.hh")
    file(READ ${SubDet}LinkDef.hh contents)
    STRING(REGEX REPLACE "\n" "" contents "${contents}")
    set(headers "")
    foreach(line ${contents})
        string(REGEX MATCH "class (.*)\\+" match ${line})
        if(NOT "${match}" STREQUAL "")
            set(headers ${headers} "${CMAKE_CURRENT_SOURCE_DIR}/include/${CMAKE_MATCH_1}.hh")
        endif()
    endforeach()

    #Generate ROOT dictionary
    ROOT_GENERATE_DICTIONARY(${SubDet}DICT ${headers} LINKDEF ${SubDet}LinkDef.hh MODULE ${SubDet} OPTIONS -inlineInputHeader)
    
    set(dict_file "${SubDet}DICT.cxx")
else()
    set(dict_file "")
endif()

if(NOT DEFINED add_sources)
    set(add_sources "")
endif()

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(${SubDet}ObjLib OBJECT ${sources} ${add_sources} ${dict_file})
set_property(TARGET ${${SubDet}ObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(Reco${SubDet} SHARED $<TARGET_OBJECTS:${SubDet}ObjLib>)
add_library(Reco${SubDet}-static STATIC $<TARGET_OBJECTS:${SubDet}ObjLib>)

#Install libraries and headers
install(TARGETS Reco${SubDet} Reco${SubDet}-static EXPORT NA62RecoTargets DESTINATION lib-$ENV{SYSTEMINSTALL})

file(GLOB sources src/*.cc)

#Needs to be specified on a subdirectory basis because of clash with 
#files with the same name in RecoBase/include and SubDetector/include
include_directories(include)
include_directories(${CMAKE_SOURCE_DIR}/include)

foreach(detector ${subdetlist})
	include_directories(${CMAKE_SOURCE_DIR}/${detector}/include)
endforeach()

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(BaseObjLib OBJECT ${sources})
set_property(TARGET ${BaseObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(RecoBase SHARED $<TARGET_OBJECTS:BaseObjLib>)
add_library(RecoBase-static STATIC $<TARGET_OBJECTS:BaseObjLib>)

#Install libraries and headers
install(TARGETS RecoBase RecoBase-static DESTINATION lib-$ENV{SYSTEMINSTALL})
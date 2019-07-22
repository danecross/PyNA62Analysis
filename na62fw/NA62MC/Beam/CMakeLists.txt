set(CMAKE_C_FLAGS "-O2 -fPIC")
set(CMAKE_Fortran_FLAGS "-O2 -fPIC -fno-automatic -fno-backslash -fno-second-underscore -std=legacy")

remove_definitions(-Woverloaded-virtual -std=c++11 -fsigned-char)
file(GLOB sources src/*.[cf]*)

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(BeamObjLib OBJECT ${sources})
set_property(TARGET ${BeamObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)
add_dependencies(BeamObjLib ${LOOPTOOLS_PREFIX})

#Then the real libraries 
add_library(Beam SHARED $<TARGET_OBJECTS:BeamObjLib>)
add_library(Beam-static STATIC $<TARGET_OBJECTS:BeamObjLib>)

#Install libraries
install(TARGETS Beam Beam-static DESTINATION lib-$ENV{SYSTEMINSTALL})

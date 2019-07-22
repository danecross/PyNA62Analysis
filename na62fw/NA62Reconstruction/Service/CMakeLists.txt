file(GLOB sources src/*.cc)

#Create libraries
#First fake object libraries, so sources are not compiled twice (once per target)
add_library(ServiceObjLib OBJECT ${sources})
set_property(TARGET ${ServiceObjLib} PROPERTY POSITION_INDEPENDENT_CODE 1)

#Then the real libraries 
add_library(RecoService SHARED $<TARGET_OBJECTS:ServiceObjLib>)
add_library(RecoService-static STATIC $<TARGET_OBJECTS:ServiceObjLib>)

#Install libraries and headers
install(TARGETS RecoService RecoService-static DESTINATION lib-$ENV{SYSTEMINSTALL})
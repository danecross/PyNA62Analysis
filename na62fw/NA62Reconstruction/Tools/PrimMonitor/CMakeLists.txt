file(GLOB sources src/*.cc)

include_directories("include")
add_executable(PrimMonitor ${sources} )

target_link_libraries(PrimMonitor ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve RGL FTGL Gui Ged TreePlayer Spectrum)
target_link_libraries(PrimMonitor NA62Persistency${EXTLIBTYPEPOSTFIX})

#Get and configure DIM
if(DEFINED ENV{DIMDEFINED})
    if($ENV{DIMDEFINED} EQUAL 1)
        target_link_libraries(PrimMonitor dim)
    endif()
endif()

add_dependencies(tools PrimMonitor)

#Install executable
install(TARGETS PrimMonitor DESTINATION bin-$ENV{SYSTEMINSTALL} COMPONENT toolsexe)

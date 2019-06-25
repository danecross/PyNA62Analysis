file(GLOB sources src/*.cc)

set (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fopenmp")
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fopenmp")

include_directories("../include")
add_executable(PrimCorrelation ${sources} )

target_link_libraries(PrimCorrelation ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve RGL FTGL Gui Ged TreePlayer Spectrum)
target_link_libraries(PrimCorrelation NA62Persistency${EXTLIBTYPEPOSTFIX})

add_dependencies(tools PrimCorrelation)

#Install executable
install(TARGETS PrimCorrelation DESTINATION bin-$ENV{SYSTEMINSTALL} COMPONENT toolsexe)
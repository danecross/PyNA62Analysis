file(GLOB sources src/*.cc)

add_executable(PrimRawDecoder ${sources} )

target_link_libraries(PrimRawDecoder ${ROOT_LIBRARIES} Minuit Geom TMVA EG Eve RGL FTGL Gui Ged TreePlayer Spectrum)
target_link_libraries(PrimRawDecoder NA62Persistency${EXTLIBTYPEPOSTFIX})
target_link_libraries(PrimRawDecoder XrdPosixPreload XrdPosix)

add_dependencies(tools PrimRawDecoder)

#Install executable
install(TARGETS PrimRawDecoder DESTINATION bin-$ENV{SYSTEMINSTALL} COMPONENT toolsexe)
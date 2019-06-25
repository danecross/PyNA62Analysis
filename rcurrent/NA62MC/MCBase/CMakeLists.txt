file(GLOB sources src/*.cc)
add_library(MCBase SHARED ${sources})
add_library(MCBase-static STATIC ${sources})

install(TARGETS MCBase MCBase-static DESTINATION lib-$ENV{SYSTEMINSTALL})
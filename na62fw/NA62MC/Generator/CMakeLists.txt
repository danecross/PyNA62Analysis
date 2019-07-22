file(GLOB sources_C *.cc)
file(GLOB sources_F *.[F])

set (CMAKE_Fortran_FLAGS "-O2 -fPIC -fno-automatic -fno-backslash -fno-second-underscore")
set (CMAKE_C_FLAGS "-O2 -fPIC")

remove_definitions(-Woverloaded-virtual -std=c++11 -fsigned-char)

add_library(CMC SHARED ${sources_C} ${sources_F} ${CMAKE_SOURCE_DIR}/src/cmc_common_address.c)
add_library(CMC-static STATIC ${sources_C} ${sources_F} ${CMAKE_SOURCE_DIR}/src/cmc_common_address.c) 
install(TARGETS CMC CMC-static DESTINATION lib-$ENV{SYSTEMINSTALL})
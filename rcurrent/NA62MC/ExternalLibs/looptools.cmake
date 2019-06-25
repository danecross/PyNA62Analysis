set(LOOPTOOLS_URL ${CMAKE_CURRENT_SOURCE_DIR}/ExternalLibs/LoopTools-2.14.tar.gz)
set(LOOPTOOLS_URL_MD5 b684bea48b4aa6a2584c6708f4f3e949)
set(LOOPTOOLS_PREFIX LoopTools214)

ExternalProject_Add(${LOOPTOOLS_PREFIX}
	PREFIX ${LOOPTOOLS_PREFIX}
	URL ${LOOPTOOLS_URL}
	URL_MD5 ${LOOPTOOLS_URL_MD5}
	CONFIGURE_COMMAND ./configure --generic
	BUILD_COMMAND  make lib
	BUILD_IN_SOURCE 1
	INSTALL_COMMAND ""
)

ExternalProject_Get_Property(${LOOPTOOLS_PREFIX} SOURCE_DIR)
message(STATUS "Source directory of ${LOOPTOOLS_PREFIX} ${SOURCE_DIR}")

add_library(ooptools STATIC IMPORTED)
include_directories(${SOURCE_DIR}/build)
set_target_properties( ooptools PROPERTIES IMPORTED_LOCATION ${SOURCE_DIR}/build/libooptools.a )
install(FILES ${SOURCE_DIR}/build/libooptools.a DESTINATION lib-$ENV{SYSTEMINSTALL})

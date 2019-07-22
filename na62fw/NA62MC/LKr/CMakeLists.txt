include(${CMAKE_SOURCE_DIR}/config/SubDir.cmake)
include_directories(${CMAKE_SOURCE_DIR}/MCBase/include/lzma)
target_link_libraries(MCLKr lzma)

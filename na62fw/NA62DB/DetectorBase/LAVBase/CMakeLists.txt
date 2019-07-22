#Include the general cmake file
include(${PROJECT_SOURCE_DIR}/scripts/CMakeList.cmake)

include_directories(include)

#Add Shared in between to create a shared library (.so), default is archive (.a)
add_library(LAVDBBase src/LAVDetectorSetup.cpp src/LAVDetectorStorage.cpp)

#Install the  libraries in the install path (again not strictly required if you want to keep them in the build directory)
install(TARGETS LAVDBBase DESTINATION ${PROJECT_SOURCE_DIR}/lib)

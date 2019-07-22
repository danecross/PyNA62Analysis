#Include the general cmake file
include(${PROJECT_SOURCE_DIR}/scripts/CMakeList.cmake)

include_directories(include)

#Create libraries: library name (DBBase -> libDBBase, lib prefix is added automatically) followed by list of source files

#Add Shared in between to create a shared library (.so), default is archive (.a)
add_library(DBBase src/base64.cpp src/BaseCache.cpp  src/BaseDetectorStorage.cpp src/BaseSubSystemStorage.cpp  src/BaseSubSystemTypeManager.cpp)

#Install the  libraries in the install path (again not strictly required if you want to keep them in the build directory)
install(TARGETS DBBase DESTINATION ${PROJECT_SOURCE_DIR}/lib)

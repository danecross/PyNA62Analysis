#Include the general cmake file
include(${PROJECT_SOURCE_DIR}/scripts/CMakeList.cmake)

#Add these directories in the include path (-I in makefile)
include_directories(include)
include_directories(${PROJECT_SOURCE_DIR}/DetectorBase/LAVBase/include)

add_executable(LAV_initialize src/LAV_initialize.cpp)
add_executable(LAV_setup src/LAV_setup.cpp)
add_executable(LAV_test src/LAV_test.cpp)

target_link_libraries(LAV_initialize DBBase LAVDBBase)
target_link_libraries(LAV_initialize boost_thread-gcc46-mt-1_53 boost_wave-gcc46-mt-1_53 boost_filesystem-gcc46-mt-1_53 boost_date_time-gcc46-mt-1_53 boost_system-gcc46-mt-1_53 boost_wserialization-gcc46-mt-1_53 boost_serialization-gcc46-mt-1_53) #External
target_link_libraries(LAV_initialize lcg_CoralBase lcg_CoralAccess lcg_CoralCommon lcg_CoralKernel lcg_CoralSockets lcg_CoralStubs lcg_CoralServerBase lcg_RelationalAccess lcg_CoralMonitor) #External

target_link_libraries(LAV_setup DBBase LAVDBBase)
target_link_libraries(LAV_setup boost_thread-gcc46-mt-1_53 boost_wave-gcc46-mt-1_53 boost_filesystem-gcc46-mt-1_53 boost_date_time-gcc46-mt-1_53 boost_system-gcc46-mt-1_53 boost_wserialization-gcc46-mt-1_53 boost_serialization-gcc46-mt-1_53) #External
target_link_libraries(LAV_setup lcg_CoralBase lcg_CoralAccess lcg_CoralCommon lcg_CoralKernel lcg_CoralSockets lcg_CoralStubs lcg_CoralServerBase lcg_RelationalAccess lcg_CoralMonitor) #External

target_link_libraries(LAV_test DBBase LAVDBBase)
target_link_libraries(LAV_test boost_thread-gcc46-mt-1_53 boost_wave-gcc46-mt-1_53 boost_filesystem-gcc46-mt-1_53 boost_date_time-gcc46-mt-1_53 boost_system-gcc46-mt-1_53 boost_wserialization-gcc46-mt-1_53 boost_serialization-gcc46-mt-1_53) #External
target_link_libraries(LAV_test lcg_CoralBase lcg_CoralAccess lcg_CoralCommon lcg_CoralKernel lcg_CoralSockets lcg_CoralStubs lcg_CoralServerBase lcg_RelationalAccess lcg_CoralMonitor) #External

#Install the executables in the install path 
install(TARGETS LAV_initialize LAV_setup LAV_test DESTINATION ${PROJECT_SOURCE_DIR}/bin)

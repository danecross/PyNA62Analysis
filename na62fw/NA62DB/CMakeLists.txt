#require minimum version of cmake (syntax)
cmake_minimum_required (VERSION 2.6 FATAL_ERROR)

#Define project name
project (NA62DB)

add_subdirectory(DBBase)
add_subdirectory(DetectorBase)
add_subdirectory(DetectorAction)

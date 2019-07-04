# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

cmake_minimum_required(VERSION 3.5)

if("/afs/cern.ch/user/d/dacross/na62fw/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz" STREQUAL "")
  message(FATAL_ERROR "LOCAL can't be empty")
endif()

if(NOT EXISTS "/afs/cern.ch/user/d/dacross/na62fw/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz")
  message(FATAL_ERROR "File not found: /afs/cern.ch/user/d/dacross/na62fw/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz")
endif()

if("MD5" STREQUAL "")
  message(WARNING "File will not be verified since no URL_HASH specified")
  return()
endif()

if("b684bea48b4aa6a2584c6708f4f3e949" STREQUAL "")
  message(FATAL_ERROR "EXPECT_VALUE can't be empty")
endif()

message(STATUS "verifying file...
     file='/afs/cern.ch/user/d/dacross/na62fw/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz'")

file("MD5" "/afs/cern.ch/user/d/dacross/na62fw/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz" actual_value)

if(NOT "${actual_value}" STREQUAL "b684bea48b4aa6a2584c6708f4f3e949")
  message(FATAL_ERROR "error: MD5 hash of
  /afs/cern.ch/user/d/dacross/na62fw/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz
does not match expected value
  expected: 'b684bea48b4aa6a2584c6708f4f3e949'
    actual: '${actual_value}'
")
endif()

message(STATUS "verifying file... done")

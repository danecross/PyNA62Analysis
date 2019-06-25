set(file "/afs/cern.ch/na62/offline/NA62FW/dev/rcurrent/NA62MC/ExternalLibs/LoopTools-2.14.tar.gz")
message(STATUS "verifying file...
     file='${file}'")
set(expect_value "b684bea48b4aa6a2584c6708f4f3e949")
set(attempt 0)
set(succeeded 0)
while(${attempt} LESS 0 OR ${attempt} EQUAL 0 AND NOT ${succeeded})
  file(MD5 "${file}" actual_value)
  if("${actual_value}" STREQUAL "${expect_value}")
    set(succeeded 1)
  elseif(${attempt} LESS 0)
    message(STATUS "MD5 hash of ${file}
does not match expected value
  expected: ${expect_value}
    actual: ${actual_value}
Retrying download.
")
    file(REMOVE "${file}")
    execute_process(COMMAND ${CMAKE_COMMAND} -P "")
  endif()
  math(EXPR attempt "${attempt} + 1")
endwhile()

if(${succeeded})
  message(STATUS "verifying file... done")
else()
  message(FATAL_ERROR "error: MD5 hash of
  ${file}
does not match expected value
  expected: ${expect_value}
    actual: ${actual_value}
")
endif()
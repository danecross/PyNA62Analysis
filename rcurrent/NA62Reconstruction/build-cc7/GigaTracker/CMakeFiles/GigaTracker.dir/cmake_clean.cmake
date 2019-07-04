file(REMOVE_RECURSE
  "libGigaTracker.pdb"
  "libGigaTracker.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/GigaTracker.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

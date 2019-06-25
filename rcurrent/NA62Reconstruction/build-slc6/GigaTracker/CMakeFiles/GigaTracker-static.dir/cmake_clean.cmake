file(REMOVE_RECURSE
  "libGigaTracker-static.pdb"
  "libGigaTracker-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/GigaTracker-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

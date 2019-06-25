file(REMOVE_RECURSE
  "libLAV.pdb"
  "libLAV.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/LAV.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

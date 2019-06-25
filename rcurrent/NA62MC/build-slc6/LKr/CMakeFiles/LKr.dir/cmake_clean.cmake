file(REMOVE_RECURSE
  "libLKr.pdb"
  "libLKr.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/LKr.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

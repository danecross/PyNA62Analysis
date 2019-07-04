file(REMOVE_RECURSE
  "libLKr-static.pdb"
  "libLKr-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/LKr-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

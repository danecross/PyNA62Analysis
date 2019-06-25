file(REMOVE_RECURSE
  "libLAV-static.pdb"
  "libLAV-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/LAV-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

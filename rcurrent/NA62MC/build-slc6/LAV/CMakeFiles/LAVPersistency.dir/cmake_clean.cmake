file(REMOVE_RECURSE
  "libLAVPersistency.pdb"
  "libLAVPersistency.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/LAVPersistency.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

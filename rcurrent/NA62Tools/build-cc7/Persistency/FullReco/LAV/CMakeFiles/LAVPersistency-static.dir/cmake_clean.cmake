file(REMOVE_RECURSE
  "libLAVPersistency-static.pdb"
  "libLAVPersistency-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/LAVPersistency-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

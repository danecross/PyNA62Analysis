file(REMOVE_RECURSE
  "libSAVPersistency-static.pdb"
  "libSAVPersistency-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/SAVPersistency-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

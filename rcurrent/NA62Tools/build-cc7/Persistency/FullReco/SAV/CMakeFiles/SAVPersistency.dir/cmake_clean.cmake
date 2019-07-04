file(REMOVE_RECURSE
  "libSAVPersistency.pdb"
  "libSAVPersistency.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/SAVPersistency.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

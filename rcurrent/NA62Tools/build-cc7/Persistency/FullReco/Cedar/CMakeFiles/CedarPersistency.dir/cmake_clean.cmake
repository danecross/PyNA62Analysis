file(REMOVE_RECURSE
  "libCedarPersistency.pdb"
  "libCedarPersistency.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/CedarPersistency.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

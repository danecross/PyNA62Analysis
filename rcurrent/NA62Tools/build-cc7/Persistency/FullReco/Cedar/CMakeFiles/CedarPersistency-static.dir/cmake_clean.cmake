file(REMOVE_RECURSE
  "libCedarPersistency-static.pdb"
  "libCedarPersistency-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/CedarPersistency-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

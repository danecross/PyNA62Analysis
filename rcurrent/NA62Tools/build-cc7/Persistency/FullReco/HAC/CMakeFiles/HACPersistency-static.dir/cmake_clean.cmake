file(REMOVE_RECURSE
  "libHACPersistency-static.pdb"
  "libHACPersistency-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/HACPersistency-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

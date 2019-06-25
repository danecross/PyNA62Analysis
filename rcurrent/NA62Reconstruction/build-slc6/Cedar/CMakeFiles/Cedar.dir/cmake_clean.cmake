file(REMOVE_RECURSE
  "libCedar.pdb"
  "libCedar.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/Cedar.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

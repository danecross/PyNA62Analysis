file(REMOVE_RECURSE
  "libCedar-static.pdb"
  "libCedar-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/Cedar-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

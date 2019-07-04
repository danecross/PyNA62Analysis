file(REMOVE_RECURSE
  "libNewCHOD.pdb"
  "libNewCHOD.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/NewCHOD.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

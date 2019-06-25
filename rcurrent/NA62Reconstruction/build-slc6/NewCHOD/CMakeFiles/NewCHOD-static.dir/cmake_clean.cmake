file(REMOVE_RECURSE
  "libNewCHOD-static.pdb"
  "libNewCHOD-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/NewCHOD-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

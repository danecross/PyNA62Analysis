file(REMOVE_RECURSE
  "libSAC-static.pdb"
  "libSAC-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/SAC-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

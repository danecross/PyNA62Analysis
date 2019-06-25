file(REMOVE_RECURSE
  "libSAC.pdb"
  "libSAC.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/SAC.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

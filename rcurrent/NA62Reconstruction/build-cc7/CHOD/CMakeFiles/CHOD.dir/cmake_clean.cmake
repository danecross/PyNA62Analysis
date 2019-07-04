file(REMOVE_RECURSE
  "libCHOD.pdb"
  "libCHOD.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/CHOD.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

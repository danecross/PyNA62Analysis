file(REMOVE_RECURSE
  "libCHOD-static.pdb"
  "libCHOD-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/CHOD-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

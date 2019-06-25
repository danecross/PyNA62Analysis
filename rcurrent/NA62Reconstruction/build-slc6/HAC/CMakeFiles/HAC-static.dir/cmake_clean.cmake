file(REMOVE_RECURSE
  "libHAC-static.pdb"
  "libHAC-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/HAC-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

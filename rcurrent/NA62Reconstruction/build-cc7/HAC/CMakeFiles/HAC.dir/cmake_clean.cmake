file(REMOVE_RECURSE
  "libHAC.pdb"
  "libHAC.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/HAC.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

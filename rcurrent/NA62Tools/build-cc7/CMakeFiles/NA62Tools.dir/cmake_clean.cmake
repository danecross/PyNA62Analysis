file(REMOVE_RECURSE
  "libNA62Tools.pdb"
  "libNA62Tools.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/NA62Tools.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

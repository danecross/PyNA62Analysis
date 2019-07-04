file(REMOVE_RECURSE
  "libNA62Tools-static.pdb"
  "libNA62Tools-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/NA62Tools-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

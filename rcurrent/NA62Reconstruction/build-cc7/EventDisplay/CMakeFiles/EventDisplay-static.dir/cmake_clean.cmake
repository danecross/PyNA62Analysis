file(REMOVE_RECURSE
  "libEventDisplay-static.pdb"
  "libEventDisplay-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/EventDisplay-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

file(REMOVE_RECURSE
  "libIRC-static.pdb"
  "libIRC-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/IRC-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

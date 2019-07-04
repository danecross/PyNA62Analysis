file(REMOVE_RECURSE
  "libRICH-static.pdb"
  "libRICH-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RICH-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

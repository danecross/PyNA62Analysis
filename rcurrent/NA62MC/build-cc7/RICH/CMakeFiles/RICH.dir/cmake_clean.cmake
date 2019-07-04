file(REMOVE_RECURSE
  "libRICH.pdb"
  "libRICH.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/RICH.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

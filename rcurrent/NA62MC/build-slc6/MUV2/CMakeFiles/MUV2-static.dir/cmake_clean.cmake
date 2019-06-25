file(REMOVE_RECURSE
  "libMUV2-static.pdb"
  "libMUV2-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/MUV2-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

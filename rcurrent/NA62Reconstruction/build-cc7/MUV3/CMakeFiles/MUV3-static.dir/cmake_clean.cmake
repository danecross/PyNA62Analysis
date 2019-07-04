file(REMOVE_RECURSE
  "libMUV3-static.pdb"
  "libMUV3-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/MUV3-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

file(REMOVE_RECURSE
  "libMUV1-static.pdb"
  "libMUV1-static.a"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/MUV1-static.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

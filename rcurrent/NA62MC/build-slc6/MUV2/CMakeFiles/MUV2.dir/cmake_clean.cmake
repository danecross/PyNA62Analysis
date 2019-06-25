file(REMOVE_RECURSE
  "libMUV2.pdb"
  "libMUV2.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/MUV2.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()

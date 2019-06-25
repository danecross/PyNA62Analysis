file(REMOVE_RECURSE
  "libMUV3.pdb"
  "libMUV3.so"
)

# Per-language clean rules from dependency scanning.
foreach(lang CXX)
  include(CMakeFiles/MUV3.dir/cmake_clean_${lang}.cmake OPTIONAL)
endforeach()
